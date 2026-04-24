//===--- UnusedIntentCheck.cpp - flang-tidy -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "UnusedIntentCheck.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/attr.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "utils/FixIt.h"
#include "utils/SourceEditUtils.h"
#include <algorithm>
#include <cctype>
#include <cstddef>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

static std::string getIntentSpecSpelling(const std::string &lineText,
                                         bool IsInOut) {
  const std::string keyword = utils::inferKeywordSpelling(lineText, "INTENT");
  const std::string arg = keyword == "intent" ? (IsInOut ? "inout" : "in")
                                              : (IsInOut ? "INOUT" : "IN");
  return keyword + "(" + arg + ")";
}

static std::optional<parser::CharBlock>
findIntentInoutRange(const utils::SourceLineInfo &line);

// Split a mixed-intent declaration line by moving `targetName` onto a new
// line with intent(in), leaving the other entities on the original line with
// intent(inout) unchanged.
static std::optional<std::string>
buildMixedIntentReplacement(const utils::SourceLineInfo &line,
                            llvm::StringRef targetName,
                            llvm::StringRef explicitIndentation) {
  auto parsed = utils::parseDeclarationLine(line.lineText);
  if (!parsed || parsed->entities.size() < 2)
    return std::nullopt;

  auto inoutRange = findIntentInoutRange(line);
  if (!inoutRange)
    return std::nullopt;

  const std::ptrdiff_t inoutBeginOffset =
      inoutRange->begin() - line.lineBegin;
  const std::ptrdiff_t inoutEndOffset = inoutRange->end() - line.lineBegin;
  if (inoutBeginOffset < 0 || inoutEndOffset < inoutBeginOffset ||
      static_cast<std::size_t>(inoutEndOffset) > parsed->prefix.size())
    return std::nullopt;

  // Build the prefix for the new line (intent(in) for the extracted entity).
  std::string prefixWithIntentIn = parsed->prefix;
  prefixWithIntentIn.replace(
      static_cast<std::size_t>(inoutBeginOffset),
      static_cast<std::size_t>(inoutEndOffset - inoutBeginOffset),
      getIntentSpecSpelling(line.lineText, false));

  const std::string targetLower = targetName.lower();
  std::vector<std::string> keepDecls;
  std::string targetDecl;
  bool foundTarget{false};
  for (const utils::EntityDecl &entity : parsed->entities) {
    if (llvm::StringRef(entity.baseName).lower() == targetLower) {
      foundTarget = true;
      targetDecl = entity.declarator;
    } else {
      keepDecls.push_back(entity.declarator);
    }
  }
  if (!foundTarget || keepDecls.empty() || targetDecl.empty())
    return std::nullopt;

  std::string firstLine = parsed->prefix;
  for (std::size_t i{0}; i < keepDecls.size(); ++i) {
    if (i > 0)
      firstLine += ", ";
    firstLine += keepDecls[i];
  }
  if (!parsed->commentPart.empty()) {
    if (!firstLine.empty() &&
        !std::isspace(static_cast<unsigned char>(firstLine.back())))
      firstLine += " ";
    firstLine += parsed->commentPart;
  }

  std::string indent = explicitIndentation.str();
  if (indent.empty())
    indent = utils::getLeadingWhitespace(firstLine);

  return firstLine + "\n" + indent +
         llvm::StringRef{prefixWithIntentIn}.ltrim(" \t").str() + targetDecl;
}

// Build a new line to insert after the current one, containing only
// `targetName` with the given intent attribute added.
static std::optional<std::string>
buildMissingIntentInsertionForMixedDecl(const utils::SourceLineInfo &line,
                                        llvm::StringRef targetName,
                                        llvm::StringRef intentSpec,
                                        llvm::StringRef explicitIndentation) {
  auto parsed = utils::parseDeclarationLine(line.lineText);
  if (!parsed || parsed->entities.size() < 2)
    return std::nullopt;

  const std::string targetLower = targetName.lower();
  std::string targetDecl;
  bool foundTarget{false};
  for (const utils::EntityDecl &entity : parsed->entities) {
    if (llvm::StringRef(entity.baseName).lower() == targetLower) {
      foundTarget = true;
      targetDecl = entity.declarator;
      break;
    }
  }
  if (!foundTarget || targetDecl.empty())
    return std::nullopt;

  auto insertPos =
      utils::findDeclAttrInsertionPoint(line.lineText, line.lineBegin);
  if (!insertPos)
    return std::nullopt;

  const std::ptrdiff_t insertOffset = *insertPos - line.lineBegin;
  std::string prefixWithAttrs = parsed->prefix;
  if (insertOffset < 0 ||
      static_cast<std::size_t>(insertOffset) > prefixWithAttrs.size())
    return std::nullopt;
  prefixWithAttrs.insert(static_cast<std::size_t>(insertOffset),
                         ", " + intentSpec.str());

  std::string indent = explicitIndentation.str();
  if (indent.empty())
    indent = utils::getLeadingWhitespace(prefixWithAttrs);

  return "\n" + indent +
         llvm::StringRef{prefixWithAttrs}.ltrim(" \t").str() + targetDecl;
}

// Look up a symbol by name, case-insensitively, within a single scope level.
static const semantics::Symbol *
findSymbolInScopeByNameCaseInsensitive(const semantics::Scope &scope,
                                       llvm::StringRef name) {
  const std::string nameLower = name.lower();
  for (const auto &pair : scope) {
    if (pair.second->name().ToString() == nameLower)
      return &*pair.second;
  }
  return nullptr;
}

// For each entity on a mixed declaration line that lacks an explicit intent,
// split it onto its own line with the appropriate intent attribute inserted.
static std::optional<std::string>
buildMissingIntentReplacementForMixedDecl(
    const utils::SourceLineInfo &line, const semantics::Scope &scope,
    llvm::StringRef explicitIndentation,
    llvm::function_ref<bool(const semantics::Symbol &)> wasDefined) {
  auto parsed = utils::parseDeclarationLine(line.lineText);
  if (!parsed || parsed->entities.size() < 2)
    return std::nullopt;

  auto insertPos =
      utils::findDeclAttrInsertionPoint(line.lineText, line.lineBegin);
  if (!insertPos)
    return std::nullopt;

  const std::ptrdiff_t insertOffset = *insertPos - line.lineBegin;
  if (insertOffset < 0 ||
      static_cast<std::size_t>(insertOffset) > parsed->prefix.size())
    return std::nullopt;

  std::string indent = explicitIndentation.str();
  if (indent.empty())
    indent = utils::getLeadingWhitespace(line.lineText);
  const bool lineHasExplicitIndent =
      indent.empty() || llvm::StringRef{line.lineText}.starts_with(indent);

  std::vector<std::string> keepDecls;
  std::vector<std::string> splitLineBodies;
  bool changed{false};

  for (const utils::EntityDecl &entity : parsed->entities) {
    const semantics::Symbol *symbol =
        findSymbolInScopeByNameCaseInsensitive(scope, entity.baseName);
    if (!symbol) {
      keepDecls.push_back(entity.declarator);
      continue;
    }
    const auto *details = symbol->detailsIf<semantics::ObjectEntityDetails>();
    if (!details || !details->isDummy() ||
        symbol->attrs().HasAny(
            {semantics::Attr::INTENT_IN, semantics::Attr::INTENT_INOUT,
             semantics::Attr::INTENT_OUT, semantics::Attr::VALUE}) ||
        symbol->attrs().test(semantics::Attr::TARGET)) {
      keepDecls.push_back(entity.declarator);
      continue;
    }

    changed = true;
    const std::string intentSpec =
        getIntentSpecSpelling(line.lineText, wasDefined(*symbol));
    std::string prefixWithAttrs = parsed->prefix;
    prefixWithAttrs.insert(static_cast<std::size_t>(insertOffset),
                           ", " + intentSpec);
    splitLineBodies.push_back(
        llvm::StringRef{prefixWithAttrs}.ltrim(" \t").str() +
        entity.declarator);
  }
  if (!changed || splitLineBodies.empty())
    return std::nullopt;

  std::string replacement;
  if (!keepDecls.empty()) {
    std::string firstLine = parsed->prefix;
    for (std::size_t i{0}; i < keepDecls.size(); ++i) {
      if (i > 0)
        firstLine += ", ";
      firstLine += keepDecls[i];
    }
    if (!parsed->commentPart.empty()) {
      if (!firstLine.empty() &&
          !std::isspace(static_cast<unsigned char>(firstLine.back())))
        firstLine += " ";
      firstLine += parsed->commentPart;
    }
    replacement = firstLine;
  }

  for (std::size_t i{0}; i < splitLineBodies.size(); ++i) {
    if (!replacement.empty() || i > 0)
      replacement += "\n";
    const bool skipIndentOnFirst =
        i == 0 && keepDecls.empty() && !lineHasExplicitIndent;
    if (!skipIndentOnFirst)
      replacement += indent;
    replacement += splitLineBodies[i];
  }
  if (keepDecls.empty() && !parsed->commentPart.empty())
    replacement += " " + parsed->commentPart;

  return replacement;
}

static std::optional<parser::CharBlock>
findIntentInoutRange(const utils::SourceLineInfo &line) {
  const std::string lowerLine = llvm::StringRef{line.lineText}.lower();
  const std::size_t searchLimit = line.lineText.find("::");
  const std::size_t limit =
      searchLimit == std::string::npos ? line.lineText.size() : searchLimit;
  std::size_t i{0};

  while (i < limit) {
    i = lowerLine.find("intent", i);
    if (i == std::string::npos || i >= limit) {
      break;
    }
    std::size_t j = i + 6;
    while (j < limit &&
           std::isspace(static_cast<unsigned char>(line.lineText[j]))) {
      ++j;
    }
    if (j >= limit || line.lineText[j] != '(') {
      i += 6;
      continue;
    }
    std::size_t k = j + 1;
    while (k < limit && line.lineText[k] != ')') {
      ++k;
    }
    if (k >= limit) {
      break;
    }
    std::string arg = lowerLine.substr(j + 1, k - (j + 1));
    arg.erase(std::remove_if(arg.begin(), arg.end(),
                             [](unsigned char ch) { return std::isspace(ch); }),
              arg.end());
    if (arg == "inout") {
      return parser::CharBlock{line.lineBegin + i, line.lineBegin + k + 1};
    }
    i = k + 1;
  }
  return std::nullopt;
}

// ---------------------------------------------------------------------------
// Per-procedure definite-write tracking helpers
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// MakeProcBindingSymbolSet
// ---------------------------------------------------------------------------

void UnusedIntentCheck::MakeProcBindingSymbolSet(
    semantics::SemanticsContext &context, const semantics::Scope &scope) {
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    if (auto *details{symbol.detailsIf<semantics::ProcBindingDetails>()}) {
      procBindingDetailsSymbolsMap_[&details->symbol()] = &symbol;
    }
  }
  for (const semantics::Scope &child : scope.children()) {
    MakeProcBindingSymbolSet(context, child);
  }
}

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

UnusedIntentCheck::UnusedIntentCheck(llvm::StringRef name,
                                     FlangTidyContext *context)
    : FlangTidyCheck{name, context} {
  MakeProcBindingSymbolSet(context->getSemanticsContext(),
                           context->getSemanticsContext().globalScope());
}

// ---------------------------------------------------------------------------
// Enter/Leave SubroutineSubprogram / FunctionSubprogram
// ---------------------------------------------------------------------------

void UnusedIntentCheck::EnterSubprogram(const parser::Name &name) {
  if (!name.symbol) {
    procStack_.push_back({nullptr, {}});
    return;
  }

  // Skip module files.
  if (name.symbol->owner().IsModuleFile()) {
    procStack_.push_back({nullptr, {}});
    return;
  }

  // Skip interface blocks.
  if (const auto *details =
          name.symbol->detailsIf<semantics::SubprogramDetails>()) {
    if (details->isInterface()) {
      procStack_.push_back({nullptr, {}});
      return;
    }
  }

  // Skip type-bound procedure implementations.
  if (procBindingDetailsSymbolsMap_.count(name.symbol) > 0) {
    procStack_.push_back({nullptr, {}});
    return;
  }

  // Find the body scope: the child of the owning scope whose symbol() matches.
  const semantics::Scope *bodyScope{nullptr};
  for (const semantics::Scope &child : name.symbol->owner().children()) {
    if (child.symbol() == name.symbol) {
      bodyScope = &child;
      break;
    }
  }
  procStack_.push_back({bodyScope, {}});
}

void UnusedIntentCheck::LeaveSubprogram() {
  if (procStack_.empty())
    return;
  ProcContext &ctx = procStack_.back();
  if (ctx.bodyScope) {
    // Defer emission: the procedure might be passed as an actual argument in a
    // later program unit, which would make changing its intent unsafe.
    deferredScopes_.push_back(
        {ctx.bodyScope, std::move(ctx.definitelyWritten)});
  }
  procStack_.pop_back();
}

void UnusedIntentCheck::Enter(const parser::SubroutineSubprogram &subr) {
  const auto &stmtWrapper =
      std::get<parser::Statement<parser::SubroutineStmt>>(subr.t);
  EnterSubprogram(std::get<parser::Name>(stmtWrapper.statement.t));
}
void UnusedIntentCheck::Leave(const parser::SubroutineSubprogram &) {
  LeaveSubprogram();
}

void UnusedIntentCheck::Enter(const parser::FunctionSubprogram &func) {
  const auto &stmtWrapper =
      std::get<parser::Statement<parser::FunctionStmt>>(func.t);
  EnterSubprogram(std::get<parser::Name>(stmtWrapper.statement.t));
}
void UnusedIntentCheck::Leave(const parser::FunctionSubprogram &) {
  LeaveSubprogram();
}

// ---------------------------------------------------------------------------
// Leave(AssignmentStmt) — direct assignment LHS marks the dummy as written.
// ---------------------------------------------------------------------------

void UnusedIntentCheck::Leave(const parser::AssignmentStmt &assignment) {
  if (procStack_.empty() || !procStack_.back().bodyScope)
    return;

  const auto &var = std::get<parser::Variable>(assignment.t);
  const auto *lhsExpr =
      semantics::GetExpr(context()->getSemanticsContext(), var);
  if (!lhsExpr)
    return;

  const semantics::Symbol *sym = evaluate::GetFirstSymbol(*lhsExpr);
  if (!sym)
    return;

  const semantics::Symbol &ultimate = sym->GetUltimate();
  if (&ultimate.owner() != procStack_.back().bodyScope)
    return;

  if (const auto *details =
          ultimate.detailsIf<semantics::ObjectEntityDetails>()) {
    if (details->isDummy()) {
      procStack_.back().definitelyWritten.insert(&ultimate);
    }
  }
}

// ---------------------------------------------------------------------------
// Leave(PointerAssignmentStmt) — "p => x" writes p (the pointer), not x.
// Flang's semantics marks x as defined; we deliberately do NOT do that here.
// ---------------------------------------------------------------------------

void UnusedIntentCheck::Leave(const parser::PointerAssignmentStmt &assignment) {
  if (procStack_.empty() || !procStack_.back().bodyScope)
    return;

  // LHS of a pointer assignment is a DataRef.
  const auto &lhsDataRef = std::get<parser::DataRef>(assignment.t);
  const parser::Name *name =
      std::get_if<parser::Name>(&lhsDataRef.u);
  if (!name || !name->symbol)
    return;

  const semantics::Symbol &ultimate = name->symbol->GetUltimate();
  if (&ultimate.owner() != procStack_.back().bodyScope)
    return;

  if (const auto *details =
          ultimate.detailsIf<semantics::ObjectEntityDetails>()) {
    if (details->isDummy()) {
      procStack_.back().definitelyWritten.insert(&ultimate);
    }
  }
}

// ---------------------------------------------------------------------------
// Enter(CallStmt) — actual args to dummies with explicit INTENT(OUT/INOUT)
// are definitely written.  Implicit-interface args are NOT counted here
// (unlike Flang's own check-call.cpp which marks them unconditionally).
// ---------------------------------------------------------------------------

void UnusedIntentCheck::Enter(const parser::CallStmt &callStmt) {
  const auto *procedureRef = callStmt.typedCall.get();
  if (!procedureRef)
    return;

  const semantics::Scope *bodyScope =
      (!procStack_.empty() && procStack_.back().bodyScope)
          ? procStack_.back().bodyScope
          : nullptr;

  for (const auto &arg : procedureRef->arguments()) {
    if (!arg)
      continue;

    const auto *expr = arg->UnwrapExpr();
    if (!expr)
      continue;

    // Record procedure-designator actual arguments unconditionally (regardless
    // of whether we are inside a tracked scope).  These procedures must keep
    // their dummies' intents compatible with the matching interface; changing
    // them would be unsafe, so we suppress warnings for those procedures.
    if (evaluate::IsProcedureDesignator(*expr)) {
      if (const auto *procDesig =
              std::get_if<evaluate::ProcedureDesignator>(&expr->u)) {
        if (const semantics::Symbol *procSym = procDesig->GetSymbol()) {
          procedureArgSymbols_.insert(&procSym->GetUltimate());
        }
      }
      continue; // procedure args cannot be data dummies — skip intent tracking
    }

    if (!bodyScope)
      continue;

    // Only count data arguments that are provably written by the callee.
    // dummyIntent() returns the dummy's declared intent from the explicit
    // interface, or Default/Unknown for implicit interfaces.
    const common::Intent intent = arg->dummyIntent();
    if (intent != common::Intent::Out && intent != common::Intent::InOut)
      continue;

    const semantics::Symbol *sym = evaluate::GetFirstSymbol(*expr);
    if (!sym)
      continue;

    const semantics::Symbol &ultimate = sym->GetUltimate();
    if (&ultimate.owner() != bodyScope)
      continue;

    if (const auto *details =
            ultimate.detailsIf<semantics::ObjectEntityDetails>()) {
      if (details->isDummy()) {
        procStack_.back().definitelyWritten.insert(&ultimate);
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Leave(Program) — emit all deferred scope warnings, suppressing procedures
// that were passed as actual procedure arguments anywhere in the program.
// ---------------------------------------------------------------------------

void UnusedIntentCheck::Leave(const parser::Program &) {
  for (const DeferredScope &deferred : deferredScopes_) {
    if (!deferred.bodyScope)
      continue;
    // If this procedure was passed as an actual argument, its dummy intents
    // must remain compatible with the matching interface — warn but don't fix.
    const semantics::Symbol *procSym = deferred.bodyScope->symbol();
    const bool isPassedAsProcArg =
        procSym &&
        procedureArgSymbols_.count(&procSym->GetUltimate()) > 0;
    EmitWarningsForScope(*deferred.bodyScope, deferred.definitelyWritten,
                         isPassedAsProcArg);
  }
  deferredScopes_.clear();
}

// ---------------------------------------------------------------------------
// EmitWarningsForScope — replaces CheckUnusedIntentHelper.
//
// WasDefined      = IsSymbolDefined (conservative, used for warnings).
// WasDefinitelyWrittenTo = definitelyWritten set (precise, used for fix-it
//                          direction so we never suggest intent(inout) for
//                          symbols only touched by implicit-interface calls
//                          or pointer-target association).
// ---------------------------------------------------------------------------

void UnusedIntentCheck::EmitWarningsForScope(
    const semantics::Scope &scope,
    const std::unordered_set<const semantics::Symbol *> &definitelyWritten,
    bool suppressFixIts) {

  auto &semCtx = context()->getSemanticsContext();

  // Conservative "possibly written" — includes implicit-interface args,
  // pointer targets, etc.  Used only for gating warnings (avoids false
  // positive warnings).
  auto WasDefined{[&semCtx](const semantics::Symbol &symbol) {
    return semCtx.IsSymbolDefined(symbol) ||
           semantics::IsInitialized(symbol, false, false, false);
  }};

  // Precise "definitely written" — only direct assignments, explicit
  // INTENT(OUT/INOUT) calls, and pointer-LHS assignments (from the parse-tree
  // walk above).  Used for fix-it direction.
  auto WasDefinitelyWrittenTo{[&](const semantics::Symbol &symbol) -> bool {
    return definitelyWritten.count(&symbol.GetUltimate()) > 0 ||
           semantics::IsInitialized(symbol, false, false, false);
  }};

  const auto *owningProc = scope.symbol();

  // Skip type-bound procedure implementations.
  if (procBindingDetailsSymbolsMap_.count(owningProc) > 0) {
    return;
  }

  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;

    const auto *details{symbol.detailsIf<semantics::ObjectEntityDetails>()};
    if (!details || !details->isDummy()) {
      continue;
    }

    // -----------------------------------------------------------------------
    // 1. Check: intent(inout) dummy that is never written to.
    // -----------------------------------------------------------------------
    const bool shouldCheckUnusedIntentInOut =
        semantics::IsIntentInOut(symbol) && symbol.Rank() == 0 &&
        !semantics::IsUnlimitedPolymorphic(symbol) &&
        !semantics::IsAssumedType(symbol);

    if (!WasDefined(symbol) && shouldCheckUnusedIntentInOut) {
      Say(symbol.name(),
          "Dummy argument '%s' with intent(inout) is never written to, "
          "consider changing to intent(in)"_warn_en_US,
          symbol.name());

      // Fix-it: safe to suggest intent(in) because !WasDefined means the
      // symbol is provably never written (not even through implicit ifaces).
      if (auto line = utils::getSourceLineInfo(semCtx, symbol.name())) {
        if (!suppressFixIts && utils::hasSingleEntityAfterDoubleColon(line->lineText)) {
          if (auto inoutRange = findIntentInoutRange(*line)) {
            const std::string replacementText =
                getIntentSpecSpelling(line->lineText, false);
            Fortran::tidy::FixItHint fix =
                Fortran::tidy::FixItHint::CreateReplacement(
                    *inoutRange, llvm::StringRef{replacementText});
            this->context()->addFixIt("bugprone-unused-intent", symbol.name(),
                                      fix);
          }
        } else if (!suppressFixIts &&
                   mixedIntentDeclsWithFix_.insert(line->lineBegin).second) {
          const std::string indentFromSource =
              utils::getLeadingWhitespaceFromSourceLine(semCtx, symbol.name());
          if (auto replacement = buildMixedIntentReplacement(
                  *line, symbol.name().ToString(), indentFromSource)) {
            Fortran::tidy::FixItHint fix =
                Fortran::tidy::FixItHint::CreateReplacement(
                    parser::CharBlock{line->lineBegin, line->lineEnd},
                    llvm::StringRef{*replacement});
            this->context()->addFixIt("bugprone-unused-intent", symbol.name(),
                                      fix);
          }
        }
      }
    }

    // -----------------------------------------------------------------------
    // 2. Check: dummy with no explicit intent.
    // -----------------------------------------------------------------------
    if (!symbol.attrs().HasAny(
            {semantics::Attr::INTENT_IN, semantics::Attr::INTENT_INOUT,
             semantics::Attr::INTENT_OUT, semantics::Attr::VALUE})) {
      Say(symbol.name(),
          "Dummy argument '%s' has no explicit intent"_warn_en_US,
          symbol.name());

      // Pointer and TARGET dummies: pointer association can be
      // misclassified as a definition by semantics.  Skip source fixes here.
      if (symbol.attrs().test(semantics::Attr::TARGET) ||
          symbol.attrs().test(semantics::Attr::POINTER)) {
        continue;
      }

      // Determine fix direction using WasDefinitelyWrittenTo (precise).
      // Three states:
      //   isDefinitelyWritten   → suggest intent(inout)
      //   isCertainlyNotWritten → suggest intent(in)   (conservative: if the
      //                          user's callee later turns out to write it,
      //                          the compiler will diagnose the mismatch)
      //   ambiguous             → emit the warning but no direction fix-it
      const bool isDefinitelyWritten = WasDefinitelyWrittenTo(symbol);
      const bool isCertainlyNotWritten = !WasDefined(symbol);

      if (!isDefinitelyWritten && !isCertainlyNotWritten) {
        // Ambiguous: the symbol reached an implicit-interface call or was a
        // pointer-assignment target.  We cannot safely suggest a direction.
        continue;
      }

      if (!suppressFixIts) {
        if (auto line = utils::getSourceLineInfo(semCtx, symbol.name())) {
          const std::string intentSpec =
              getIntentSpecSpelling(line->lineText, isDefinitelyWritten);

          if (utils::hasSingleEntityAfterDoubleColon(line->lineText)) {
            if (auto insertPos = utils::findDeclAttrInsertionPoint(
                    line->lineText, line->lineBegin)) {
              const std::string fixText = ", " + intentSpec;
              Fortran::tidy::FixItHint fix =
                  Fortran::tidy::FixItHint::CreateInsertion(
                      parser::CharBlock{*insertPos, *insertPos}, fixText);
              this->context()->addFixIt("bugprone-unused-intent", symbol.name(),
                                        fix);
            }
          } else if (mixedMissingIntentDeclsWithFix_.insert(line->lineBegin)
                         .second) {
            const std::string indentFromSource =
                utils::getLeadingWhitespaceFromSourceLine(semCtx, symbol.name());
            if (auto replacement = buildMissingIntentReplacementForMixedDecl(
                    *line, scope, indentFromSource, WasDefinitelyWrittenTo)) {
              Fortran::tidy::FixItHint fix =
                  Fortran::tidy::FixItHint::CreateReplacement(
                      parser::CharBlock{line->lineBegin, line->lineEnd},
                      llvm::StringRef{*replacement});
              this->context()->addFixIt("bugprone-unused-intent", symbol.name(),
                                        fix);
            } else {
              const std::string fixText =
                  buildMissingIntentInsertionForMixedDecl(
                      *line, symbol.name().ToString(), intentSpec,
                      indentFromSource)
                      .value_or(
                          "\n" + utils::getLeadingWhitespace(line->lineText) +
                          intentSpec +
                          utils::getDoubleColonSeparator(line->lineText) +
                          symbol.name().ToString());
              Fortran::tidy::FixItHint fix =
                  Fortran::tidy::FixItHint::CreateInsertion(
                      parser::CharBlock{line->lineEnd, line->lineEnd}, fixText);
              this->context()->addFixIt("bugprone-unused-intent", symbol.name(),
                                        fix);
            }
          }
        }
      }
    }
  }
}

} // namespace Fortran::tidy::bugprone
