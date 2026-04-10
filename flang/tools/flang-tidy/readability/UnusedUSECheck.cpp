//===--- UnusedUSECheck.cpp - flang-tidy ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "UnusedUSECheck.h"
#include "flang/Evaluate/expression.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "utils/FixIt.h"
#include "llvm/ADT/StringRef.h"
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace Fortran::tidy::readability {

using namespace parser::literals;

namespace {

static bool isSingleLine(parser::CharBlock source) {
  return !llvm::StringRef{source.begin(), source.size()}.contains('\n');
}

static std::optional<parser::CharBlock>
getEntireSourceLine(semantics::SemanticsContext &ctx,
                    parser::CharBlock anchor) {
  if (anchor.begin() == nullptr) {
    return std::nullopt;
  }

  const auto &allCooked = ctx.allCookedSources();
  const auto *cooked = allCooked.Find(anchor);
  if (!cooked) {
    return std::nullopt;
  }

  const parser::CharBlock source = cooked->AsCharBlock();
  const char *lineBegin = anchor.begin();
  const char *lineEnd = anchor.end();

  while (lineBegin > source.begin() && lineBegin[-1] != '\n') {
    --lineBegin;
  }
  while (lineEnd < source.end() && *lineEnd != '\n') {
    ++lineEnd;
  }
  if (lineEnd < source.end() && *lineEnd == '\n') {
    ++lineEnd;
  }

  return parser::CharBlock{lineBegin,
                           static_cast<std::size_t>(lineEnd - lineBegin)};
}

} // namespace

UnusedUSECheck::UnusedUSECheck(llvm::StringRef name, FlangTidyContext *context)
    : FlangTidyCheck(name, context) {}

void UnusedUSECheck::Enter(const parser::UseStmt &stmt) {
  ++activeUseStmtDepth_;

  const semantics::Symbol *moduleSymbol = stmt.moduleName.symbol;
  if (!moduleSymbol) {
    return;
  }

  const parser::CharBlock stmtSource =
      context()->getSemanticsContext().location().value_or(
          stmt.moduleName.source);
  UseStmtFixInfo fixInfo;
  fixInfo.stmtSource = stmtSource;
  fixInfo.diagnosticLoc = stmt.moduleName.source;
  fixInfo.moduleSymbol = moduleSymbol;

  if (std::holds_alternative<std::list<parser::Only>>(stmt.u)) {
    const auto &onlyList = std::get<std::list<parser::Only>>(stmt.u);
    // Pre-fetch the local scope once for the whole ONLY list.
    const semantics::Scope &currentScope =
        context()->getSemanticsContext().FindScope(stmtSource);
    for (const auto &only : onlyList) {
      if (const auto *name = std::get_if<parser::Name>(&only.u)) {
        if (!name->symbol) {
          fixInfo.hasUnsupportedItem = true;
          continue;
        }
        // Flang's resolver sets name->symbol to the *module's* symbol via
        //   Resolve(name, AddUse(...).use)
        // but in usage contexts name.symbol is the *local* scope symbol (with
        // UseDetails pointing back to the module symbol).  Find the local
        // symbol directly so importedSymbols_ and usedSymbols_ share the
        // same key.
        const semantics::Symbol *localSym =
            currentScope.FindSymbol(name->source);
        if (!localSym) {
          fixInfo.hasUnsupportedItem = true;
          continue;
        }
        importedSymbols_[localSym] = {name->source, name->source, stmtSource};
        fixInfo.items.push_back({localSym, name->source});
      } else if (const auto *rename = std::get_if<parser::Rename>(&only.u)) {
        if (const auto *names =
                std::get_if<parser::Rename::Names>(&rename->u)) {
          const auto &localName = std::get<0>(names->t);
          const auto &useName = std::get<1>(names->t);
          if (!localName.symbol) {
            fixInfo.hasUnsupportedItem = true;
            continue;
          }
          const parser::CharBlock itemSource{
              localName.source.begin(),
              static_cast<std::size_t>(useName.source.end() -
                                       localName.source.begin())};
          importedSymbols_[localName.symbol] = {localName.source, itemSource,
                                                stmtSource};
          fixInfo.items.push_back({localName.symbol, itemSource});
        } else {
          fixInfo.hasUnsupportedItem = true;
        }
      } else {
        fixInfo.hasUnsupportedItem = true;
      }
    }
  } else {
    const auto &renameList = std::get<std::list<parser::Rename>>(stmt.u);
    if (!renameList.empty()) {
      for (const auto &rename : renameList) {
        if (const auto *names = std::get_if<parser::Rename::Names>(&rename.u)) {
          const auto &localName = std::get<0>(names->t);
          const auto &useName = std::get<1>(names->t);
          if (!localName.symbol) {
            fixInfo.hasUnsupportedItem = true;
            continue;
          }
          const parser::CharBlock itemSource{
              localName.source.begin(),
              static_cast<std::size_t>(useName.source.end() -
                                       localName.source.begin())};
          importedSymbols_[localName.symbol] = {localName.source, itemSource,
                                                stmtSource};
          fixInfo.items.push_back({localName.symbol, itemSource});
        } else {
          fixInfo.hasUnsupportedItem = true;
        }
      }
    } else {
      wholeModuleImports_[moduleSymbol] = {stmt.moduleName.source, stmtSource};
      fixInfo.isWholeModuleImport = true;
    }
  }

  useStmtFixes_.push_back(fixInfo);
}

// Mark a symbol as used, tracing back through UseDetails to find which of the
// directly-imported whole-module imports provides it.  This handles:
//   - ordinary named references (via Enter(parser::Name))
//   - operator overloads and other unnamed invocations (via Enter(parser::Expr)
//     with evaluate::CollectSymbols)
//   - transitively re-exported symbols, by inspecting the local symbol's
//     UseDetails rather than always following GetUltimate()
void UnusedUSECheck::checkSymbol(const semantics::Symbol &sym) {
  // ONLY-list import: mark the specific symbol as used.
  if (importedSymbols_.find(&sym) != importedSymbols_.end()) {
    usedSymbols_.insert(&sym);
    return;
  }

  const semantics::Symbol &ultimate = sym.GetUltimate();

  // The ultimate may itself live in an ONLY-list import (rare, but possible
  // when CollectSymbols returns the ultimate rather than the local alias).
  if (importedSymbols_.find(&ultimate) != importedSymbols_.end()) {
    usedSymbols_.insert(&ultimate);
    return;
  }

  // Whole-module import: the ultimate symbol's owning module is directly used.
  const semantics::Symbol *ownerSymbol = ultimate.owner().symbol();
  if (ownerSymbol && ownerSymbol->has<semantics::ModuleDetails>() &&
      wholeModuleImports_.find(ownerSymbol) != wholeModuleImports_.end()) {
    usedModules_.insert(ownerSymbol);
    return;
  }

  // The ultimate has UseDetails (it was itself use-associated in the module
  // that re-exported it).  Check its immediate source module.
  if (const auto *useDetails = ultimate.detailsIf<semantics::UseDetails>()) {
    const semantics::Symbol *moduleSymbol =
        useDetails->symbol().owner().symbol();
    if (moduleSymbol && moduleSymbol->has<semantics::ModuleDetails>() &&
        wholeModuleImports_.find(moduleSymbol) != wholeModuleImports_.end()) {
      usedModules_.insert(moduleSymbol);
      return;
    }
  }

  // Transitive re-export: the local symbol (sym) is use-associated from an
  // intermediate module (e.g. loct_math_oct_m) that re-exports from the
  // ultimate source (e.g. math_oct_m).  sym's own UseDetails points at the
  // intermediate module, which is the one the user directly imported.
  if (&sym != &ultimate) {
    if (const auto *useDetails = sym.detailsIf<semantics::UseDetails>()) {
      const semantics::Symbol *moduleSymbol =
          useDetails->symbol().owner().symbol();
      if (moduleSymbol && moduleSymbol->has<semantics::ModuleDetails>() &&
          wholeModuleImports_.find(moduleSymbol) != wholeModuleImports_.end()) {
        usedModules_.insert(moduleSymbol);
      }
    }
  }
}

void UnusedUSECheck::Enter(const parser::Name &name) {
  if (activeUseStmtDepth_ > 0 || !name.symbol) {
    return;
  }
  checkSymbol(*name.symbol);
}

void UnusedUSECheck::Enter(const parser::Expr &expr) {
  if (activeUseStmtDepth_ > 0 || wholeModuleImports_.empty()) {
    return;
  }
  // Retrieve the semantics-annotated (typed) expression.  This is populated
  // after name resolution and contains resolved procedure references for
  // operator overloads, which never appear as parser::Name nodes.
  const auto *typedExpr{
      semantics::GetExpr(context()->getSemanticsContext(), expr)};
  if (!typedExpr) {
    return;
  }
  for (const semantics::Symbol &sym : evaluate::CollectSymbols(*typedExpr)) {
    checkSymbol(sym);
  }
}

void UnusedUSECheck::Leave(const parser::UseStmt &) {
  if (activeUseStmtDepth_ > 0) {
    --activeUseStmtDepth_;
  }
}

std::optional<Fortran::tidy::FixItHint>
UnusedUSECheck::buildFix(const UseStmtFixInfo &info) {
  if (!isSingleLine(info.stmtSource)) {
    return std::nullopt;
  }

  auto lineRange =
      getEntireSourceLine(context()->getSemanticsContext(), info.stmtSource);
  if (!lineRange) {
    return std::nullopt;
  }

  if (info.isWholeModuleImport) {
    return Fortran::tidy::FixItHint::CreateRemoval(*lineRange);
  }

  if (info.hasUnsupportedItem || info.items.empty()) {
    return std::nullopt;
  }

  std::vector<std::string> keptItems;
  for (const auto &item : info.items) {
    if (item.symbol && usedSymbols_.find(item.symbol) != usedSymbols_.end()) {
      keptItems.push_back(
          llvm::StringRef{item.itemSource.begin(), item.itemSource.size()}
              .trim()
              .str());
    }
  }

  if (keptItems.empty()) {
    return Fortran::tidy::FixItHint::CreateRemoval(*lineRange);
  }

  llvm::StringRef stmtText{info.stmtSource.begin(), info.stmtSource.size()};
  std::size_t commentPos = stmtText.find('!');
  llvm::StringRef declText = stmtText.substr(
      0, commentPos == llvm::StringRef::npos ? stmtText.size() : commentPos);
  llvm::StringRef commentText = commentPos == llvm::StringRef::npos
                                    ? llvm::StringRef{}
                                    : stmtText.substr(commentPos);

  const auto &firstItem = info.items.front();
  if (firstItem.itemSource.begin() < info.stmtSource.begin() ||
      firstItem.itemSource.begin() > declText.end()) {
    return std::nullopt;
  }

  llvm::StringRef prefix{info.stmtSource.begin(),
                         static_cast<std::size_t>(firstItem.itemSource.begin() -
                                                  info.stmtSource.begin())};
  std::string replacement = prefix.str();
  for (std::size_t i = 0; i < keptItems.size(); ++i) {
    if (i > 0) {
      replacement += ", ";
    }
    replacement += keptItems[i];
  }
  replacement += commentText.str();

  return Fortran::tidy::FixItHint::CreateReplacement(
      info.stmtSource, llvm::StringRef{replacement});
}

void UnusedUSECheck::Leave(const parser::ProgramUnit &) {
  for (auto &fixInfo : useStmtFixes_) {
    if (fixInfo.isWholeModuleImport) {
      if (!fixInfo.moduleSymbol ||
          usedModules_.find(fixInfo.moduleSymbol) != usedModules_.end()) {
        continue;
      }

      if (!fixInfo.fixEmitted) {
        if (auto fix = buildFix(fixInfo)) {
          context()->addFixIt(name(), fixInfo.diagnosticLoc, *fix);
          fixInfo.fixEmitted = true;
        }
      }
      continue;
    }

    bool hasUnusedItem = false;
    for (const auto &item : fixInfo.items) {
      if (item.symbol && usedSymbols_.find(item.symbol) == usedSymbols_.end()) {
        hasUnusedItem = true;
        break;
      }
    }
    if (!hasUnusedItem) {
      continue;
    }

    if (!fixInfo.fixEmitted) {
      if (auto fix = buildFix(fixInfo)) {
        context()->addFixIt(name(), fixInfo.diagnosticLoc, *fix);
        fixInfo.fixEmitted = true;
      }
    }
  }

  for (const auto &[symbol, info] : importedSymbols_) {
    if (usedSymbols_.find(symbol) == usedSymbols_.end()) {
      Say(info.diagnosticLoc, "Unused symbol '%s' in USE statement"_warn_en_US,
          symbol->name());
    }
  }

  for (const auto &[moduleSymbol, info] : wholeModuleImports_) {
    if (usedModules_.find(moduleSymbol) == usedModules_.end()) {
      Say(info.diagnosticLoc, "Unused USE statement for module '%s'"_warn_en_US,
          moduleSymbol->name());
    }
  }

  importedSymbols_.clear();
  usedSymbols_.clear();
  wholeModuleImports_.clear();
  usedModules_.clear();
  useStmtFixes_.clear();
  activeUseStmtDepth_ = 0;
}

} // namespace Fortran::tidy::readability
