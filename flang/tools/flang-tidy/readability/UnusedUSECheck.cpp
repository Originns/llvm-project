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
#include "utils/SourceEditUtils.h"
#include "llvm/ADT/StringRef.h"
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace Fortran::tidy::readability {

using namespace parser::literals;


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

// Mark a symbol as used by walking the UseDetails/HostAssocDetails chain one
// step at a time.  At each level we check whether the *owner* of the current
// symbol is a module the user directly imported.  This correctly handles:
//   - ordinary named references (via Enter(parser::Name))
//   - operator overloads without parse-tree names (via Enter(parser::Expr) +
//     evaluate::CollectSymbols)
//   - symbols accessed via host association from a containing module
//   - intrinsic-module procedures (e.g. c_loc, ieee_is_nan) that are
//     re-exported internally through __fortran_builtins — GetUltimate() would
//     skip past the user-visible module, so we must stop at each intermediate
//     step instead
void UnusedUSECheck::checkSymbol(const semantics::Symbol &sym) {
  const semantics::Symbol *current = &sym;
  while (current) {
    // ONLY-list import: the local alias is the key.
    if (importedSymbols_.find(current) != importedSymbols_.end()) {
      usedSymbols_.insert(current);
      return;
    }

    // Whole-module import: does the scope that owns *current* belong to a
    // module the user directly imported?
    const semantics::Symbol *ownerModule = current->owner().symbol();
    if (ownerModule && ownerModule->has<semantics::ModuleDetails>() &&
        wholeModuleImports_.find(ownerModule) != wholeModuleImports_.end()) {
      usedModules_.insert(ownerModule);
      return;
    }

    // Follow the use/host-association chain one level deeper.
    if (const auto *ud = current->detailsIf<semantics::UseDetails>()) {
      current = &ud->symbol();
    } else if (const auto *hd =
                   current->detailsIf<semantics::HostAssocDetails>()) {
      current = &hd->symbol();
    } else {
      return; // bottom of chain with no match
    }
  }
}

void UnusedUSECheck::Enter(const parser::Name &name) {
  if (activeUseStmtDepth_ > 0 || !name.symbol) {
    return;
  }
  // Flang's resolver sets name.symbol to GetUltimate() for procedure calls
  // (e.g. c_loc → __builtin_c_loc in __fortran_builtins), discarding the
  // intermediate use-association chain.  Look the name up in the current
  // scope to recover the local/use-associated symbol that correctly traces
  // back to the directly-imported module.
  const semantics::Symbol *sym{name.symbol};
  if (!importedSymbols_.empty() || !wholeModuleImports_.empty()) {
    if (auto loc{context()->getSemanticsContext().location()}) {
      const semantics::Scope &scope{
          context()->getSemanticsContext().FindScope(*loc)};
      if (const semantics::Symbol *localSym{scope.FindSymbol(name.source)}) {
        sym = localSym;
      }
    }
  }
  checkSymbol(*sym);
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
  // All of our edits require the statement to occupy exactly one line.
  auto lineRange{utils::removeStatementLine(
      context()->getSemanticsContext(), info.stmtSource)};
  if (!lineRange) {
    return std::nullopt;
  }

  // Whole-module import with no ONLY list — just delete the line.
  if (info.isWholeModuleImport) {
    return lineRange;
  }

  if (info.hasUnsupportedItem || info.items.empty()) {
    return std::nullopt;
  }

  // Collect the subset of named imports that are still in use.
  std::vector<std::string> keptItems;
  for (const auto &item : info.items) {
    if (item.symbol && usedSymbols_.find(item.symbol) != usedSymbols_.end()) {
      keptItems.push_back(
          llvm::StringRef{item.itemSource.begin(), item.itemSource.size()}
              .trim()
              .str());
    }
  }

  // All ONLY items are unused — delete the whole line.
  if (keptItems.empty()) {
    return lineRange;
  }

  // Some items are still needed: rebuild the statement keeping only those.
  // Split off any trailing comment so we can reattach it unchanged.
  llvm::StringRef stmtText{info.stmtSource.begin(), info.stmtSource.size()};
  llvm::StringRef declText{utils::stripTrailingComment(stmtText)};
  llvm::StringRef commentText{stmtText.substr(declText.size())};

  const auto &firstItem = info.items.front();
  if (firstItem.itemSource.begin() < info.stmtSource.begin() ||
      firstItem.itemSource.begin() > declText.end()) {
    return std::nullopt;
  }

  // Everything before the first ONLY item (e.g. "use m, only: ") is kept.
  llvm::StringRef prefix{info.stmtSource.begin(),
                         static_cast<std::size_t>(firstItem.itemSource.begin() -
                                                  info.stmtSource.begin())};
  std::string replacement{prefix.str()};
  for (std::size_t i{0}; i < keptItems.size(); ++i) {
    if (i > 0)
      replacement += ", ";
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
