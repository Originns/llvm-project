//===--- UnusedUSECheck.cpp - flang-tidy ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "UnusedUSECheck.h"
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
  UseStmtFixInfo fixInfo{stmtSource, stmt.moduleName.source, moduleSymbol};

  if (std::holds_alternative<std::list<parser::Only>>(stmt.u)) {
    const auto &onlyList = std::get<std::list<parser::Only>>(stmt.u);
    for (const auto &only : onlyList) {
      if (const auto *name = std::get_if<parser::Name>(&only.u)) {
        if (!name->symbol) {
          fixInfo.hasUnsupportedItem = true;
          continue;
        }
        importedSymbols_[name->symbol] = {name->source, name->source,
                                          stmtSource};
        fixInfo.items.push_back({name->symbol, name->source});
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

void UnusedUSECheck::Enter(const parser::Name &name) {
  if (activeUseStmtDepth_ > 0 || !name.symbol) {
    return;
  }

  const semantics::Symbol *symbol = name.symbol;

  if (importedSymbols_.find(symbol) != importedSymbols_.end()) {
    usedSymbols_.insert(symbol);
    return;
  }

  const semantics::Symbol &ultimate = symbol->GetUltimate();
  const semantics::Symbol *ownerSymbol = ultimate.owner().symbol();
  if (ownerSymbol && ownerSymbol->has<semantics::ModuleDetails>() &&
      wholeModuleImports_.find(ownerSymbol) != wholeModuleImports_.end()) {
    usedModules_.insert(ownerSymbol);
    return;
  }

  if (const auto *useDetails = ultimate.detailsIf<semantics::UseDetails>()) {
    const semantics::Symbol *moduleSymbol =
        useDetails->symbol().owner().symbol();
    if (moduleSymbol && moduleSymbol->has<semantics::ModuleDetails>() &&
        wholeModuleImports_.find(moduleSymbol) != wholeModuleImports_.end()) {
      usedModules_.insert(moduleSymbol);
    }
  }

  if (symbol != &ultimate) {
    if (const auto *useDetails = symbol->detailsIf<semantics::UseDetails>()) {
      const semantics::Symbol *moduleSymbol =
          useDetails->symbol().owner().symbol();
      if (moduleSymbol && moduleSymbol->has<semantics::ModuleDetails>() &&
          wholeModuleImports_.find(moduleSymbol) != wholeModuleImports_.end()) {
        usedModules_.insert(moduleSymbol);
      }
    }
  }
}

void UnusedUSECheck::Leave(const parser::UseStmt &) {
  if (activeUseStmtDepth_ > 0) {
    --activeUseStmtDepth_;
  }
}

std::optional<Fortran::tidy::FixItHint>
UnusedUSECheck::buildFix(const UseStmtFixInfo &info) const {
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

  return Fortran::tidy::FixItHint::CreateReplacement(info.stmtSource,
                                                     replacement);
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
