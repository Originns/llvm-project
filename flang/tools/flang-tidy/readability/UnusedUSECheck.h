//===--- UnusedUSECheck.h - flang-tidy --------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_READABILITY_UNUSEUSECHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_READABILITY_UNUSEUSECHECK_H

#include "FlangTidyCheck.h"
#include "utils/FixIt.h"
#include <map>
#include <optional>
#include <set>
#include <vector>

namespace Fortran::tidy::readability {

class UnusedUSECheck : public FlangTidyCheck {
public:
  UnusedUSECheck(llvm::StringRef name, FlangTidyContext *context);

  void Enter(const parser::UseStmt &) override;
  void Enter(const parser::Name &) override;
  void Enter(const parser::Expr &) override;
  void Leave(const parser::UseStmt &) override;
  void Leave(const parser::ProgramUnit &) override;

private:
  struct ImportedSymbolInfo {
    parser::CharBlock diagnosticLoc;
    parser::CharBlock itemSource;
    parser::CharBlock stmtSource;
  };

  struct WholeModuleImportInfo {
    parser::CharBlock diagnosticLoc;
    parser::CharBlock stmtSource;
  };

  struct ImportedItemForFix {
    const semantics::Symbol *symbol{nullptr};
    parser::CharBlock itemSource;
  };

  struct UseStmtFixInfo {
    parser::CharBlock stmtSource;
    parser::CharBlock diagnosticLoc;
    const semantics::Symbol *moduleSymbol{nullptr};
    bool isWholeModuleImport{false};
    bool hasUnsupportedItem{false};
    bool fixEmitted{false};
    std::vector<ImportedItemForFix> items;
  };

  std::optional<Fortran::tidy::FixItHint> buildFix(const UseStmtFixInfo &info);
  void checkSymbol(const semantics::Symbol &sym);

  std::map<const semantics::Symbol *, ImportedSymbolInfo> importedSymbols_;
  std::set<const semantics::Symbol *> usedSymbols_;
  std::map<const semantics::Symbol *, WholeModuleImportInfo>
      wholeModuleImports_;
  std::set<const semantics::Symbol *> usedModules_;
  std::vector<UseStmtFixInfo> useStmtFixes_;
  unsigned activeUseStmtDepth_{0};
};

} // namespace Fortran::tidy::readability

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_READABILITY_UNUSEUSECHECK_H
