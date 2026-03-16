//===--- UnusedIntentCheck.h - flang-tidy -----------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_UNUSEDINTENTCHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_UNUSEDINTENTCHECK_H

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace Fortran::tidy::bugprone {

/// This check verifies that all INTENT attributes are used.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/unused-intent.html
class UnusedIntentCheck : public virtual FlangTidyCheck {
public:
  UnusedIntentCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~UnusedIntentCheck() = default;

  void Enter(const parser::SubroutineSubprogram &) override;
  void Leave(const parser::SubroutineSubprogram &) override;
  void Enter(const parser::FunctionSubprogram &) override;
  void Leave(const parser::FunctionSubprogram &) override;
  void Leave(const parser::AssignmentStmt &) override;
  void Leave(const parser::PointerAssignmentStmt &) override;
  void Enter(const parser::CallStmt &) override;

private:
  // Per-procedure context built during parse-tree walk.
  struct ProcContext {
    const semantics::Scope *bodyScope{nullptr};
    // Symbols provably written in this procedure:
    //   - LHS of an assignment statement
    //   - actual arg to an explicit-interface dummy with INTENT(OUT/INOUT)
    //   - LHS of a pointer assignment (the pointer itself, NOT its target)
    std::unordered_set<const semantics::Symbol *> definitelyWritten;
  };

  std::vector<ProcContext> procStack_;
  std::unordered_map<const semantics::Symbol *, const semantics::Symbol *>
      procBindingDetailsSymbolsMap_;
  // Guards against duplicate fix-its on the same source line.
  std::unordered_set<const char *> mixedIntentDeclsWithFix_;
  std::unordered_set<const char *> mixedMissingIntentDeclsWithFix_;

  void EnterSubprogram(const parser::Name &name);
  void LeaveSubprogram();
  void EmitWarningsForScope(
      const semantics::Scope &scope,
      const std::unordered_set<const semantics::Symbol *> &definitelyWritten);
  void MakeProcBindingSymbolSet(semantics::SemanticsContext &context,
                                const semantics::Scope &scope);
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_UNUSEDINTENTCHECK_H
