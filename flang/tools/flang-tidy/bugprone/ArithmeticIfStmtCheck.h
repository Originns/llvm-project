#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_ARITHMETICIFSTMTCHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_ARITHMETICIFSTMTCHECK_H

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

/// This check detects the use of arithmetic IF statements.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/arithmetic-if-statement.html
class ArithmeticIfStmtCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~ArithmeticIfStmtCheck() = default;
  void Enter(const parser::ArithmeticIfStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_ARITHMETICIFSTMTCHECK_H
