#ifndef FORTRAN_TIDY_ARITHMETICIFSTMTCHECK
#define FORTRAN_TIDY_ARITHMETICIFSTMTCHECK

#include "FlangTidyCheck.h"
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

#endif // FORTRAN_TIDY_ARITHMETICIFSTMTCHECK
