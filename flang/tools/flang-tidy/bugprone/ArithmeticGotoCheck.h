#ifndef FORTRAN_TIDY_ARITHMETICGOTOCHECK
#define FORTRAN_TIDY_ARITHMETICGOTOCHECK

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

/// This check detects the use of arithmetic GOTO statements in Fortran code.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/bugprone-arithmetic-goto.html
class ArithmeticGotoCheck : public FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~ArithmeticGotoCheck() = default;
  void Enter(const parser::ComputedGotoStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_ARITHMETICGOTOCHECK
