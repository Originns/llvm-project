#ifndef FORTRAN_TIDY_ARITHMETICGOTOCHECK
#define FORTRAN_TIDY_ARITHMETICGOTOCHECK

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

class ArithmeticGotoCheck : public FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~ArithmeticGotoCheck() = default;
  void Enter(const parser::ComputedGotoStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_ARITHMETICGOTOCHECK
