#ifndef FORTRAN_TIDY_ARITHMETICGOTOCHECK
#define FORTRAN_TIDY_ARITHMETICGOTOCHECK

#include "flang/Evaluate/expression.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::bugprone {

class ArithmeticGotoCheck : public virtual semantics::BaseChecker {
public:
  explicit ArithmeticGotoCheck(semantics::SemanticsContext &context)
      : context_{context} {}
  ~ArithmeticGotoCheck();
  void Enter(const parser::ComputedGotoStmt &);

private:
  semantics::SemanticsContext &context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_ARITHMETICGOTOCHECK
