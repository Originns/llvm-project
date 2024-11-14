#ifndef FORTRAN_TIDY_ARITHMETICGOTOCHECK
#define FORTRAN_TIDY_ARITHMETICGOTOCHECK

#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/semantics.h"

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
