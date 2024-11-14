#ifndef FORTRAN_TIDY_SHORTCIRCUIT
#define FORTRAN_TIDY_SHORTCIRCUIT

#include "flang/Semantics/semantics.h"

namespace Fortran::tidy::bugprone {

class ShortCircuitCheck : public virtual semantics::BaseChecker {
public:
  explicit ShortCircuitCheck(semantics::SemanticsContext &);
  ~ShortCircuitCheck();
  void Enter(const parser::IfThenStmt &);

private:
  semantics::SemanticsContext &context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_SHORTCIRCUIT
