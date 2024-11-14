#ifndef FORTRAN_TIDY_PRECISIONLOSS
#define FORTRAN_TIDY_PRECISIONLOSS

#include "flang/Semantics/semantics.h"

namespace Fortran::tidy::bugprone {

class PrecisionLossCheck : public virtual semantics::BaseChecker {
public:
  explicit PrecisionLossCheck(semantics::SemanticsContext &);
  ~PrecisionLossCheck();
  void Enter(const parser::AssignmentStmt &);

private:
  semantics::SemanticsContext &context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_PRECISIONLOSS
