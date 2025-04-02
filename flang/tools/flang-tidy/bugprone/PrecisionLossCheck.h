#ifndef FORTRAN_TIDY_PRECISIONLOSS
#define FORTRAN_TIDY_PRECISIONLOSS

#include "FlangTidyCheck.h"

namespace Fortran::tidy::bugprone {

class PrecisionLossCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  // explicit PrecisionLossCheck(llvm::StringRef name, FlangTidyContext
  // *context);
  virtual ~PrecisionLossCheck() = default;
  void Enter(const parser::AssignmentStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_PRECISIONLOSS
