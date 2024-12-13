#ifndef FORTRAN_TIDY_PRECISIONLOSS
#define FORTRAN_TIDY_PRECISIONLOSS

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::bugprone {

class PrecisionLossCheck : public virtual FlangTidyCheck {
public:
  explicit PrecisionLossCheck(llvm::StringRef name, FlangTidyContext *context);
  ~PrecisionLossCheck();
  void Enter(const parser::AssignmentStmt &);

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_PRECISIONLOSS
