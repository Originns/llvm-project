#ifndef FORTRAN_TIDY_UNUSEDINTENTCHECK
#define FORTRAN_TIDY_UNUSEDINTENTCHECK

#include "../FlangTidyCheck.h"
#include "FlangTidyContext.h"

namespace Fortran::tidy::bugprone {

class UnusedIntentCheck : public virtual FlangTidyCheck {
public:
  UnusedIntentCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~UnusedIntentCheck() = default;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_UNUSEDINTENTCHECK
