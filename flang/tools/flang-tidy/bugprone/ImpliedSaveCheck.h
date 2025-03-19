#ifndef FORTRAN_TIDY_IMPLICITSAVECHECK
#define FORTRAN_TIDY_IMPLICITSAVECHECK

#include "../FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::bugprone {

class ImpliedSaveCheck : public virtual FlangTidyCheck {
public:
  ImpliedSaveCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~ImpliedSaveCheck() = default;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_IMPLICITSAVECHECK
