#ifndef FORTRAN_TIDY_IMPLICITPROCDECLCHECK
#define FORTRAN_TIDY_IMPLICITPROCDECLCHECK

#include "../FlangTidyCheck.h"
#include "../FlangTidyContext.h"
#include "FlangTidyContext.h"

namespace Fortran::tidy::bugprone {

class UndeclaredProcCheck : public virtual FlangTidyCheck {
public:
  UndeclaredProcCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~UndeclaredProcCheck() = default;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_IMPLICITPROCDECLCHECK
