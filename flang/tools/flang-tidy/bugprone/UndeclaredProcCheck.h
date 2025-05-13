#ifndef FORTRAN_TIDY_IMPLICITPROCDECLCHECK
#define FORTRAN_TIDY_IMPLICITPROCDECLCHECK

#include "../FlangTidyCheck.h"
#include "../FlangTidyContext.h"
#include "FlangTidyContext.h"

namespace Fortran::tidy::bugprone {

/// This check verifies that all procedures are explicitly declared.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/implicit-procedure-declaration.html
class UndeclaredProcCheck : public virtual FlangTidyCheck {
public:
  UndeclaredProcCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~UndeclaredProcCheck() = default;
private:
  void CheckForUndeclaredProcedures(semantics::SemanticsContext &, const semantics::Scope &);
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_IMPLICITPROCDECLCHECK
