#ifndef FORTRAN_TIDY_IMPLICITDECLCHECK
#define FORTRAN_TIDY_IMPLICITDECLCHECK

#include "../FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::bugprone {

/// This check verifies that all variables are declared before use.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/implicit-declaration.html
class ImplicitDeclCheck : public virtual FlangTidyCheck {
public:
  ImplicitDeclCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~ImplicitDeclCheck() = default;
private:
  void CheckForImplicitDeclarations(semantics::SemanticsContext &, const semantics::Scope &);
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_IMPLICITDECLCHECK
