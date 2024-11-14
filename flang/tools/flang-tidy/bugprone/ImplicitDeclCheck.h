#ifndef FORTRAN_TIDY_IMPLICITDECLCHECK
#define FORTRAN_TIDY_IMPLICITDECLCHECK

#include "flang/Semantics/semantics.h"

namespace Fortran::tidy::bugprone {

void CheckImplicitDecl(semantics::SemanticsContext &);

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_IMPLICITDECLCHECK
