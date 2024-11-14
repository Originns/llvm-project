#ifndef FORTRAN_TIDY_IMPLICITPROCDECLCHECK
#define FORTRAN_TIDY_IMPLICITPROCDECLCHECK

#include "flang/Semantics/semantics.h"

namespace Fortran::tidy::bugprone {

void CheckUndeclaredProc(semantics::SemanticsContext &);

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_IMPLICITPROCDECLCHECK
