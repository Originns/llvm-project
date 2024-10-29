#ifndef FORTRAN_TIDY_IMPLICITDECLCHECK
#define FORTRAN_TIDY_IMPLICITDECLCHECK

#include "flang/Evaluate/expression.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::bugprone {

void CheckImplicitDecl(semantics::SemanticsContext &);

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_IMPLICITDECLCHECK
