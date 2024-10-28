#ifndef FORTRAN_TIDY_IMPLICITDECLCHECK
#define FORTRAN_TIDY_IMPLICITDECLCHECK

#include "flang/Evaluate/expression.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy {

void CheckImplicitDecl(semantics::SemanticsContext &);

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_IMPLICITDECLCHECK
