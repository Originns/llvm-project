#ifndef FORTRAN_TIDY_UNUSEDINTENTCHECK
#define FORTRAN_TIDY_UNUSEDINTENTCHECK

#include "flang/Evaluate/expression.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::bugprone {

void CheckUnusedIntent(semantics::SemanticsContext &);

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_UNUSEDINTENTCHECK
