#ifndef FORTRAN_TIDY_UNWRAPS
#define FORTRAN_TIDY_UNWRAPS

#include "flang/Evaluate/expression.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::utils {

std::optional<semantics::SymbolRef> UnwrapSymbol(const parser::Name &name);

} // namespace Fortran::tidy::utils

#endif // FORTRAN_TIDY_UNWRAPS