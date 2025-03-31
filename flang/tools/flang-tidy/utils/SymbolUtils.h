#ifndef FORTRAN_TIDY_UTILS_SYMBOL
#define FORTRAN_TIDY_UTILS_SYMBOL

#include "flang/Semantics/scope.h"
#include "flang/Semantics/symbol.h"

namespace Fortran::tidy::utils {

inline bool IsFromModFileSafe(const semantics::Symbol &sym) {
  return sym.test(semantics::Symbol::Flag::ModFile) ||
         (!sym.owner().IsTopLevel() &&
          (sym.owner().symbol() && IsFromModFileSafe(*sym.owner().symbol())));
}

} // namespace Fortran::tidy::utils

#endif // FORTRAN_TIDY_UTILS_SYMBOL
