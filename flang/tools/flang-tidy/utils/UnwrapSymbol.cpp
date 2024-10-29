#include "UnwrapSymbol.h"

namespace Fortran::tidy::utils {

std::optional<semantics::SymbolRef> UnwrapSymbol(const parser::Name &name) {
  if (name.symbol) {
    return semantics::SymbolRef{*name.symbol};
  }
  return std::nullopt;
}

} // namespace Fortran::tidy::utils