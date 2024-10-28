#include "UnwrapSymbol.h"

namespace Fortran::tidy::utils {
std::optional<semantics::SymbolRef>
UnwrapSymbol(const parser::Name &name) {
  if (name.symbol) { // Check if semantic analysis has populated the symbol
    return semantics::SymbolRef{*name.symbol}; // Wrap in SymbolRef
  }
  return std::nullopt; // Return empty if symbol is not available
}
} // namespace Fortran::tidy::utils