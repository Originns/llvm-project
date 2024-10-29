#include "ImplicitDeclCheck.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
void CheckForImplicitDeclarations(semantics::SemanticsContext &context,
                                  const semantics::Scope &scope) {
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    if (symbol.test(semantics::Symbol::Flag::Implicit)) {
      context.Say(symbol.name(),
                  "Implicit declaration of symbol '%s'"_warn_en_US,
                  symbol.name());
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckForImplicitDeclarations(context, child);
  }
}

void CheckImplicitDecl(semantics::SemanticsContext &context) {
  CheckForImplicitDeclarations(context, context.globalScope());
}

} // namespace Fortran::tidy::bugprone
