#include "ImplicitDeclCheck.h"

namespace Fortran::tidy {

using namespace parser::literals;
void CheckForImplicitDeclarations(semantics::SemanticsContext &ctx,
                                  const semantics::Scope &scope) {
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    // is the symbol implicit?
    if (symbol.test(semantics::Symbol::Flag::Implicit)) {
      // warn about it
      ctx.messages().Say(symbol.name(),
                         "Implicit declaration of symbol '%s'"_warn_en_US,
                         symbol.name());
    }
  }

  // check its children
  for (const semantics::Scope &child : scope.children()) {
    CheckForImplicitDeclarations(ctx, child);
  }
}

void CheckImplicitDecl(semantics::SemanticsContext &context) {
  CheckForImplicitDeclarations(context, context.globalScope());
}

} // namespace Fortran::tidy
