#include "UndeclaredProcCheck.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
static void CheckForUndeclaredProcedures(semantics::SemanticsContext &context,
                                         const semantics::Scope &scope) {
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    if (symbol.has<semantics::ProcEntityDetails>() &&
        symbol.owner().IsGlobal()) {
      // Unknown global external, implicit interface;
      context.Say(symbol.name(),
                  "Implicit declaration of procedure '%s'"_warn_en_US,
                  symbol.name());
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckForUndeclaredProcedures(context, child);
  }
}

void CheckUndeclaredProc(semantics::SemanticsContext &context) {
  CheckForUndeclaredProcedures(context, context.globalScope());
}

} // namespace Fortran::tidy::bugprone
