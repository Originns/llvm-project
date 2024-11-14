#include "UnusedIntentCheck.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
static void CheckUnusedIntentHelper(semantics::SemanticsContext &context,
                                    const semantics::Scope &scope) {
  auto WasDefined{[&context](const semantics::Symbol &symbol) {
    return context.IsSymbolDefined(symbol) ||
           semantics::IsInitialized(symbol, /*ignoreDataStatements=*/false,
                                    /*ignoreAllocatable=*/false,
                                    /*ignorePointer=*/false);
  }};
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    if (!WasDefined(symbol) && semantics::IsIntentInOut(symbol)) {
      context.Say(
          symbol.name(),
          "Variable '%s' with intent(inout) is never defined, consider changing to intent(in)"_warn_en_US,
          symbol.name());
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckUnusedIntentHelper(context, child);
  }
}

void CheckUnusedIntent(semantics::SemanticsContext &context) {
  CheckUnusedIntentHelper(context, context.globalScope());
}

} // namespace Fortran::tidy::bugprone
