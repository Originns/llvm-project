#include "UnusedIntentCheck.h"
#include "flang/Semantics/attr.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
static void CheckUnusedIntentHelper(semantics::SemanticsContext &context,
                                    const semantics::Scope &scope) {
  if (scope.IsModuleFile())
    return;

  auto WasDefined{[&context](const semantics::Symbol &symbol) {
    return context.IsSymbolDefined(symbol) ||
           semantics::IsInitialized(symbol, /*ignoreDataStatements=*/false,
                                    /*ignoreAllocatable=*/false,
                                    /*ignorePointer=*/false);
  }};
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    // is the symbol a function argument
    if (const auto *details{symbol.detailsIf<semantics::ObjectEntityDetails>()};
        details && details->isDummy()) {
      if (!WasDefined(symbol) && semantics::IsIntentInOut(symbol)) {
        context.Say(
            symbol.name(),
            "Variable '%s' with intent(inout) is never defined, consider changing to intent(in)"_warn_en_US,
            symbol.name());
      }
      if (!symbol.attrs().HasAny({semantics::Attr::INTENT_IN,
                                  semantics::Attr::INTENT_INOUT,
                                  semantics::Attr::INTENT_OUT})) {
        // warn about dummy arguments without explicit intent
        context.Say(symbol.name(),
                    "Dummy argument '%s' has no explicit intent"_warn_en_US,
                    symbol.name());
      }
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckUnusedIntentHelper(context, child);
  }
}

UnusedIntentCheck::UnusedIntentCheck(llvm::StringRef name,
                                     FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {
  CheckUnusedIntentHelper(context_->getSemanticsContext(),
                          context_->getSemanticsContext().globalScope());
}

} // namespace Fortran::tidy::bugprone
