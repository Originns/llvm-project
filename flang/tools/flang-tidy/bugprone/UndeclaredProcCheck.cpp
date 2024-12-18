#include "UndeclaredProcCheck.h"
#include "flang/Semantics/symbol.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
static void CheckForUndeclaredProcedures(semantics::SemanticsContext &context,
                                         const semantics::Scope &scope) {
  if (scope.IsModuleFile())
    return;

  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    if (auto *details{symbol.detailsIf<semantics::ProcEntityDetails>()};
        details) {
      // Unknown global external procedure
      if (symbol.owner().IsGlobal()) {
        context.Say(symbol.name(),
                    "Implicit declaration of procedure '%s'"_warn_en_US,
                    symbol.name());
      } else if (!details->HasExplicitInterface()) { // Procedure with no
                                                     // explicit interface
        context.Say(symbol.name(),
                    "Procedure '%s' has no explicit interface"_warn_en_US,
                    symbol.name());
      }
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckForUndeclaredProcedures(context, child);
  }
}

UndeclaredProcCheck::UndeclaredProcCheck(llvm::StringRef name,
                                         FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {
  CheckForUndeclaredProcedures(context_->getSemanticsContext(),
                               context_->getSemanticsContext().globalScope());
}

} // namespace Fortran::tidy::bugprone
