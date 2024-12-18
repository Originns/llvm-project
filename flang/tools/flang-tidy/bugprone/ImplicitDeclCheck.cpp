#include "ImplicitDeclCheck.h"
#include "FlangTidyCheck.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
static void CheckForImplicitDeclarations(semantics::SemanticsContext &context,
                                         const semantics::Scope &scope) {
  if (scope.IsModuleFile())
    return;

  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    if (symbol.test(semantics::Symbol::Flag::Implicit) &&
        !symbol.test(semantics::Symbol::Flag::Function) &&
        !symbol.test(semantics::Symbol::Flag::Subroutine)) {
      context.Say(symbol.name(),
                  "Implicit declaration of symbol '%s'"_warn_en_US,
                  symbol.name());
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckForImplicitDeclarations(context, child);
  }
}

ImplicitDeclCheck::ImplicitDeclCheck(llvm::StringRef name,
                                     FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {
  CheckForImplicitDeclarations(context_->getSemanticsContext(),
                               context_->getSemanticsContext().globalScope());
}

} // namespace Fortran::tidy::bugprone
