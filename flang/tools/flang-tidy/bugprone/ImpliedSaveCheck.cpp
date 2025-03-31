#include "ImpliedSaveCheck.h"
#include "flang/Evaluate/tools.h"
#include "flang/Semantics/attr.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"
#include "utils/SymbolUtils.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
static void CheckForImpliedSAVEs(semantics::SemanticsContext &context,
                                 const semantics::Scope &scope) {
  if (scope.IsModuleFile())
    return;

  if (scope.kind() != semantics::Scope::Kind::MainProgram) {
    for (const auto &pair : scope) {
      const semantics::Symbol &symbol = *pair.second;
      const auto &ultimate = symbol.GetUltimate();

      // if the symbol is a procedure, skip it
      if (semantics::IsProcedure(symbol)) {
        continue;
      }

      // if the ultimate is from a mod file skip it too
      if (utils::IsFromModFileSafe(ultimate)) {
        continue;
      }

      // implicit save
      if (semantics::IsSaved(symbol) &&
          !symbol.attrs().test(semantics::Attr::SAVE)) {
        context.Say(symbol.name(), "Implicit SAVE on symbol '%s'"_warn_en_US,
                    symbol.name());
      }
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckForImpliedSAVEs(context, child);
  }
}

ImpliedSaveCheck::ImpliedSaveCheck(llvm::StringRef name,
                                   FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {
  CheckForImpliedSAVEs(context_->getSemanticsContext(),
                       context_->getSemanticsContext().globalScope());
}

} // namespace Fortran::tidy::bugprone
