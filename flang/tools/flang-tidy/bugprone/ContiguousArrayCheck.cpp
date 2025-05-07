#include "ContiguousArrayCheck.h"
#include "flang/Semantics/symbol.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

void ContiguousArrayCheck::CheckForContinguousArray(
    semantics::SemanticsContext &context, const semantics::Scope &scope) {
  if (scope.IsModuleFile())
    return;

  for (const auto &pair : scope) {
    const semantics::Symbol &symbol{*pair.second};

    if (const auto *details{symbol.detailsIf<semantics::SubprogramDetails>()};
        details && details->isInterface()) {
      for (const auto &dummyArg : details->dummyArgs()) {
        if (!dummyArg)
          continue;

        if (const auto *details{
                dummyArg->detailsIf<semantics::ObjectEntityDetails>()};
            details && details->IsAssumedShape() &&
            !dummyArg->attrs().test(semantics::Attr::CONTIGUOUS)) {
          Say(symbol.name(),
              "assumed-shape array '%s' should be contiguous"_warn_en_US,
              symbol.name());
        }
      }
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckForContinguousArray(context, child);
  }
}

ContiguousArrayCheck::ContiguousArrayCheck(llvm::StringRef name,
                                           FlangTidyContext *context)
    : FlangTidyCheck(name, context) {
  CheckForContinguousArray(context->getSemanticsContext(),
                           context->getSemanticsContext().globalScope());
}

} // namespace Fortran::tidy::bugprone
