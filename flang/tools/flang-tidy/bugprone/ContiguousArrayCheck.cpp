#include "ContiguousArrayCheck.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

void ContiguousArrayCheck::CheckForContinguousArray(
    semantics::SemanticsContext &context, const semantics::Scope &scope) {
  if (scope.IsModuleFile())
    return;

  // if there is a symbol that is inferred shape and doesnt have contiguous,
  // warn about it
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol{*pair.second};
    // does it have object entity details?
    if (const auto *details{
            symbol.detailsIf<semantics::ObjectEntityDetails>()}) {
      // is it an array?
      if (details->IsAssumedShape() &&
          !symbol.attrs().test(semantics::Attr::CONTIGUOUS)) {
        Say(symbol.name(),
            "assumed-shape array '%s' should be contiguous"_warn_en_US,
            symbol.name());
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
