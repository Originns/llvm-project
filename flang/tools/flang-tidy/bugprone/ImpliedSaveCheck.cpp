#include "ImpliedSaveCheck.h"
#include "flang/Evaluate/tools.h"
#include "flang/Semantics/attr.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"

namespace Fortran::tidy::bugprone {

llvm::raw_ostream &operator<<(llvm::raw_ostream &o,
                              const semantics::Symbol::Flags &flags) {
  std::size_t n{flags.count()};
  std::size_t seen{0};
  for (std::size_t j{0}; seen < n; ++j) {
    semantics::Symbol::Flag flag{static_cast<semantics::Symbol::Flag>(j)};
    if (flags.test(flag)) {
      if (seen > 0) {
        o << ", ";
      }
      o << flag;
      ++seen;
    }
  }
  return o;
}

using namespace parser::literals;
static void CheckForImpliedSAVEs(semantics::SemanticsContext &context,
                                 const semantics::Scope &scope) {

  // does the symbol have the SAVE attribute?
  if (scope.IsModuleFile())
    return;

  if (scope.kind() != semantics::Scope::Kind::MainProgram) {
    for (const auto &pair : scope) {
      const semantics::Symbol &symbol = *pair.second;
      const auto &ultimate = symbol.GetUltimate();
      // if the ultimate is from a mod file skip it too
      if (ultimate.IsFromModFile()) {
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
