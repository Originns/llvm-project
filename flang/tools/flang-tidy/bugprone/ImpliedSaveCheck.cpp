#include "ImpliedSaveCheck.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/attr.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"

namespace Fortran::tidy::bugprone {

//ImpliedSaveCheck::ImpliedSaveCheck(llvm::StringRef name,
//                                   FlangTidyContext *context)
//    : FlangTidyCheck{name}, context(){context} {}

using namespace parser::literals;
void ImpliedSaveCheck::Enter(const parser::EntityDecl &entityDecl) {
  const auto &objectName = std::get<parser::ObjectName>(entityDecl.t);
  const auto *symbol = objectName.symbol;
  if (symbol && semantics::IsSaved(*symbol) &&
      !symbol->attrs().test(semantics::Attr::SAVE)) {
    Say(
        symbol->name(), "Implicit SAVE on symbol '%s'"_warn_en_US,
        symbol->name());
  }
}

} // namespace Fortran::tidy::bugprone
