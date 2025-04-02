#include "ArithmeticGotoCheck.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
void ArithmeticGotoCheck::Enter(const parser::ComputedGotoStmt &gotoStmt) {
  if (context()->getSemanticsContext().location().has_value()) {
    Say(
        context()->getSemanticsContext().location().value(),
        "Arithmetic goto statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::bugprone
