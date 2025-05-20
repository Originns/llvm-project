#include "ArithmeticIfStmtCheck.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
void ArithmeticIfStmtCheck::Enter(const parser::ArithmeticIfStmt &ifStmt) {
  if (context()->getSemanticsContext().location().has_value()) {
    Say(
        context()->getSemanticsContext().location().value(),
        "Arithmetic if statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::bugprone
