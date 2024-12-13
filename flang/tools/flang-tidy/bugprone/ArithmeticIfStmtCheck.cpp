#include "ArithmeticIfStmtCheck.h"

namespace Fortran::tidy::bugprone {

ArithmeticIfStmtCheck::~ArithmeticIfStmtCheck() {}

using namespace parser::literals;
void ArithmeticIfStmtCheck::Enter(const parser::ArithmeticIfStmt &ifStmt) {
  if (context_->getSemanticsContext().location().has_value()) {
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Arithmetic if statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::bugprone
