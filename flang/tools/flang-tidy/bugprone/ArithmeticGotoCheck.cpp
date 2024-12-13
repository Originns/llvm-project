#include "ArithmeticGotoCheck.h"

namespace Fortran::tidy::bugprone {

ArithmeticGotoCheck::~ArithmeticGotoCheck() {}

using namespace parser::literals;
void ArithmeticGotoCheck::Enter(const parser::ComputedGotoStmt &gotoStmt) {
  llvm::outs() << "hi there" << "\n";
  if (context_->getSemanticsContext().location().has_value()) {
    llvm::outs() << "hi there 2" << "\n";
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Arithmetic goto statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::bugprone
