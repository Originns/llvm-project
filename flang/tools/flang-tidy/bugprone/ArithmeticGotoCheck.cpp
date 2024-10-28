#include "ArithmeticGotoCheck.h"

namespace Fortran::tidy {

ArithmeticGotoCheck::~ArithmeticGotoCheck() {}

using namespace parser::literals;
void ArithmeticGotoCheck::Enter(const parser::ComputedGotoStmt &gotoStmt) {
  if (context_.location().has_value()) {
    context_.Say(context_.location().value(),
                 "Arithmetic goto statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy
