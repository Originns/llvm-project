#include "AvoidCommonBlocks.h"

namespace Fortran::tidy::modernize {

using namespace parser::literals;
void AvoidCommonBlocksCheck::Enter(const parser::CommonStmt &) {
  if (context()->getSemanticsContext().location().has_value()) {
    Say(
        context()->getSemanticsContext().location().value(),
        "Common blocks are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
