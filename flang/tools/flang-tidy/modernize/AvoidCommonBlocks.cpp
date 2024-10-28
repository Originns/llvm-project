#include "AvoidCommonBlocks.h"

namespace Fortran::tidy {

AvoidCommonBlocksCheck::AvoidCommonBlocksCheck(
    semantics::SemanticsContext &context)
    : context_{context} {}

AvoidCommonBlocksCheck::~AvoidCommonBlocksCheck() {}

using namespace parser::literals;
void AvoidCommonBlocksCheck::Enter(const parser::CommonStmt &common) {
  if (context_.location().has_value()) {
    context_.Say(context_.location().value(),
                 "Common blocks are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy