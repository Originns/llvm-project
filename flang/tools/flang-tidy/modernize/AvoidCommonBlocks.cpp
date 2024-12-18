#include "AvoidCommonBlocks.h"
#include "FlangTidyCheck.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::modernize {

AvoidCommonBlocksCheck::AvoidCommonBlocksCheck(llvm::StringRef name,
                                               FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {}

using namespace parser::literals;
void AvoidCommonBlocksCheck::Enter(const parser::CommonStmt &) {
  if (context_->getSemanticsContext().location().has_value()) {
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Common blocks are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
