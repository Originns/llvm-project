#include "AvoidPauseStmt.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::modernize {

AvoidPauseStmtCheck::AvoidPauseStmtCheck(llvm::StringRef name,
                                         FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {}

using namespace parser::literals;
void AvoidPauseStmtCheck::Enter(const parser::PauseStmt &) {
  if (context_->getSemanticsContext().location().has_value()) {
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Pause statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
