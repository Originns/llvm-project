#include "AvoidAssignStmt.h"
#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

AvoidAssignStmtCheck::AvoidAssignStmtCheck(llvm::StringRef name,
                                           FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {}

using namespace parser::literals;
void AvoidAssignStmtCheck::Enter(const parser::AssignStmt &) {
  if (context_->getSemanticsContext().location().has_value()) {
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Assign statements are not recommended"_warn_en_US);
  }
}

void AvoidAssignStmtCheck::Enter(const parser::AssignedGotoStmt &) {
  if (context_->getSemanticsContext().location().has_value()) {
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Assigned Goto statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
