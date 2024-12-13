#include "AvoidBackspaceStmt.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::modernize {

AvoidBackspaceStmtCheck::AvoidBackspaceStmtCheck(llvm::StringRef name,
                                                 FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {}

AvoidBackspaceStmtCheck::~AvoidBackspaceStmtCheck() {}

using namespace parser::literals;
void AvoidBackspaceStmtCheck::Enter(const parser::BackspaceStmt &) {
  if (context_->getSemanticsContext().location().has_value()) {
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Assign statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
