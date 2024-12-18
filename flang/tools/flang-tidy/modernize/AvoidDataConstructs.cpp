#include "AvoidDataConstructs.h"
#include "flang/Parser/parse-tree.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::modernize {

AvoidDataConstructsCheck::AvoidDataConstructsCheck(llvm::StringRef name,
                                                   FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {}

using namespace parser::literals;
void AvoidDataConstructsCheck::Enter(const parser::DataStmt &) {
  if (context_->getSemanticsContext().location().has_value()) {
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Data statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
