#include "AvoidBackspaceStmt.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

using namespace parser::literals;
void AvoidBackspaceStmtCheck::Enter(const parser::BackspaceStmt &) {
  if (context()->getSemanticsContext().location().has_value()) {
    Say(
        context()->getSemanticsContext().location().value(),
        "Assign statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
