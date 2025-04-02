#include "AvoidPauseStmt.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

using namespace parser::literals;
void AvoidPauseStmtCheck::Enter(const parser::PauseStmt &) {
  if (context()->getSemanticsContext().location().has_value()) {
    Say(
        context()->getSemanticsContext().location().value(),
        "Pause statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
