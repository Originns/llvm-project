#include "AvoidDataConstructs.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

using namespace parser::literals;
void AvoidDataConstructsCheck::Enter(const parser::DataStmt &) {
  if (context()->getSemanticsContext().location().has_value()) {
    Say(
        context()->getSemanticsContext().location().value(),
        "Data statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
