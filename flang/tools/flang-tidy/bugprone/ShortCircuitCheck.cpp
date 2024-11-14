#include "ShortCircuitCheck.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::bugprone {

ShortCircuitCheck::ShortCircuitCheck(semantics::SemanticsContext &context)
    : context_{context} {}

ShortCircuitCheck::~ShortCircuitCheck() {}

using namespace parser::literals;
void ShortCircuitCheck::Enter(const parser::IfThenStmt &ifStmt) {
  const auto &logicalExpr = std::get<parser::ScalarLogicalExpr>(ifStmt.t);
  const auto &expr = semantics::GetExpr(logicalExpr);
  (void)expr;
  (void)context_;

  // we want to iterate this subtree and find out if we have some sort of memory
  // access e.g. array access, pointer access, etc. (e.g. we could collect all
  // designators and procedure references ?)
}

} // namespace Fortran::tidy::bugprone
