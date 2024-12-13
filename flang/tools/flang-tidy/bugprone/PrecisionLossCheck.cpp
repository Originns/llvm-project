#include "PrecisionLossCheck.h"
#include "flang/Evaluate/expression.h"
#include "flang/Evaluate/type.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"

namespace Fortran::tidy::bugprone {

PrecisionLossCheck::PrecisionLossCheck(llvm::StringRef name,
                                       FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {}

PrecisionLossCheck::~PrecisionLossCheck() {}

using namespace parser::literals;
void PrecisionLossCheck::Enter(const parser::AssignmentStmt &assignment) {
  const auto &var{std::get<parser::Variable>(assignment.t)};
  const auto &expr{std::get<parser::Expr>(assignment.t)};
  const auto *lhs{semantics::GetExpr(context_->getSemanticsContext(), var)};
  const auto *rhs{semantics::GetExpr(context_->getSemanticsContext(), expr)};
  if (lhs && rhs) {
    const auto &lhsType{lhs->GetType()};
    const auto &rhsType{rhs->GetType()};
    if (lhsType && rhsType) {
      if (lhsType->category() != common::TypeCategory::Derived &&
          lhsType->category() == rhsType->category()) {
        if (lhsType->kind() < rhsType->kind()) {
          context_->getSemanticsContext().Say(
              context_->getSemanticsContext().location().value(),
              "Possible loss of precision in implicit conversion (%s to %s)"_warn_en_US,
              rhsType->AsFortran(), lhsType->AsFortran());
        }
      }
    }
  }
}

} // namespace Fortran::tidy::bugprone
