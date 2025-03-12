#include "PrecisionLossCheck.h"
#include "flang/Common/Fortran.h"
#include "flang/Evaluate/expression.h"
#include "flang/Evaluate/type.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"

namespace Fortran::tidy::bugprone {

PrecisionLossCheck::PrecisionLossCheck(llvm::StringRef name,
                                       FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {}

static bool IsLossOfPrecision(evaluate::DynamicType lhs,
                              evaluate::DynamicType rhs) {
  auto lhsCat = lhs.category();
  auto rhsCat = rhs.category();

  // ignore derived types
  if (lhsCat == common::TypeCategory::Derived ||
      rhsCat == common::TypeCategory::Derived)
    return false;

  // this will fail if we call this on a derived type
  int lhsKind = lhs.kind();
  int rhsKind = rhs.kind();

  // integer -> integer, real, complex
  // real -> integer, real, complex
  // complex -> integer, real, complex
  //
  if (lhsCat == rhsCat && lhsKind < rhsKind)
    return true;

  // complex = real are basically real = real
  if (lhsCat == common::TypeCategory::Complex &&
      rhsCat == common::TypeCategory::Real) {
    if (lhsKind < rhsKind)
      return true;
    return false;
  }

  // real = complex loses precision
  if ((lhsCat == common::TypeCategory::Real ||
       lhsCat == common::TypeCategory::Integer) &&
      rhsCat == common::TypeCategory::Complex) {
    return true;
  }

  // integer = real/complex conversions always lose precision
  if (lhsCat == common::TypeCategory::Integer &&
      (rhsCat == common::TypeCategory::Real ||
       rhsCat == common::TypeCategory::Complex)) {
    return true;
  }

  // real/complex = integer conversions always lose precision
  if ((lhsCat == common::TypeCategory::Real ||
       lhsCat == common::TypeCategory::Complex) &&
      rhsCat == common::TypeCategory::Integer && lhsKind <= rhsKind) {
    return true;
  }

  return false;
}

using namespace parser::literals;
void PrecisionLossCheck::Enter(const parser::AssignmentStmt &assignment) {
  const auto &var{std::get<parser::Variable>(assignment.t)};
  const auto &expr{std::get<parser::Expr>(assignment.t)};
  const auto *lhs{semantics::GetExpr(context_->getSemanticsContext(), var)};
  const auto *rhs{semantics::GetExpr(context_->getSemanticsContext(), expr)};

  if (!lhs || !rhs)
    return;

  const auto &lhsType{lhs->GetType()};
  const auto &rhsType{rhs->GetType()};

  if (!lhsType || !rhsType)
    return;

  if (IsLossOfPrecision(*lhsType, *rhsType)) {
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Possible loss of precision in implicit conversion (%s to %s)"_warn_en_US,
        rhsType->AsFortran(), lhsType->AsFortran());
  }
}

} // namespace Fortran::tidy::bugprone
