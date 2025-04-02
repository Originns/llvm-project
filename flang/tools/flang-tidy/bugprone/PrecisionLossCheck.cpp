#include "PrecisionLossCheck.h"
#include "flang/Common/Fortran.h"
#include "flang/Evaluate/expression.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"
#include <clang/Basic/SourceLocation.h>
#include <variant>

namespace Fortran::tidy::bugprone {

// PrecisionLossCheck::PrecisionLossCheck(llvm::StringRef name,
//                                        FlangTidyContext *context)
//     : FlangTidyCheck{name}, context(){context} {}

static bool IsLossOfPrecision(const semantics::SomeExpr *lhs,
                              const semantics::SomeExpr *rhs,
                              bool hasExplicitKind, bool isConstantReal) {

  const auto &lhsType{lhs->GetType()};
  const auto &rhsType{rhs->GetType()};

  if (!lhsType || !rhsType)
    return false;

  auto lhsCat = lhsType->category();
  auto rhsCat = lhsType->category();

  // ignore derived types
  if (lhsCat == common::TypeCategory::Derived ||
      rhsCat == common::TypeCategory::Derived)
    return false;

  // this will fail if we call this on a derived type
  int lhsKind = lhsType->kind();
  int rhsKind = rhsType->kind();

  // integer -> integer, real, complex
  // real -> integer, real, complex
  // complex -> integer, real, complex
  //
  if (lhsCat == rhsCat && lhsKind < rhsKind)
    return true;

  // is the rhs is a literal and lhs has larger kind than the rhs
  if (isConstantReal && lhsKind > rhsKind && !hasExplicitKind) {
    return true;
  }

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
  const auto *lhs{semantics::GetExpr(context()->getSemanticsContext(), var)};
  const auto *rhs{semantics::GetExpr(context()->getSemanticsContext(), expr)};

  if (!lhs || !rhs)
    return;

  bool hasExplicitKind = false;
  bool isConstantReal = false;
  // if the expr is LiteralConstant, we can get the value
  if (std::holds_alternative<parser::LiteralConstant>(expr.u)) {
    const auto &literal = std::get<parser::LiteralConstant>(expr.u);
    // if it holds a RealLiteralConstant, we can get the value
    if (std::holds_alternative<parser::RealLiteralConstant>(literal.u)) {
      const auto &realLiteral =
          std::get<parser::RealLiteralConstant>(literal.u);
      hasExplicitKind = realLiteral.kind.has_value();
      isConstantReal = true;
    }
  }

  if (IsLossOfPrecision(lhs, rhs, hasExplicitKind, isConstantReal)) {
    Say(context()->getSemanticsContext().location().value(),
        "Possible loss of precision in implicit conversion (%s to %s) "_warn_en_US,
        rhs->GetType()->AsFortran(), lhs->GetType()->AsFortran());
  }
}

} // namespace Fortran::tidy::bugprone
