//===--- ExpmOneCheck.cpp - flang-tidy ------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ExpmOneCheck.h"
#include "flang/Evaluate/check-expression.h"
#include "flang/Evaluate/expression.h"
#include "flang/Evaluate/fold.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/tools.h"
#include "llvm/ADT/StringRef.h"
#include <cstdint>
#include <cstring>

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

// ---------------------------------------------------------------------------
// Option parsing helpers
// ---------------------------------------------------------------------------

/// Parse \p s as a double; return \p fallback on failure.
static double parseDouble(llvm::StringRef s, double fallback) {
  double v{};
  if (!s.getAsDouble(v))
    return v;
  return fallback;
}

// ---------------------------------------------------------------------------
// Constructor / options
// ---------------------------------------------------------------------------

ExpmOneCheck::ExpmOneCheck(llvm::StringRef Name, FlangTidyContext *Context)
    : FlangTidyCheck(Name, Context),
      WarnThreshold(parseDouble(
          Options.get("WarnThreshold",
                      llvm::StringRef{std::to_string(DefaultWarnThreshold)}),
          DefaultWarnThreshold)) {}

void ExpmOneCheck::storeOptions(FlangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "WarnThreshold",
                llvm::StringRef{std::to_string(WarnThreshold)});
}

namespace {

// ---------------------------------------------------------------------------
// Parse-tree helpers
// ---------------------------------------------------------------------------

/// If \p expr is a call to the \c exp intrinsic (case-insensitive), return a
/// pointer to its first argument expression.  Otherwise return nullptr.
static const parser::Expr *getExpArg(const parser::Expr &expr) {
  const auto *funcRef =
      std::get_if<common::Indirection<parser::FunctionReference>>(&expr.u);
  if (!funcRef)
    return nullptr;

  const parser::Call &call = funcRef->value().v;
  const parser::ProcedureDesignator &proc =
      std::get<parser::ProcedureDesignator>(call.t);
  const parser::Name *name = std::get_if<parser::Name>(&proc.u);
  if (!name)
    return nullptr;

  if (!llvm::StringRef{name->source.begin(), name->source.size()}
           .equals_insensitive("exp"))
    return nullptr;

  const auto &argList = std::get<std::list<parser::ActualArgSpec>>(call.t);
  if (argList.empty())
    return nullptr;

  const parser::ActualArg &firstArg =
      std::get<parser::ActualArg>(argList.front().t);
  const auto *exprPtr =
      std::get_if<common::Indirection<parser::Expr>>(&firstArg.u);
  return exprPtr ? &exprPtr->value() : nullptr;
}

// ---------------------------------------------------------------------------
// Typed-expression helpers
// ---------------------------------------------------------------------------

/// Check whether a real-typed constant is ±1 (sign = +1 or -1).
template <int KIND>
static bool realConstantIsOne(
    const evaluate::Expr<evaluate::Type<evaluate::TypeCategory::Real, KIND>>
        &expr,
    int sign) {
  using T = evaluate::Type<evaluate::TypeCategory::Real, KIND>;
  auto scalar = evaluate::GetScalarConstantValue<T>(expr);
  if (!scalar)
    return false;
  using Int64 = evaluate::value::Integer<64>;
  auto ref = evaluate::Scalar<T>::FromInteger(Int64{sign}).value;
  return scalar->Compare(ref) == evaluate::Relation::Equal;
}

/// Return true when the typed expression is a real or integer constant whose
/// value equals \p sign (expected to be +1 or -1).
static bool isConstantOne(const evaluate::Expr<evaluate::SomeType> &expr,
                          int sign) {
  // --- real ---
  if (const auto *re =
          std::get_if<evaluate::Expr<evaluate::SomeReal>>(&expr.u)) {
    return std::visit(
        [sign](const auto &e) { return realConstantIsOne(e, sign); }, re->u);
  }
  // --- integer (handles implicit integer 1 on the RHS before promotion) ---
  if (const auto *ie =
          std::get_if<evaluate::Expr<evaluate::SomeInteger>>(&expr.u)) {
    return std::visit(
        [sign](const auto &e) -> bool {
          using T = typename std::decay_t<decltype(e)>::Result;
          auto scalar = evaluate::GetScalarConstantValue<T>(e);
          if (!scalar)
            return false;
          return scalar->ToInt64() == sign;
        },
        ie->u);
  }
  return false;
}

/// Build a Real<Integer<64>,53> (IEEE double) from a host \c double value.
static evaluate::value::Real<evaluate::value::Integer<64>, 53>
toRealDouble(double v) {
  std::uint64_t bits;
  std::memcpy(&bits, &v, sizeof(bits));
  return evaluate::value::Real<evaluate::value::Integer<64>, 53>{
      evaluate::value::Integer<64>{static_cast<std::int64_t>(bits)}};
}

/// Return true when the argument to \c exp() should trigger a warning.
///
/// Rules:
///  - Non-constant expression: always warn (value may be near zero at runtime).
///  - Constant real: warn when \c |x| < \p threshold.
///  - Constant integer: warn only for zero (no precision loss for other ints).
static bool shouldWarnForExpArg(const evaluate::Expr<evaluate::SomeType> &arg,
                                double threshold) {
  // Non-constant: always warn.
  if (!evaluate::IsConstantExpr(arg))
    return true;

  // Constant real: warn when |x| < threshold.
  if (const auto *re =
          std::get_if<evaluate::Expr<evaluate::SomeReal>>(&arg.u)) {
    return std::visit(
        [threshold](const auto &e) -> bool {
          using T = typename std::decay_t<decltype(e)>::Result;
          auto scalar = evaluate::GetScalarConstantValue<T>(e);
          if (!scalar)
            return false; // folded but not constant? be conservative
          if (scalar->IsZero())
            return true;

          // Build threshold in the same Real type as the scalar by first
          // representing it as IEEE double and then converting.
          auto thresholdD = toRealDouble(threshold);
          auto thresholdT = evaluate::Scalar<T>::Convert(thresholdD).value;

          // Warn if |x| < threshold.
          return scalar->ABS().Compare(thresholdT) == evaluate::Relation::Less;
        },
        re->u);
  }

  // Constant integer: only zero is "near zero".
  if (const auto *ie =
          std::get_if<evaluate::Expr<evaluate::SomeInteger>>(&arg.u)) {
    return std::visit(
        [](const auto &e) -> bool {
          using T = typename std::decay_t<decltype(e)>::Result;
          auto scalar = evaluate::GetScalarConstantValue<T>(e);
          return scalar && scalar->IsZero();
        },
        ie->u);
  }

  return false; // complex constants etc. — skip
}

} // namespace

// ---------------------------------------------------------------------------
// Check hooks
// ---------------------------------------------------------------------------

// Matches:  exp(x) - 1
void ExpmOneCheck::Enter(const parser::Expr::Subtract &sub) {
  const parser::Expr &lhs = std::get<0>(sub.t).value();
  const parser::Expr &rhs = std::get<1>(sub.t).value();

  const parser::Expr *expArg = getExpArg(lhs);
  if (!expArg)
    return;

  const auto *typedRhs =
      semantics::GetExpr(context()->getSemanticsContext(), rhs);
  if (!typedRhs || !isConstantOne(*typedRhs, +1))
    return;

  const auto *typedArg =
      semantics::GetExpr(context()->getSemanticsContext(), *expArg);
  if (!typedArg || !shouldWarnForExpArg(*typedArg, WarnThreshold))
    return;

  Say(lhs.source, "Expression 'exp(x)-1' loses precision near x=0; "
                  "consider using 'expm1(x)'"_warn_en_US);
}

// Matches:  -1 + exp(x)   and   exp(x) + (-1)
void ExpmOneCheck::Enter(const parser::Expr::Add &add) {
  const parser::Expr &lhs = std::get<0>(add.t).value();
  const parser::Expr &rhs = std::get<1>(add.t).value();

  // Helper: given the exp-call side and the ±1 side, check and warn.
  auto check = [&](const parser::Expr &expSide,
                   const parser::Expr &oneSide) -> bool {
    const parser::Expr *expArg = getExpArg(expSide);
    if (!expArg)
      return false;
    const auto *typedOne =
        semantics::GetExpr(context()->getSemanticsContext(), oneSide);
    if (!typedOne || !isConstantOne(*typedOne, -1))
      return false;
    const auto *typedArg =
        semantics::GetExpr(context()->getSemanticsContext(), *expArg);
    if (!typedArg || !shouldWarnForExpArg(*typedArg, WarnThreshold))
      return false;
    Say(expSide.source, "Expression 'exp(x)+(-1)' loses precision near x=0; "
                        "consider using 'expm1(x)'"_warn_en_US);
    return true;
  };

  if (!check(lhs, rhs))
    check(rhs, lhs);
}

} // namespace Fortran::tidy::bugprone
