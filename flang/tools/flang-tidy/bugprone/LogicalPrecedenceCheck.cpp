//===--- LogicalPrecedenceCheck.cpp - flang-tidy --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "LogicalPrecedenceCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
void LogicalPrecedenceCheck::Enter(const parser::Expr &expr) {
  // We only care about Logical OR operations at the top level of this check
  const auto *orExpr{std::get_if<parser::Expr::OR>(&expr.u)};
  if (!orExpr) {
    return;
  }

  // Helper to check if a child expression is an unwrapped AND
  auto IsUnwrappedAnd =
      [](const parser::Expr &childExpr) -> const parser::Expr::AND * {
    // If the child is directly an AND, it means there are no parentheses
    // separating it from the parent OR.
    return std::get_if<parser::Expr::AND>(&childExpr.u);
  };

  const parser::Expr &lhs{std::get<0>(orExpr->t).value()};
  const parser::Expr &rhs{std::get<1>(orExpr->t).value()};

  // Diagnose: (a .AND. b) .OR. c  => binds as .AND. first
  if (const auto *lhsAnd = IsUnwrappedAnd(lhs); lhsAnd) {
    Say(lhs.source,
        "possible precedence confusion: '.AND.' binds tighter than '.OR.';"
        " use parentheses to clarify"_warn_en_US);
  }

  // Diagnose: a .OR. (b .AND. c)  => binds as .AND. first
  if (const auto *rhsAnd = IsUnwrappedAnd(rhs); rhsAnd) {
    Say(rhs.source,
        "possible precedence confusion: '.AND.' binds tighter than '.OR.';"
        " use parentheses to clarify"_warn_en_US);
  }
}

} // namespace Fortran::tidy::bugprone
