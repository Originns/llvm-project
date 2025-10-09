//===--- PointerTemporaryCheck.cpp - flang-tidy ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "PointerTemporaryCheck.h"
#include "flang/Evaluate/check-expression.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/tools.h"
#include <cstddef>
#include <variant>

namespace Fortran::tidy::bugprone {

using namespace parser::literals;
void PointerTemporaryCheck::Leave(
    const parser::PointerAssignmentStmt &pointerAssignment) {
  const auto &[dataRef, _, expr] = pointerAssignment.t;
  const auto *name = std::get_if<parser::Name>(&dataRef.u);
  const auto *rhsExpr{
      semantics::GetExpr(context()->getSemanticsContext(), expr)};

  if (!rhsExpr || !name)
    return;

  const auto *lhsSymbol = name->symbol;
  const auto *rhsSymbol = evaluate::UnwrapWholeSymbolDataRef(*rhsExpr);

  // lhs dummy array pointer with intent(inout/out) & rhs contiguous
  if (lhsSymbol && rhsSymbol && semantics::IsDummy(*lhsSymbol) &&
      lhsSymbol->GetShape() /* is it an array? */ &&
      lhsSymbol->attrs().test(semantics::Attr::POINTER) &&
      !lhsSymbol->attrs().HasAny(
          {semantics::Attr::INTENT_IN, semantics::Attr::VALUE}) &&
      evaluate::IsContiguous(
          *rhsSymbol, context()->getSemanticsContext().foldingContext())) {
    Say(context()->getSemanticsContext().location().value(),
        "Pointer dummy argument '%s' may become associated with a "
        "contiguous target, which may be a temporary"_warn_en_US,
        lhsSymbol->name());
  }
}

} // namespace Fortran::tidy::bugprone
