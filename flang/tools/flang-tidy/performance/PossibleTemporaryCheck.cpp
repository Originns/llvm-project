//===--- PossibleTemporaryCheck.cpp - flang-tidy --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "PossibleTemporaryCheck.h"
#include "flang/Evaluate/check-expression.h"
#include "flang/Evaluate/type.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::performance {

using namespace parser::literals;
void PossibleTemporaryCheck::Enter(const parser::CallStmt &callStmt) {
  const auto *procedureRef = callStmt.typedCall.get();
  if (procedureRef) {
    // if the proc is implicit, all arrays are passed as contiguous
    if (!procedureRef->proc().GetInterfaceSymbol()) {
      return;
    }

    int j{0};
    for (const auto &arg : procedureRef->arguments()) {
      ++j;
      if (!arg)
        continue;

      if (const semantics::SomeExpr *argExpr{arg->UnwrapExpr()}) {
        // is the expression a whole symbol data ref or a base symbol data ref
        const auto *var{evaluate::GetFirstSymbol(*argExpr)};
        if (!var)
          continue;

        if (var->Rank() == 0) // is array?
          continue;

        // get the proc interface
        const auto *procInterface = procedureRef->proc().GetInterfaceSymbol();
        if (!procInterface)
          continue;

        const auto *details =
            procInterface->detailsIf<semantics::SubprogramDetails>();
        if (!details)
          continue;

        // get the dummy arg
        const auto *dummy = details->dummyArgs()[j - 1];
        if (!dummy)
          continue;

        const auto *shape = dummy->GetShape();
        if (!shape)
          continue;

        if (shape->IsAssumedRank() &&
            !dummy->attrs().test(semantics::Attr::CONTIGUOUS))
          continue;

        // if its deferred shape, we cannot say anything
        if (!evaluate::IsSimplyContiguous(
                *dummy, context()->getSemanticsContext().foldingContext()))
          continue;

        // warn
        Say(*arg->sourceLocation(),
            "Argument may be passed as a temporary"_warn_en_US)
            .Attach(parser::Message(
                dummy->name(),
                "Dummy argument '%s' is contiguous"_because_en_US,
                dummy->name()));

        /*
         * const bool mustDoCopyInOut =
             actual.isArray() && arg.mustBeMadeContiguous() &&
             (passingPolymorphicToNonPolymorphic ||
              !isSimplyContiguous(*arg.entity, foldingContext));

              const auto &shapeAttrs = dummy->type.attrs();
              using ShapeAttrs =
         Fortran::evaluate::characteristics::TypeAndShape::Attr; if
         (shapeAttrs.test(ShapeAttrs::AssumedRank) ||
                  shapeAttrs.test(ShapeAttrs::AssumedShape))
                return dummy->attrs.test(
                    Fortran::evaluate::characteristics::DummyDataObject::Attr::Contiguous);
              if (shapeAttrs.test(ShapeAttrs::DeferredShape))
                return false;
              // Explicit shape arrays are contiguous.
              return dummy->type.Rank() > 0;
         */
      }
    }
  }
}

} // namespace Fortran::tidy::performance
