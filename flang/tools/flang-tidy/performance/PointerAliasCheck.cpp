//===--- PointerAliasCheck.cpp - flang-tidy -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "PointerAliasCheck.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree-visitor.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include <vector>

namespace Fortran::tidy::performance {

namespace {

/// Gathers the assignment statements of a loop body, including those in nested
/// constructs, so that the whole loop nest is considered.
struct AssignmentCollector {
  template <typename T>
  bool Pre(const T &) {
    return true;
  }
  template <typename T>
  void Post(const T &) {}

  bool Pre(const parser::AssignmentStmt &stmt) {
    assignments.push_back(&stmt);
    return true;
  }

  std::vector<const parser::AssignmentStmt *> assignments;
};

/// An array pointer is the only kind of entity the compiler must assume may
/// overlap with another one here.
bool IsArrayPointer(const semantics::Symbol &symbol) {
  const semantics::Symbol &ultimate{symbol.GetUltimate()};
  return ultimate.attrs().test(semantics::Attr::POINTER) &&
         ultimate.Rank() > 0;
}

} // namespace

using namespace parser::literals;

void PointerAliasCheck::Enter(const parser::DoConstruct &doConstruct) {
  // DO CONCURRENT already asserts that the iterations are independent.
  if (doConstruct.IsDoConcurrent())
    return;

  AssignmentCollector collector;
  parser::Walk(std::get<parser::Block>(doConstruct.t), collector);

  auto &semanticsContext{context()->getSemanticsContext()};
  for (const parser::AssignmentStmt *stmt : collector.assignments) {
    const auto &[variable, expression] = stmt->t;
    const auto *lhs{semantics::GetExpr(semanticsContext, variable)};
    const auto *rhs{semantics::GetExpr(semanticsContext, expression)};
    if (!lhs || !rhs)
      continue;

    const semantics::Symbol *target{evaluate::GetFirstSymbol(*lhs)};
    if (!target || !IsArrayPointer(*target))
      continue;

    parser::CharBlock at{variable.GetSource()};
    const semantics::Symbol &targetUltimate{target->GetUltimate()};

    for (const semantics::SymbolRef &ref : evaluate::GetSymbolVector(*rhs)) {
      const semantics::Symbol &source{ref->GetUltimate()};
      if (&source == &targetUltimate || !IsArrayPointer(source))
        continue;
      if (!reported_.emplace(at.begin(), &source).second)
        continue;

      Say(at,
          "Pointer '%s' is assigned and pointer '%s' is read in the same loop; they may alias"_warn_en_US,
          targetUltimate.name().ToString(), source.name().ToString())
          .Attach(source.name(),
              "'%s' is declared POINTER, so it is not known to be disjoint from '%s'"_because_en_US,
              source.name().ToString(), targetUltimate.name().ToString());
    }
  }
}

} // namespace Fortran::tidy::performance
