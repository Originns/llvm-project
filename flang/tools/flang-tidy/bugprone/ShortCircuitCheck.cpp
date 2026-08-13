//===--- ShortCircuitCheck.cpp - flang-tidy -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "ShortCircuitCheck.h"
#include "flang/Evaluate/traverse.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"
#include <set>
#include <string>

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

using SymbolSet = std::set<const semantics::Symbol *>;

struct OptionalSymbolCollector
    : public evaluate::SetTraverse<OptionalSymbolCollector, SymbolSet> {
  using Base = evaluate::SetTraverse<OptionalSymbolCollector, SymbolSet>;
  OptionalSymbolCollector() : Base{*this} {}
  using Base::operator();

  SymbolSet operator()(const semantics::Symbol &symbol) const {
    if (semantics::IsOptional(symbol)) {
      return SymbolSet{&symbol};
    }
    return SymbolSet{};
  }

  SymbolSet operator()(const evaluate::ProcedureRef &procRef) const {
    const auto &proc = procRef.proc();

    if (auto *intrinsic = proc.GetSpecificIntrinsic()) {
      if (intrinsic->name == "present") {
        return SymbolSet{};
      }
    }

    return Base::operator()(procRef);
  }
};

struct PresentCallCollector
    : public evaluate::SetTraverse<PresentCallCollector, SymbolSet> {
  using Base = evaluate::SetTraverse<PresentCallCollector, SymbolSet>;
  PresentCallCollector() : Base{*this} {}
  using Base::operator();

  SymbolSet operator()(const evaluate::ProcedureRef &procRef) const {
    const auto &proc = procRef.proc();
    if (auto *intrinsic = proc.GetSpecificIntrinsic()) {
      if (intrinsic->name == "present" && procRef.arguments().size() == 1) {
        const auto &arg = procRef.arguments()[0];
        if (arg && arg->UnwrapExpr()) {
          OptionalSymbolCollector argCollector;
          auto argSymbols = argCollector(*arg->UnwrapExpr());
          return argSymbols;
        }
      }
    }
    return SymbolSet{};
  }
};

// Check a single typed logical expression for the pattern
//   present(x) [.AND./.OR.] <use of x>
// and emit a warning for each optional symbol that appears both inside
// present() and as a bare reference, since Fortran does not guarantee
// short-circuit evaluation.
void ShortCircuitCheck::checkExpr(const evaluate::Expr<evaluate::SomeType> &expr,
                                  parser::CharBlock source) {
  OptionalSymbolCollector optionalCollector;
  const auto optionalSymbols = optionalCollector(expr);

  PresentCallCollector presentCollector;
  const auto presentCallSymbols = presentCollector(expr);

  for (const auto *optionalSym : optionalSymbols) {
    if (presentCallSymbols.count(optionalSym)) {
      Say(source,
          "optional argument '%s' used in logical expression alongside "
          "present()"_warn_en_US,
          optionalSym->name());
    }
  }
}

// IF (...) THEN ... END IF
void ShortCircuitCheck::Enter(const parser::IfConstruct &ifConstruct) {
  const auto &ifThenStmt{
      std::get<parser::Statement<parser::IfThenStmt>>(ifConstruct.t)};
  const auto &ex{std::get<parser::ScalarLogicalExpr>(ifThenStmt.statement.t)};
  if (const auto *expr{semantics::GetExpr(context()->getSemanticsContext(), ex)})
    checkExpr(*expr, ifThenStmt.source);
}

// IF (...) single-statement-action
void ShortCircuitCheck::Enter(const parser::IfStmt &ifStmt) {
  const auto &ex{std::get<parser::ScalarLogicalExpr>(ifStmt.t)};
  const auto source{context()->getSemanticsContext().location().value_or(
      parser::CharBlock{})};
  if (const auto *expr{semantics::GetExpr(context()->getSemanticsContext(), ex)})
    checkExpr(*expr, source);
}

// DO WHILE (...)
void ShortCircuitCheck::Enter(const parser::NonLabelDoStmt &doStmt) {
  // LoopControl.u is variant<Bounds, ScalarLogicalExpr, Concurrent>.
  // The ScalarLogicalExpr alternative is the DO WHILE condition.
  const auto &loopControl{std::get<std::optional<parser::LoopControl>>(doStmt.t)};
  if (!loopControl)
    return;
  const auto *whileCond{
      std::get_if<parser::ScalarLogicalExpr>(&loopControl->u)};
  if (!whileCond)
    return;
  const auto source{context()->getSemanticsContext().location().value_or(
      parser::CharBlock{})};
  if (const auto *expr{semantics::GetExpr(context()->getSemanticsContext(),
                                          *whileCond)})
    checkExpr(*expr, source);
}

} // namespace Fortran::tidy::bugprone
