#include "UninitializedVarCheck.h"
#include "flang/Common/Fortran.h"
#include "flang/Evaluate/call.h"
#include "flang/Evaluate/tools.h"
#include "flang/Evaluate/variable.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"
#include "utils/CollectActualArguments.h"
#include <variant>

namespace Fortran::tidy::bugprone {

UninitializedVarCheck::UninitializedVarCheck(
    semantics::SemanticsContext &context)
    : context_{context} {}

UninitializedVarCheck::~UninitializedVarCheck() {}

// check AssignmentStmts
void UninitializedVarCheck::Leave(const parser::AssignmentStmt &assignment) {
  const auto &var = std::get<parser::Variable>(assignment.t);
  const auto *lhs = semantics::GetExpr(context_, var);
  if (lhs) {
    if (const semantics::Symbol * base{evaluate::GetFirstSymbol(*lhs)}; base) {
      // initialize the base symbol
      definedVars_.insert(*base);
    }
  }
}

// check PointerAssignmentStmts
void UninitializedVarCheck::Leave(
    const parser::PointerAssignmentStmt &ptrAssignmentStmt) {
  const auto *typedAssignment = semantics::GetAssignment(ptrAssignmentStmt);
  if (typedAssignment) {
    if (const semantics::Symbol *
        base{evaluate::GetFirstSymbol(typedAssignment->lhs)}) {
      definedVars_.insert(*base);
    }
  }
}

// check DoConstructs
void UninitializedVarCheck::Enter(const parser::DoConstruct &doConstruct) {
  if (doConstruct.GetLoopControl()) {
    if (const auto *bounds{std::get_if<parser::LoopControl::Bounds>(
            &doConstruct.GetLoopControl().value().u)}) {
      const auto *symbol = bounds->name.thing.symbol;
      if (symbol) {
        definedVars_.insert(*symbol);
      }
    }
  }
}

// check AllocateStmts
void UninitializedVarCheck::Leave(const parser::AllocateStmt &allocateStmt) {
  // get the list of Allocations
  const auto &allocations =
      std::get<std::list<parser::Allocation>>(allocateStmt.t);

  // for each allocation, get the AllocateObject
  for (const auto &allocation : allocations) {
    const auto &allocateObject = std::get<parser::AllocateObject>(allocation.t);
    const auto *expr = semantics::GetExpr(allocateObject);

    // extract the first symbol from the AllocateObject
    if (expr) {
      if (auto dataRef{evaluate::ExtractDataRef(*expr, true, true)}) {
        definedVars_.insert(dataRef->GetFirstSymbol());
      }
    }
  }
}

// check CommonStmts
void UninitializedVarCheck::Leave(const parser::CommonStmt &commonStmt) {
  const auto &blocks = commonStmt.blocks;
  for (const auto &block : blocks) {
    // get common block objects
    const auto &objects =
        std::get<std::list<parser::CommonBlockObject>>(block.t);
    for (const auto &object : objects) {
      // extract the symbol from the name
      const auto &name = std::get<parser::Name>(object.t);
      if (const auto *symbol{name.symbol}) {
        definedVars_.insert(*symbol);
      }
    }
  }
}

// check CallStmts
void UninitializedVarCheck::Leave(const parser::CallStmt &callStmt) {
  const auto *procedureRef = callStmt.typedCall.get();
  if (procedureRef) {
    for (const auto &arg : procedureRef->arguments()) {
      if (const semantics::SomeExpr * argExpr{arg->UnwrapExpr()}) {
        if (const semantics::Symbol *
            var{evaluate::UnwrapWholeSymbolDataRef(*argExpr)}) {
          common::Intent intent{arg->dummyIntent()};
          if (intent == common::Intent::InOut ||
              intent == common::Intent::Out) {
            definedVars_.insert(*var);
          }
        }
      }
    }
  }
}

using namespace parser::literals;
void UninitializedVarCheck::Enter(const parser::Expr &e) {
  const auto *expr = semantics::GetExpr(e);
  if (!expr) {
    return;
  }

  // check FunctionRefs
  if (std::holds_alternative<common::Indirection<parser::FunctionReference>>(
          e.u)) {
    evaluate::ActualArgumentSet argSet{evaluate::CollectActualArguments(*expr)};
    for (const evaluate::ActualArgumentRef &argRef : argSet) {
      if (const semantics::SomeExpr * argExpr{argRef->UnwrapExpr()}) {
        if (const semantics::Symbol *
            var{evaluate::UnwrapWholeSymbolDataRef(*argExpr)}) {
          common::Intent intent{argRef->dummyIntent()};
          if (intent == common::Intent::InOut ||
              intent == common::Intent::Out) {
            definedVars_.insert(*var);
          }
        }
      }
    }
  }

  // do the actual check
  if (std::holds_alternative<common::Indirection<parser::Designator>>(e.u)) {
    const auto symbols = evaluate::CollectSymbols(*expr);

    if (!context_.location()) {
      return;
    }

    const auto &scope = context_.FindScope(context_.location().value());

    for (const auto &symbol : symbols) {
      // if the symbol doesnt originate from our scope, skip it
      if (symbol->GetUltimate().owner() != scope) {
        continue;
      }

      // symbol should not be a dummy argument
      if (const auto *details{
              symbol->detailsIf<semantics::ObjectEntityDetails>()};
          details && details->isDummy()) {
        continue;
      }

      // is it inited?
      if (semantics::IsInitialized(symbol))
        continue;

      if (definedVars_.find(symbol) == definedVars_.end()) {
        context_.Say(e.source,
                     "Variable '%s' may be used uninitialized"_warn_en_US,
                     symbol->name());
      }
    }
  }
}

} // namespace Fortran::tidy::bugprone
