#include "UninitializedVarCheck.h"
#include "flang/Evaluate/call.h"
#include "flang/Evaluate/tools.h"
#include "flang/Evaluate/variable.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/attr.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"
#include "utils/CollectActualArguments.h"
#include <variant>

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

// check AssignmentStmts
void UninitializedVarCheck::Leave(const parser::AssignmentStmt &assignment) {
  const auto &var = std::get<parser::Variable>(assignment.t);
  const auto *lhs = semantics::GetExpr(context()->getSemanticsContext(), var);
  if (lhs) {
    // TODO: handle this better, ideally with a visitor(?)
    if (const semantics::Symbol *base{evaluate::GetFirstSymbol(*lhs)}; base) {
      // initialize the base symbol
      definedVars_.insert(*base);

      // if it has attr allocatable and is not part of our allocated vars and
      // is from the same scope, warn
      if (base->attrs().test(semantics::Attr::ALLOCATABLE)) {

        // ignore deferred length character allocatables
        const auto &ultimate{ResolveAssociations(*base)};
        if (const semantics::DeclTypeSpec *type{ultimate.GetType()};
            type &&
            type->category() == semantics::DeclTypeSpec::Category::Character &&
            type->characterTypeSpec().length().isDeferred() &&
            semantics::IsAllocatable(ultimate) && ultimate.Rank() == 0) {
          allocatedVars_.insert(*base);
          return;
        }

        if (allocatedVars_.find(*base) == allocatedVars_.end() &&
            base->owner() ==
                context()->getSemanticsContext().FindScope(
                    context()->getSemanticsContext().location().value())) {
          Say(
              var.GetSource(), "Variable '%s' may be unallocated"_warn_en_US,
              base->name().ToString());
        }
      }
    }
  }
}

// check PointerAssignmentStmts
void UninitializedVarCheck::Leave(
    const parser::PointerAssignmentStmt &ptrAssignmentStmt) {
  const auto *typedAssignment = semantics::GetAssignment(ptrAssignmentStmt);
  if (typedAssignment) {
    if (const semantics::Symbol *base{
            evaluate::GetFirstSymbol(typedAssignment->lhs)}) {
      definedVars_.insert(*base);
    }
  }
}

void UninitializedVarCheck::Leave(const parser::OpenStmt &openStmt) {
  // if it has iostat, we can mark it as defined
  const auto &connectSpec = openStmt.v;

  // find ALL stat variables
  const auto statVars = std::find_if(
      connectSpec.begin(), connectSpec.end(), [](const auto &spec) {
        return std::holds_alternative<parser::StatVariable>(spec.u);
      });

  // if we have a stat variable, mark it as defined
  if (statVars != connectSpec.end()) {
    const auto &statVar = std::get<parser::StatVariable>(statVars->u);
    if (const auto *expr{
            semantics::GetExpr(context()->getSemanticsContext(), statVar)}) {
      if (const auto *symbol{evaluate::UnwrapWholeSymbolDataRef(*expr)}) {
        definedVars_.insert(*symbol);
      }
    }
  }
}

void UninitializedVarCheck::Leave(const parser::ReadStmt &readStmt) {
  if (!readStmt.items.empty()) {
    for (const auto &item : readStmt.items) {
      if (const auto *var{std::get_if<parser::Variable>(&item.u)}) {
        if (const auto *expr{
                semantics::GetExpr(context()->getSemanticsContext(), *var)}) {
          if (const auto *symbol{evaluate::UnwrapWholeSymbolDataRef(*expr)}) {
            definedVars_.insert(*symbol);
            allocatedVars_.insert(*symbol);
          }
        }
      }
    }
  }
}

void UninitializedVarCheck::Enter(
    const parser::OutputImpliedDo &outputImpliedDo) {
  const auto &bounds = std::get<parser::IoImpliedDoControl>(outputImpliedDo.t);
  if (const auto *symbol{bounds.name.thing.thing.symbol}) {
    definedVars_.insert(*symbol);
  }
}

void UninitializedVarCheck::Enter(
    const parser::InputImpliedDo &inputImpliedDo) {
  const auto &bounds = std::get<parser::IoImpliedDoControl>(inputImpliedDo.t);
  if (const auto *symbol{bounds.name.thing.thing.symbol}) {
    definedVars_.insert(*symbol);
  }
}

void UninitializedVarCheck::Enter(const parser::AcImpliedDo &acImpliedDo) {
  const auto &doControl = std::get<parser::AcImpliedDoControl>(acImpliedDo.t);
  const auto &bounds =
      std::get<parser::AcImpliedDoControl::Bounds>(doControl.t);
  if (const auto *symbol{bounds.name.thing.thing.symbol}) {
    definedVars_.insert(*symbol);
  }
}

void UninitializedVarCheck::Enter(const parser::DataImpliedDo &dataImpliedDo) {
  const auto &bounds = std::get<parser::DataImpliedDo::Bounds>(dataImpliedDo.t);
  if (const auto *symbol{bounds.name.thing.thing.symbol}) {
    definedVars_.insert(*symbol);
  }
}

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
    const auto *expr =
        semantics::GetExpr(context()->getSemanticsContext(), allocateObject);

    // extract the first symbol from the AllocateObject
    if (expr) {
      if (auto dataRef{evaluate::ExtractDataRef(*expr, true, true)}) {
        // not counting an allocation as a definition
        // definedVars_.insert(dataRef->GetFirstSymbol());
        allocatedVars_.insert(dataRef->GetFirstSymbol());
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
        // TODO: check if this is correct
        // allocatedVars_.insert(*symbol);
      }
    }
  }
}

// check CallStmts
void UninitializedVarCheck::Enter(const parser::CallStmt &callStmt) {
  const auto *procedureRef = callStmt.typedCall.get();
  if (procedureRef) {
    for (const auto &arg : procedureRef->arguments()) {
      if (!arg)
        continue;
      if (const semantics::SomeExpr *argExpr{arg->UnwrapExpr()}) {
        // is the expression a whole symbol data ref or a base symbol data ref
        const auto *var{evaluate::UnwrapWholeSymbolDataRef(*argExpr)};
        if (!var) {
          var = evaluate::GetFirstSymbol(*argExpr);
        }
        if (var) {
          common::Intent intent{arg->dummyIntent()};
          if (intent == common::Intent::Out ||
              intent == common::Intent::InOut) { /* TODO: set InOut when leaving
                                                    CallStmt  */
            definedVars_.insert(*var);
            allocatedVars_.insert(*var);
          }
        }
      }
    }
  }
}

// check WriteStmts
void UninitializedVarCheck::Leave(const parser::WriteStmt &writeStmt) {
  if (writeStmt.iounit) {
    if (const auto *var{std::get_if<parser::Variable>(&writeStmt.iounit->u)}) {
      if (const auto *expr{
              semantics::GetExpr(context()->getSemanticsContext(), *var)}) {
        if (const auto *symbol{evaluate::UnwrapWholeSymbolDataRef(*expr)}) {
          definedVars_.insert(*symbol);
          allocatedVars_.insert(*symbol);
        }
      }
    }
  }
}

void UninitializedVarCheck::Enter(const parser::Expr &e) {
  static bool shouldSkip = false;
  const auto *expr = semantics::GetExpr(context()->getSemanticsContext(), e);
  if (!expr) {
    return;
  }

  // check FunctionRefs
  if (std::holds_alternative<common::Indirection<parser::FunctionReference>>(
          e.u)) {
    // if the function is sizeof(), set a flag
    const auto &functionRef =
        std::get<common::Indirection<parser::FunctionReference>>(e.u);
    const auto &procDesignator =
        std::get<parser::ProcedureDesignator>(functionRef.value().v.t);

    if (std::holds_alternative<parser::Name>(procDesignator.u)) {
      const auto *procedureSym =
          std::get<parser::Name>(procDesignator.u).symbol;
      if (procedureSym &&
          procedureSym->attrs().test(semantics::Attr::INTRINSIC) &&
          (procedureSym->name() == "sizeof" ||
           procedureSym->name() == "c_sizeof" ||
           procedureSym->name() == "allocated" ||
           procedureSym->name() == "associated")) {
        shouldSkip = true;
      }
    }

    evaluate::ActualArgumentSet argSet{evaluate::CollectActualArguments(*expr)};
    for (const evaluate::ActualArgumentRef &argRef : argSet) {
      if (const semantics::SomeExpr *argExpr{argRef->UnwrapExpr()}) {
        if (const semantics::Symbol *var{
                evaluate::UnwrapWholeSymbolDataRef(*argExpr)}) {
          common::Intent intent{argRef->dummyIntent()};
          if (intent == common::Intent::Out ||
              intent == common::Intent::InOut) { /* TODO: set InOut when
                                                    leaving func ref*/
            definedVars_.insert(*var);
            allocatedVars_.insert(*var);
          }
        }
      }
    }

    if (shouldSkip) {
      return;
    }
  }

  // do the actual check
  if (std::holds_alternative<common::Indirection<parser::Designator>>(e.u)) {
    const auto symbols = evaluate::CollectSymbols(*expr);

    if (symbols.empty() || !context()->getSemanticsContext().location()) {
      return;
    }

    const auto &scope = context()->getSemanticsContext().FindScope(
        context()->getSemanticsContext().location().value());

    for (const auto &symbol : symbols) {
      // if the symbol doesnt originate from our scope, skip it
      if (symbol->GetUltimate().owner() != scope) {
        continue;
      }

      // symbol should not be a dummy argument, or a reference to a slice of
      // memory (e.g. array section)
      if (const auto *details{
              symbol->detailsIf<semantics::ObjectEntityDetails>()};
          details && (details->isDummy())) {
        continue;
      }

      // if its a subprogram, skip it
      if (symbol->has<semantics::SubprogramDetails>()) {
        continue;
      }

      // is it inited?
      if (semantics::IsInitialized(symbol, true, true, true))
        continue;

      // is it a parameter?
      if (symbol->attrs().test(semantics::Attr::PARAMETER))
        continue;

      // is the symbol allocatable
      if (symbol->attrs().test(semantics::Attr::ALLOCATABLE)) {
        if (allocatedVars_.find(symbol) == allocatedVars_.end()) {
          Say(
              e.source, "Variable '%s' may be unallocated"_warn_en_US,
              symbol->name());
        }
      }

      // is the symbol an associated entity
      if (const auto *details =
              symbol->detailsIf<semantics::AssocEntityDetails>();
          details && details->expr()) {
        continue;
      }

      if (definedVars_.find(symbol) == definedVars_.end() && !shouldSkip) {
        Say(
            e.source, "Variable '%s' may be used uninitialized"_warn_en_US,
            symbol->name());
      }
    }
  }

  shouldSkip = false;
}

} // namespace Fortran::tidy::bugprone
