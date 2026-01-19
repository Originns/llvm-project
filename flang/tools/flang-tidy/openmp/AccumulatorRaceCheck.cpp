//===--- AccumulatorRaceCheck.cpp - flang-tidy ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "AccumulatorRaceCheck.h"
#include "flang/Evaluate/check-expression.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Parser/tools.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Frontend/OpenMP/OMP.h.inc"
#include <algorithm>
#include <variant>
#include <vector>

namespace Fortran::tidy::openmp {

using namespace parser::literals;

static std::vector<llvm::SmallVector<const semantics::Symbol *, 4>>
    currentPrivate{};

void AccumulatorRaceCheck::Enter(const parser::OmpBlockConstruct &construct) {
  ++inParallelRegion_;

  currentPrivate.push_back({});

  const auto &directive = construct.BeginDir();
  const auto &clause = directive.Clauses();

  // only handle lastprivate, firstprivate, and private for now
  for (const auto &clause : clause.v) {
    if (std::holds_alternative<parser::OmpClause::Lastprivate>(clause.u)) {
      const auto &lastprivateClause =
          std::get<parser::OmpClause::Lastprivate>(clause.u);
      const auto &ompObjectList =
          std::get<parser::OmpObjectList>(lastprivateClause.v.t);
      for (const auto &ompObject : ompObjectList.v) {

        // TODO: is there a better way to do this?
        if (const auto *name{std::get_if<parser::Name>(&ompObject.u)}) {
          const auto *symbol = name->symbol;
          if (symbol) {
            currentPrivate.back().emplace_back(symbol);
          }
        }
        // handle Designator -> DataRef -> Name
        if (const auto *designator{
                std::get_if<parser::Designator>(&ompObject.u)}) {
          const auto *name = parser::GetDesignatorNameIfDataRef(*designator);
          if (name) {
            const auto *symbol = name->symbol;
            if (symbol) {
              currentPrivate.back().emplace_back(symbol);
            }
          }
        }
      }

    } else if (std::holds_alternative<parser::OmpClause::Firstprivate>(
                   clause.u)) {
      const auto &firstprivateClause =
          std::get<parser::OmpClause::Firstprivate>(clause.u);
      const auto &ompObjectList = firstprivateClause.v;
      for (const auto &ompObject : ompObjectList.v) {
        // get the symbol, only do names for now
        if (const auto *name{std::get_if<parser::Name>(&ompObject.u)}) {
          const auto *symbol = name->symbol;
          if (symbol) {
            currentPrivate.back().emplace_back(symbol);
          }
        }
        if (const auto *designator{
                std::get_if<parser::Designator>(&ompObject.u)}) {
          const auto *name = parser::GetDesignatorNameIfDataRef(*designator);
          if (name) {
            const auto *symbol = name->symbol;
            if (symbol) {
              currentPrivate.back().emplace_back(symbol);
            }
          }
        }
      }
    } else if (std::holds_alternative<parser::OmpClause::Private>(clause.u)) {
      const auto &privateClause =
          std::get<parser::OmpClause::Private>(clause.u);
      const auto &ompObjectList = privateClause.v;
      for (const auto &ompObject : ompObjectList.v) {
        // get the symbol, only do names for now
        if (const auto *name{std::get_if<parser::Name>(&ompObject.u)}) {
          const auto *symbol = name->symbol;
          if (symbol) {
            currentPrivate.back().emplace_back(symbol);
          }
        }
        if (const auto *designator{
                std::get_if<parser::Designator>(&ompObject.u)}) {
          const auto *name = parser::GetDesignatorNameIfDataRef(*designator);
          if (name) {
            const auto *symbol = name->symbol;
            if (symbol) {
              currentPrivate.back().emplace_back(symbol);
            }
          }
        }
      }
    }
  }
}

void AccumulatorRaceCheck::Leave(const parser::OmpBlockConstruct &) {
  --inParallelRegion_;
  currentPrivate.pop_back();
}

void AccumulatorRaceCheck::Enter(const parser::OpenMPLoopConstruct &construct) {
  ++inParallelRegion_;

  // extract all symbols
  currentPrivate.push_back({}); // push a new private list

  const auto &directive = construct.BeginDir();
  const auto &clause = directive.Clauses();
  for (const auto &clause : clause.v) {
    if (std::holds_alternative<parser::OmpClause::Lastprivate>(clause.u)) {
      const auto &lastprivateClause =
          std::get<parser::OmpClause::Lastprivate>(clause.u);
      const auto &ompObjectList =
          std::get<parser::OmpObjectList>(lastprivateClause.v.t);
      for (const auto &ompObject : ompObjectList.v) {
        // get the symbol, only do names for now
        if (const auto *name{std::get_if<parser::Name>(&ompObject.u)}) {
          const auto *symbol = name->symbol;
          if (symbol) {
            currentPrivate.back().emplace_back(symbol);
          }
        }
        if (const auto *designator{
                std::get_if<parser::Designator>(&ompObject.u)}) {
          const auto *name = parser::GetDesignatorNameIfDataRef(*designator);
          if (name) {
            const auto *symbol = name->symbol;
            if (symbol) {
              currentPrivate.back().emplace_back(symbol);
            }
          }
        }
      }

    } else if (std::holds_alternative<parser::OmpClause::Firstprivate>(
                   clause.u)) {
      const auto &firstprivateClause =
          std::get<parser::OmpClause::Firstprivate>(clause.u);
      const auto &ompObjectList = firstprivateClause.v;
      for (const auto &ompObject : ompObjectList.v) {
        // get the symbol, only do names for now
        if (const auto *name{std::get_if<parser::Name>(&ompObject.u)}) {
          const auto *symbol = name->symbol;
          if (symbol) {
            currentPrivate.back().emplace_back(symbol);
          }
        }
        if (const auto *designator{
                std::get_if<parser::Designator>(&ompObject.u)}) {
          const auto *name = parser::GetDesignatorNameIfDataRef(*designator);
          if (name) {
            const auto *symbol = name->symbol;
            if (symbol) {
              currentPrivate.back().emplace_back(symbol);
            }
          }
        }
      }
    } else if (std::holds_alternative<parser::OmpClause::Private>(clause.u)) {
      const auto &privateClause =
          std::get<parser::OmpClause::Private>(clause.u);
      const auto &ompObjectList = privateClause.v;
      for (const auto &ompObject : ompObjectList.v) {
        // get the symbol, only do names for now
        if (const auto *name{std::get_if<parser::Name>(&ompObject.u)}) {
          const auto *symbol = name->symbol;
          if (symbol) {
            currentPrivate.back().emplace_back(symbol);
          }
        }
        if (const auto *designator{
                std::get_if<parser::Designator>(&ompObject.u)}) {
          const auto *name = parser::GetDesignatorNameIfDataRef(*designator);
          if (name) {
            const auto *symbol = name->symbol;
            if (symbol) {
              currentPrivate.back().emplace_back(symbol);
            }
          }
        }
      }
    }
  }
}

void AccumulatorRaceCheck::Leave(const parser::OpenMPLoopConstruct &) {
  --inParallelRegion_;
  currentPrivate.pop_back();
}

void AccumulatorRaceCheck::Enter(
    const parser::OpenMPAtomicConstruct &construct) {
  // atomic update?
  if (construct.GetKind() == llvm::omp::Clause::OMPC_update) {
    ++inAtomicUpdate_;
  }
}

void AccumulatorRaceCheck::Leave(
    const parser::OpenMPAtomicConstruct &construct) {
  if (construct.GetKind() == llvm::omp::Clause::OMPC_update) {
    --inAtomicUpdate_;
  }
}

void AccumulatorRaceCheck::Enter(const parser::OpenMPCriticalConstruct &) {
  ++inCriticalSection_;
}

void AccumulatorRaceCheck::Leave(const parser::OpenMPCriticalConstruct &) {
  --inCriticalSection_;
}

void AccumulatorRaceCheck::Enter(const parser::AssignmentStmt &stmt) {
  if (inParallelRegion_ && !inCriticalSection_ && !inAtomicUpdate_) {
    const auto &var = std::get<parser::Variable>(stmt.t);
    const auto &expr = std::get<parser::Expr>(stmt.t);

    const auto *lhsExpr =
        semantics::GetExpr(context()->getSemanticsContext(), var);
    // if the lhs isnt a whole or component data ref (NO ARRAY), ignore
    const semantics::Symbol *lhsSymbol =
        evaluate::UnwrapWholeSymbolOrComponentDataRef(*lhsExpr);

    if (!lhsSymbol) {
      return;
    }

    // if the lhs is in any of the private lists, ignore (stack has no begin()
    // iterator)
    bool isPrivate =
        std::any_of(currentPrivate.rbegin(), currentPrivate.rend(),
                    [&lhsSymbol](const auto &privateList) {
                      return std::find(privateList.begin(), privateList.end(),
                                       lhsSymbol) != privateList.end();
                    });

    if (isPrivate) {
      return;
    }

    const auto *rhsExpr =
        semantics::GetExpr(context()->getSemanticsContext(), expr);

    if (rhsExpr && !evaluate::IsConstantExpr(*rhsExpr) && lhsSymbol) {
      Say(var.GetSource(), "possible race condition on '%s'"_warn_en_US,
          lhsSymbol->name());
    }
  }
}

} // namespace Fortran::tidy::openmp
