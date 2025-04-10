#include "AccumulatorRaceCheck.h"
#include "flang/Evaluate/check-expression.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::openmp {

using namespace parser::literals;

void AccumulatorRaceCheck::Enter(const parser::OpenMPBlockConstruct &) {
  inParallelRegion_ = true;
}

void AccumulatorRaceCheck::Leave(const parser::OpenMPBlockConstruct &) {
  inParallelRegion_ = false;
}

void AccumulatorRaceCheck::Enter(const parser::OmpAtomicUpdate &) {
  inAtomicUpdate_ = true;
}

void AccumulatorRaceCheck::Leave(const parser::OmpAtomicUpdate &) {
  inAtomicUpdate_ = false;
}

void AccumulatorRaceCheck::Enter(const parser::OpenMPCriticalConstruct &) {
  inCriticalSection_ = true;
}

void AccumulatorRaceCheck::Leave(const parser::OpenMPCriticalConstruct &) {
  inCriticalSection_ = false;
}

void AccumulatorRaceCheck::Enter(const parser::AssignmentStmt &stmt) {
  if (inParallelRegion_ && !inCriticalSection_ && !inAtomicUpdate_) {
    const auto &var = std::get<parser::Variable>(stmt.t);
    const auto &expr = std::get<parser::Expr>(stmt.t);

    const auto *lhsExpr =
        semantics::GetExpr(context()->getSemanticsContext(), var);
    const semantics::Symbol *lhsSymbol = nullptr;
    if (lhsExpr) {
      lhsSymbol = evaluate::GetFirstSymbol(*lhsExpr);
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
