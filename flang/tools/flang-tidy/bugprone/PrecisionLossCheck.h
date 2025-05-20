#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_PRECISIONLOSSCHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_PRECISIONLOSSCHECK_H

#include "../FlangTidyCheck.h"

namespace Fortran::tidy::bugprone {

/// This check verifies that precision loss is avoided in assignments.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/precision-loss.html
class PrecisionLossCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~PrecisionLossCheck() = default;
  void Enter(const parser::AssignmentStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_PRECISIONLOSSCHECK_H
