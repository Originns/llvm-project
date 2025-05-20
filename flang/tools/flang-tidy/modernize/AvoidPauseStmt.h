#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_MODERNIZE_AVOIDPAUSESTMT_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_MODERNIZE_AVOIDPAUSESTMT_H

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

/// This check verifies that PAUSE statements are avoided.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/avoid-pause-stmt.html
class AvoidPauseStmtCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~AvoidPauseStmtCheck() = default;
  void Enter(const parser::PauseStmt &) override;
};

} // namespace Fortran::tidy::modernize

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_MODERNIZE_AVOIDPAUSESTMT_H
