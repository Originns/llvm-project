#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_MODERNIZE_AVOIDBACKSPACESTMT_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_MODERNIZE_AVOIDBACKSPACESTMT_H

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

/// This check verifies that BACKSPACE statements are avoided.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/avoid-backspace-stmt.html
class AvoidBackspaceStmtCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~AvoidBackspaceStmtCheck() = default;
  void Enter(const parser::BackspaceStmt &) override;
};

} // namespace Fortran::tidy::modernize

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_MODERNIZE_AVOIDBACKSPACESTMT_H
