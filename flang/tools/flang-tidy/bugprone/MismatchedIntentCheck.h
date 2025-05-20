#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_MISMATCHEDINTENTCHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_MISMATCHEDINTENTCHECK_H

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

/// This check warns if a variable is passed to a procedure multiple times with
/// mismatched intent.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/mismatched-intent.html
class MismatchedIntentCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~MismatchedIntentCheck() = default;

  void Enter(const parser::CallStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_MISMATCHEDINTENTCHECK_H
