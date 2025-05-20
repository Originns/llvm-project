#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_UNUSEDINTENTCHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_UNUSEDINTENTCHECK_H

#include "../FlangTidyCheck.h"
#include "../FlangTidyContext.h"

namespace Fortran::tidy::bugprone {

/// This check verifies that all INTENT attributes are used.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/unused-intent.html
class UnusedIntentCheck : public virtual FlangTidyCheck {
public:
  UnusedIntentCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~UnusedIntentCheck() = default;
private:
  void CheckUnusedIntentHelper(semantics::SemanticsContext &, const semantics::Scope &);
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_UNUSEDINTENTCHECK_H
