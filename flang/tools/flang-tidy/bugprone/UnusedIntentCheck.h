#ifndef FORTRAN_TIDY_UNUSEDINTENTCHECK
#define FORTRAN_TIDY_UNUSEDINTENTCHECK

#include "../FlangTidyCheck.h"
#include "FlangTidyContext.h"

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

#endif // FORTRAN_TIDY_UNUSEDINTENTCHECK
