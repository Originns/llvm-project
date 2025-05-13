#ifndef FORTRAN_TIDY_MISSINGDEFAULTCHECK
#define FORTRAN_TIDY_MISSINGDEFAULTCHECK

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

/// This check verifies that all CASE constructs have a default case.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/missing-default.html
class MissingDefaultCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~MissingDefaultCheck() = default;

  void Enter(const parser::CaseConstruct &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_MISSINGDEFAULTCHECK
