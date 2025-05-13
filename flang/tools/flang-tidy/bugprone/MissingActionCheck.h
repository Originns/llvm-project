#ifndef FORTRAN_TIDY_MISSINGACTIONCHECK
#define FORTRAN_TIDY_MISSINGACTIONCHECK

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

/// This check verifies that all OPEN statements have an ACTION clause, and that
/// the FILE UNIT NUMBER is not a constant literal.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/missing-action.html
class MissingActionCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~MissingActionCheck() = default;

  void Leave(const parser::FileUnitNumber &) override;
  void Leave(const parser::OpenStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_MISSINGACTIONCHECK
