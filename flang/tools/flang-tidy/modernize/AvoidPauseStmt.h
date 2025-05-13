#ifndef FORTRAN_TIDY_AVOIDPAUSESTMT
#define FORTRAN_TIDY_AVOIDPAUSESTMT

#include "FlangTidyCheck.h"
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

#endif // FORTRAN_TIDY_AVOIDPAUSESTMT
