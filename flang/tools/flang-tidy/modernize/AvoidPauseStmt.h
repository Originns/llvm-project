#ifndef FORTRAN_TIDY_AVOIDPAUSESTMT
#define FORTRAN_TIDY_AVOIDPAUSESTMT

#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

class AvoidPauseStmtCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~AvoidPauseStmtCheck() = default;
  void Enter(const parser::PauseStmt &) override;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_AVOIDPAUSESTMT
