#ifndef FORTRAN_TIDY_BACKSPACESTMT
#define FORTRAN_TIDY_BACKSPACESTMT

#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

class AvoidBackspaceStmtCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~AvoidBackspaceStmtCheck() = default;
  void Enter(const parser::BackspaceStmt &) override;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_BACKSPACESTMT
