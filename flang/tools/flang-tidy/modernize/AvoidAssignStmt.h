#ifndef FORTRAN_TIDY_ASSIGNSTMT
#define FORTRAN_TIDY_ASSIGNSTMT

#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

class AvoidAssignStmtCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~AvoidAssignStmtCheck() = default;
  void Enter(const parser::AssignStmt &) override;
  void Enter(const parser::AssignedGotoStmt &) override;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_ASSIGNSTMT
