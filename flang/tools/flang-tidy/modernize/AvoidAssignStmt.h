#ifndef FORTRAN_TIDY_ASSIGNSTMT
#define FORTRAN_TIDY_ASSIGNSTMT

#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

/// This check verifies that ASSIGN and ASSIGNED GOTO statements are avoided.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/avoid-assign-stmt.html
class AvoidAssignStmtCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~AvoidAssignStmtCheck() = default;
  void Enter(const parser::AssignStmt &) override;
  void Enter(const parser::AssignedGotoStmt &) override;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_ASSIGNSTMT
