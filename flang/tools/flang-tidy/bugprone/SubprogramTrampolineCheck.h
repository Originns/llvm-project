#ifndef FORTRAN_TIDY_SUBPROGRAMTRAMPOLINE
#define FORTRAN_TIDY_SUBPROGRAMTRAMPOLINE

#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

/// This check verifies that a contained subprogram is not passed as an
/// actual argument to a procedure.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/subprogram-trampoline.html
class SubprogramTrampolineCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~SubprogramTrampolineCheck() = default;
  void Enter(const parser::Expr &) override;
  void Enter(const parser::CallStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_SUBPROGRAMTRAMPOLINE
