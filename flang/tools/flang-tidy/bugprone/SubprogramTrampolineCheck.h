#ifndef FORTRAN_TIDY_SUBPROGRAMTRAMPOLINE
#define FORTRAN_TIDY_SUBPROGRAMTRAMPOLINE

#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

class SubprogramTrampolineCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~SubprogramTrampolineCheck() = default;
  void Enter(const parser::Expr &) override;
  void Enter(const parser::CallStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_SUBPROGRAMTRAMPOLINE
