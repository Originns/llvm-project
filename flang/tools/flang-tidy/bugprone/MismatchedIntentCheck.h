#ifndef FORTRAN_TIDY_MISMATCHEDINTENTCHECK
#define FORTRAN_TIDY_MISMATCHEDINTENTCHECK

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

class MismatchedIntentCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~MismatchedIntentCheck() = default;

  void Enter(const parser::CallStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_MISMATCHEDINTENTCHECK
