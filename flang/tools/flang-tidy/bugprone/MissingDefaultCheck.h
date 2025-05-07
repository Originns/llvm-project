#ifndef FORTRAN_TIDY_MISSINGDEFAULTCHECK
#define FORTRAN_TIDY_MISSINGDEFAULTCHECK

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

class MissingDefaultCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~MissingDefaultCheck() = default;

  void Enter(const parser::CaseConstruct &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_MISSINGDEFAULTCHECK
