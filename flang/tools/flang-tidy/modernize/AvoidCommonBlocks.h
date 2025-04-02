#ifndef FORTRAN_TIDY_COMMONBLOCK
#define FORTRAN_TIDY_COMMONBLOCK

#include "FlangTidyCheck.h"

namespace Fortran::tidy::modernize {

class AvoidCommonBlocksCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~AvoidCommonBlocksCheck() = default;
  void Enter(const parser::CommonStmt &) override;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_COMMONBLOCK
