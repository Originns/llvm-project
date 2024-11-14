#ifndef FORTRAN_TIDY_COMMONBLOCK
#define FORTRAN_TIDY_COMMONBLOCK

#include "flang/Semantics/semantics.h"

namespace Fortran::tidy::modernize {

class AvoidCommonBlocksCheck : public virtual semantics::BaseChecker {
public:
  explicit AvoidCommonBlocksCheck(semantics::SemanticsContext &);
  ~AvoidCommonBlocksCheck();
  void Enter(const parser::CommonStmt &);

private:
  semantics::SemanticsContext &context_;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_COMMONBLOCK
