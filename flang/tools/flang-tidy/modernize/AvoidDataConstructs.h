#ifndef FORTRAN_TIDY_DATACONSTRUCT
#define FORTRAN_TIDY_DATACONSTRUCT

#include "FlangTidyCheck.h"

namespace Fortran::tidy::modernize {

/// This check verifies that DATA constructs are avoided.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/avoid-data-constructs.html
class AvoidDataConstructsCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~AvoidDataConstructsCheck() = default;
  void Enter(const parser::DataStmt &) override;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_DATACONSTRUCT
