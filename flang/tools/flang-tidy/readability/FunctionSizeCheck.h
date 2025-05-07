#ifndef FORTRAN_TIDY_FUNCTIONSIZECHECK
#define FORTRAN_TIDY_FUNCTIONSIZECHECK

#include "../FlangTidyCheck.h"
#include "flang/Parser/char-block.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::readability {

class FunctionSizeCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~FunctionSizeCheck() = default;

  void Enter(const parser::SubroutineSubprogram &) override;
  void Leave(const parser::SubroutineSubprogram &) override;
  void Enter(const parser::FunctionSubprogram &) override;
  void Leave(const parser::FunctionSubprogram &) override;

  void Enter(const parser::Block &) override;
  void Leave(const parser::Block &) override;

private:
  void UpdateMaxNestingLevel();
  void CheckNestingThreshold();

  bool inProcedure_ = false;
  int currentNestingLevel_ = 0;
  int maxNestingLevel_ = 0;
  parser::CharBlock currentProcLoc_;
};

} // namespace Fortran::tidy::readability

#endif // FORTRAN_TIDY_FUNCTIONSIZECHECK
