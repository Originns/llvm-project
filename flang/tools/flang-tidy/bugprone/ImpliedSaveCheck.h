#ifndef FORTRAN_TIDY_IMPLICITSAVECHECK
#define FORTRAN_TIDY_IMPLICITSAVECHECK

#include "../FlangTidyCheck.h"

namespace Fortran::tidy::bugprone {

class ImpliedSaveCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  //ImpliedSaveCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~ImpliedSaveCheck() = default;

  void Enter(const parser::EntityDecl &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_IMPLICITSAVECHECK
