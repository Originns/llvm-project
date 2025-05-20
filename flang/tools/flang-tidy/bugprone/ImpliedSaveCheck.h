#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_IMPLIEDSAVECHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_IMPLIEDSAVECHECK_H

#include "../FlangTidyCheck.h"

namespace Fortran::tidy::bugprone {

/// This check warns about variables that are implicitly saved.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/implicit-save.html
class ImpliedSaveCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~ImpliedSaveCheck() = default;

  void Enter(const parser::EntityDecl &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_IMPLIEDSAVECHECK_H
