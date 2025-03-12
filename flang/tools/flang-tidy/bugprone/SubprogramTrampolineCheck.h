#ifndef FORTRAN_TIDY_SUBPROGRAMTRAMPOLINE
#define FORTRAN_TIDY_SUBPROGRAMTRAMPOLINE

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::bugprone {

class SubprogramTrampolineCheck : public virtual FlangTidyCheck {
public:
  explicit SubprogramTrampolineCheck(llvm::StringRef name,
                                     FlangTidyContext *context);
  virtual ~SubprogramTrampolineCheck() = default;
  void Enter(const parser::Expr &) override;
  void Enter(const parser::CallStmt &) override;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_SUBPROGRAMTRAMPOLINE
