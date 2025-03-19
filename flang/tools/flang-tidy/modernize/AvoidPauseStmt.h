#ifndef FORTRAN_TIDY_AVOIDPAUSESTMT
#define FORTRAN_TIDY_AVOIDPAUSESTMT

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::modernize {

class AvoidPauseStmtCheck : public virtual FlangTidyCheck {
public:
  explicit AvoidPauseStmtCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~AvoidPauseStmtCheck() = default;
  void Enter(const parser::PauseStmt &) override;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_AVOIDPAUSESTMT
