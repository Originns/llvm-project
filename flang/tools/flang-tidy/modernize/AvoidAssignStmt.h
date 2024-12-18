#ifndef FORTRAN_TIDY_ASSIGNSTMT
#define FORTRAN_TIDY_ASSIGNSTMT

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

class AvoidAssignStmtCheck : public virtual FlangTidyCheck {
public:
  explicit AvoidAssignStmtCheck(llvm::StringRef name,
                                FlangTidyContext *context);
  virtual ~AvoidAssignStmtCheck() = default;
  void Enter(const parser::AssignStmt &) override;
  void Enter(const parser::AssignedGotoStmt &) override;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_ASSIGNSTMT
