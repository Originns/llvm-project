#ifndef FORTRAN_TIDY_BACKSPACESTMT
#define FORTRAN_TIDY_BACKSPACESTMT

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::modernize {

class AvoidBackspaceStmtCheck : public virtual FlangTidyCheck {
public:
  explicit AvoidBackspaceStmtCheck(llvm::StringRef name,
                                   FlangTidyContext *context);
  ~AvoidBackspaceStmtCheck();
  void Enter(const parser::BackspaceStmt &);

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_BACKSPACESTMT
