#ifndef FORTRAN_TIDY_COMMONBLOCK
#define FORTRAN_TIDY_COMMONBLOCK

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::modernize {

class AvoidCommonBlocksCheck : public virtual FlangTidyCheck {
public:
  explicit AvoidCommonBlocksCheck(llvm::StringRef name,
                                  FlangTidyContext *context);
  ~AvoidCommonBlocksCheck();
  void Enter(const parser::CommonStmt &);

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_COMMONBLOCK
