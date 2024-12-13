#ifndef FORTRAN_TIDY_DATACONSTRUCT
#define FORTRAN_TIDY_DATACONSTRUCT

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"

namespace Fortran::tidy::modernize {

class AvoidDataConstructsCheck : public virtual FlangTidyCheck {
public:
  explicit AvoidDataConstructsCheck(llvm::StringRef name,
                                    FlangTidyContext *context);
  ~AvoidDataConstructsCheck();
  void Enter(const parser::DataStmt &);

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::modernize

#endif // FORTRAN_TIDY_DATACONSTRUCT
