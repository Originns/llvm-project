#ifndef FORTRAN_TIDY_ARITHMETICIFSTMTCHECK
#define FORTRAN_TIDY_ARITHMETICIFSTMTCHECK

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

class ArithmeticIfStmtCheck : public virtual FlangTidyCheck {
public:
  explicit ArithmeticIfStmtCheck(llvm::StringRef name,
                                 FlangTidyContext *context)
      : FlangTidyCheck{name}, context_{context} {}
  virtual ~ArithmeticIfStmtCheck() = default;
  void Enter(const parser::ArithmeticIfStmt &) override;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_ARITHMETICIFSTMTCHECK
