#ifndef FORTRAN_TIDY_ARITHMETICIFSTMTCHECK
#define FORTRAN_TIDY_ARITHMETICIFSTMTCHECK

#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

class ArithmeticIfStmtCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~ArithmeticIfStmtCheck() = default;
  void Enter(const parser::ArithmeticIfStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_ARITHMETICIFSTMTCHECK
