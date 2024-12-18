#ifndef FORTRAN_TIDY_ARITHMETICGOTOCHECK
#define FORTRAN_TIDY_ARITHMETICGOTOCHECK

#include "../FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::bugprone {

class ArithmeticGotoCheck : public FlangTidyCheck {
public:
  explicit ArithmeticGotoCheck(llvm::StringRef name, FlangTidyContext *context)
      : FlangTidyCheck{name}, context_{context} {}
  virtual ~ArithmeticGotoCheck() = default;
  void Enter(const parser::ComputedGotoStmt &) override;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_ARITHMETICGOTOCHECK
