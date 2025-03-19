#ifndef FORTRAN_TIDY_MISSINGDEFAULTCHECK
#define FORTRAN_TIDY_MISSINGDEFAULTCHECK

#include "../FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::bugprone {

class MissingDefaultCheck : public virtual FlangTidyCheck {
public:
  MissingDefaultCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~MissingDefaultCheck() = default;

  void Enter(const parser::CaseConstruct &) override;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_MISSINGDEFAULTCHECK
