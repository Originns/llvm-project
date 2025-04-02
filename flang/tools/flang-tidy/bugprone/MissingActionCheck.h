#ifndef FORTRAN_TIDY_MISSINGACTIONCHECK
#define FORTRAN_TIDY_MISSINGACTIONCHECK

#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

class MissingActionCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  //MissingActionCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~MissingActionCheck() = default;

  void Leave(const parser::FileUnitNumber &) override;
  void Leave(const parser::OpenStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_MISSINGACTIONCHECK
