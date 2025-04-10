#ifndef FORTRAN_TIDY_CONTIGUOUSARRAYCHECK
#define FORTRAN_TIDY_CONTIGUOUSARRAYCHECK

#include "../FlangTidyCheck.h"

namespace Fortran::tidy::bugprone {

class ContiguousArrayCheck : public FlangTidyCheck {
public:
  explicit ContiguousArrayCheck(llvm::StringRef name,
                                FlangTidyContext *context);
  virtual ~ContiguousArrayCheck() = default;

private:
  void CheckForContinguousArray(semantics::SemanticsContext &,
                                const semantics::Scope &);
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_CONTIGUOUSARRAYCHECK
