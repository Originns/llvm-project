#ifndef FORTRAN_TIDY_UNUSEDUSECHECK
#define FORTRAN_TIDY_UNUSEDUSECHECK

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::misc {

class UnusedUseCheck : public virtual FlangTidyCheck {
public:
  explicit UnusedUseCheck(llvm::StringRef name, FlangTidyContext *context);
  virtual ~UnusedUseCheck() = default;

  void Leave(const parser::UseStmt &) override;
  void Leave(const parser::Program &) override;

private:
  FlangTidyContext *context_;
};

} // namespace Fortran::tidy::misc

#endif // FORTRAN_TIDY_UNUSEDUSECHECK
