#ifndef FORTRAN_TIDY_FLANGTIDYCHECK_H
#define FORTRAN_TIDY_FLANGTIDYCHECK_H

#include "flang/Semantics/semantics.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy {

// flang tidy check is a base class for all flang tidy checks
// it has Enter and Leave methods that are called by the flang
// semantic checker when entering and leaving a node in the AST
class FlangTidyCheck : public semantics::BaseChecker {
public:
  FlangTidyCheck(llvm::StringRef name) : name_{name} {}
  llvm::StringRef name() const { return name_; }

private:
  llvm::StringRef name_;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYCHECK_H
