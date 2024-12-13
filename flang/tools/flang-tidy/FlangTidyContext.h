#ifndef FORTRAN_TIDY_FLANGTIDYCONTEXT_H
#define FORTRAN_TIDY_FLANGTIDYCONTEXT_H

#include "flang/Semantics/semantics.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringRef.h"

#include "FlangTidyOptions.h"

namespace Fortran::tidy {

// flang tidy context holds information about the enabled checks
class FlangTidyContext {
public:
  FlangTidyContext(const FlangTidyOptions &options,
                   semantics::SemanticsContext *ctx) {
    for (const auto &CheckName : options.enabledChecks) {
      Checks.insert(CheckName);
    }
    Context = ctx;
  }

  bool isCheckEnabled(const llvm::StringRef &CheckName) const {
    return Checks.count(CheckName) > 0;
  }

  semantics::SemanticsContext &getSemanticsContext() const { return *Context; }

public:
  llvm::SmallSet<llvm::StringRef, 16> Checks;
  semantics::SemanticsContext *Context;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYCONTEXT_H
