#ifndef FORTRAN_TIDY_FLANGTIDYCONTEXT_H
#define FORTRAN_TIDY_FLANGTIDYCONTEXT_H

#include "flang/Semantics/semantics.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringRef.h"
#include <clang/Basic/Diagnostic.h>

#include "FlangTidyOptions.h"

namespace Fortran::tidy {

// flang tidy context holds information about the enabled checks
class FlangTidyContext {
public:
  FlangTidyContext(const FlangTidyOptions &options,
                   semantics::SemanticsContext *ctx,
                   clang::DiagnosticsEngine *diags) {
    for (const auto &CheckName : options.enabledChecks) {
      Checks.insert(CheckName);
    }
    Context = ctx;
    Diags = diags;
  }

  bool isCheckEnabled(const llvm::StringRef &CheckName) const {
    bool enabled = false;

    for (const auto &Pattern : Checks) {
      if (Pattern.starts_with("-")) {
        llvm::StringRef DisablePrefix = Pattern.drop_front(1);
        if (DisablePrefix.ends_with("*")) {
          DisablePrefix = DisablePrefix.drop_back(1);
          if (CheckName.starts_with(DisablePrefix)) {
            enabled = false;
          }
        } else if (DisablePrefix == CheckName) {
          enabled = false;
        }
      } else if (Pattern.ends_with("*")) {
        llvm::StringRef EnablePrefix = Pattern.drop_back(1);
        if (CheckName.starts_with(EnablePrefix)) {
          enabled = true;
        }
      } else if (Pattern == CheckName) {
        enabled = true;
      }
    }

    return enabled;
  }

  semantics::SemanticsContext &getSemanticsContext() const { return *Context; }

public:
  llvm::SmallSet<llvm::StringRef, 16> Checks;
  semantics::SemanticsContext *Context;
  clang::DiagnosticsEngine *Diags;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYCONTEXT_H
