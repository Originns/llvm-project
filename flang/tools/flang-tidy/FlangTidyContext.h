#ifndef FORTRAN_TIDY_FLANGTIDYCONTEXT_H
#define FORTRAN_TIDY_FLANGTIDYCONTEXT_H

#include "flang/Semantics/semantics.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringRef.h"
#include <clang/Basic/Diagnostic.h>

#include "FlangTidyOptions.h"

namespace Fortran::tidy {

// flang tidy context holds information about the enabled checks


/// This class is used to manage the context for Flang Tidy checks.
/// It contains the enabled checks and the semantics context.
/// It provides methods to check if a specific check is enabled and to access the semantics context.
///
/// For user-facing documentation, see:
/// https://flang.llvm.org/@PLACEHOLDER@/flang-tidy.html
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
  /// List of enabled checks.
  llvm::SmallSet<llvm::StringRef, 16> Checks;
  /// The semantics context used for the checks.
  semantics::SemanticsContext *Context;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYCONTEXT_H
