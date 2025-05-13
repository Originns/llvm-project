#ifndef FORTRAN_TIDY_FLANGTIDYOPTIONS_H
#define FORTRAN_TIDY_FLANGTIDYOPTIONS_H

#include <string>
#include <vector>

namespace Fortran::tidy {

/// Options for the Flang Tidy tool that are passed from the command line.
///
/// For user-facing documentation, see:
/// https://clang.llvm.org/docs/LibTooling.html#flang-tidy-options
struct FlangTidyOptions {
  std::vector<std::string> sourcePaths;
  std::vector<std::string> enabledChecks;
  std::vector<std::string> extraArgs;
  const char *argv0;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYOPTIONS_H
