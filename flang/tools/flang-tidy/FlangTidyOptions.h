#ifndef FORTRAN_TIDY_FLANGTIDYOPTIONS_H
#define FORTRAN_TIDY_FLANGTIDYOPTIONS_H

#include <string>
#include <vector>

namespace Fortran::tidy {

struct FlangTidyOptions {
  std::vector<std::string> sourcePaths;
  std::vector<std::string> enabledChecks;
  std::vector<std::string> extraArgs;
  const char *argv0;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYOPTIONS_H
