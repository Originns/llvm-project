#ifndef FORTRAN_TIDY_FLANGTIDYOPTIONS_H
#define FORTRAN_TIDY_FLANGTIDYOPTIONS_H

#include <string>
#include <vector>

namespace Fortran::tidy {

struct FlangTidyOptions {
  std::string fileName;
  std::vector<std::string> enabledChecks;
  bool enableAllWarnings = false;
  bool dumpParseTree = false;
  std::vector<std::string> extraArgs;
  const char **argv;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYOPTIONS_H
