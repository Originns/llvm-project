#ifndef FORTRAN_TIDY_FLANGTIDY_H
#define FORTRAN_TIDY_FLANGTIDY_H

#include <string>
#include <vector>

namespace Fortran::tidy {

struct FlangTidyOptions {
  std::vector<std::string> fileNames;
  std::vector<std::string> enabledChecks;
  bool enableAllWarnings = false;
  bool dumpParseTree = false;
  std::vector<std::string> extraArgs;
};

int runFlangTidy(const FlangTidyOptions &options);

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDY_H
