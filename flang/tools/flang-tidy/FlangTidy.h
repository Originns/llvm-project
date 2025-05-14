#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_FLANGTIDY_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_FLANGTIDY_H

#include "FlangTidyOptions.h"

namespace Fortran::tidy {

int runFlangTidy(const FlangTidyOptions &options);

} // namespace Fortran::tidy

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_FLANGTIDY_H
