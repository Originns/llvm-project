#ifndef FORTRAN_TIDY_FLANGTIDY_H
#define FORTRAN_TIDY_FLANGTIDY_H

#include "FlangTidyOptions.h"

namespace Fortran::tidy {

int runFlangTidy(const FlangTidyOptions &options);

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDY_H
