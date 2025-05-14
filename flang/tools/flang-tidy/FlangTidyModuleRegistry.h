#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_FLANGTIDYMODULEREGISTRY_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_FLANGTIDYMODULEREGISTRY_H

#include "FlangTidyModule.h"
#include "llvm/Support/Registry.h"

namespace Fortran::tidy {

using FlangTidyModuleRegistry = llvm::Registry<FlangTidyModule>;

} // namespace Fortran::tidy

namespace llvm {
extern template class Registry<Fortran::tidy::FlangTidyModule>;
} // namespace llvm

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_FLANGTIDYMODULEREGISTRY_H
