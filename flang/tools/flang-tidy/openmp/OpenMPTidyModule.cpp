#include "../FlangTidyModule.h"
#include "../FlangTidyModuleRegistry.h"
#include "AccumulatorRaceCheck.h"

namespace Fortran::tidy {
namespace openmp {

class OpenMPModule : public FlangTidyModule {
public:
  void addCheckFactories(FlangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<AccumulatorRaceCheck>(
        "openmp-accumulator-race");
  }
};

} // namespace openmp

// Register the OpenMPTidyModule using this statically initialized variable.
static FlangTidyModuleRegistry::Add<openmp::OpenMPModule>
    X("openmp-module", "Adds checks for OpenMP parallelization.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the OpenMPTidyModule.

// NOLINTNEXTLINE
volatile int OpenMPModuleAnchorSource = 0;

} // namespace Fortran::tidy
