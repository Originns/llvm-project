#include "../FlangTidyModule.h"
#include "../FlangTidyModuleRegistry.h"

namespace Fortran::tidy {
namespace readability {

class ReadabilityTidyModule : public FlangTidyModule {
public:
  void addCheckFactories(FlangTidyCheckFactories &CheckFactories) override {}
};

} // namespace readability

// Register the ReadabilityTidyModule using this statically initialized
// variable.
static FlangTidyModuleRegistry::Add<readability::ReadabilityTidyModule>
    X("readability-module", "Adds checks for readability.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the ReadabilityTidyModule.

// NOLINTNEXTLINE
volatile int ReadabilityModuleAnchorSource = 0;

} // namespace Fortran::tidy
