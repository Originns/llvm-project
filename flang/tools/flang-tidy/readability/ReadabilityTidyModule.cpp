#include "../FlangTidyModule.h"
#include "../FlangTidyModuleRegistry.h"
#include "FunctionCognitiveComplexityCheck.h"
#include "FunctionSizeCheck.h"

namespace Fortran::tidy {
namespace readability {

class ReadabilityTidyModule : public FlangTidyModule {
public:
  void addCheckFactories(FlangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<FunctionCognitiveComplexityCheck>(
        "readability-function-cognitive-complexity");
    CheckFactories.registerCheck<FunctionSizeCheck>(
        "readability-function-size");
  }
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
