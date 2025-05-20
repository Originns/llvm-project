#include "../FlangTidyModule.h"
#include "../FlangTidyModuleRegistry.h"
#include "AvoidAssignStmt.h"

namespace Fortran::tidy {
namespace modernize {

class ModernizeModule : public FlangTidyModule {
public:
  void addCheckFactories(FlangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<AvoidAssignStmtCheck>(
        "modernize-avoid-assign-stmt");
  }
};

} // namespace modernize

// Register the BugproneTidyModule using this statically initialized variable.
static FlangTidyModuleRegistry::Add<modernize::ModernizeModule>
    X("modernize-module", "Adds checks to enforce modern code style.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the ModernizeModule.

// NOLINTNEXTLINE
volatile int ModernizeModuleAnchorSource = 0;

} // namespace Fortran::tidy
