#include "../FlangTidyModule.h"
#include "../FlangTidyModuleRegistry.h"
#include "PureProcedureCheck.h"

namespace Fortran::tidy {
namespace misc {

class MiscModule : public FlangTidyModule {
public:
  void addCheckFactories(FlangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<PureProcedureCheck>("misc-pure-procedure");
    // CheckFactories.registerCheck<UnusedUseCheck>("misc-unused-use");
  }
};

} // namespace misc

// Register the MiscTidyModule using this statically initialized variable.
static FlangTidyModuleRegistry::Add<misc::MiscModule>
    X("misc-module", "Miscellaneous checks.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the MiscModule.
// NOLINTNEXTLINE
volatile int MiscModuleAnchorSource = 0;

} // namespace Fortran::tidy
