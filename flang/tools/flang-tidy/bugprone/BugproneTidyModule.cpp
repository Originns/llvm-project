#include "../FlangTidyModule.h"
#include "../FlangTidyModuleRegistry.h"
#include "ArithmeticGotoCheck.h"
#include "ArithmeticIfStmtCheck.h"
#include "ImplicitDeclCheck.h"
#include "PrecisionLossCheck.h"
#include "UndeclaredProcCheck.h"
#include "UninitializedVarCheck.h"
#include "UnusedIntentCheck.h"

namespace Fortran::tidy {
namespace bugprone {

class BugproneModule : public FlangTidyModule {
public:
  void addCheckFactories(FlangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<ArithmeticGotoCheck>(
        "bugprone-arithmetic-goto");
    CheckFactories.registerCheck<ArithmeticIfStmtCheck>(
        "bugprone-arithmetic-if");
    CheckFactories.registerCheck<ImplicitDeclCheck>(
        "bugprone-implicit-declaration");
    CheckFactories.registerCheck<PrecisionLossCheck>("bugprone-precision-loss");
    CheckFactories.registerCheck<UndeclaredProcCheck>(
        "bugprone-undeclared-proc");
    CheckFactories.registerCheck<UninitializedVarCheck>(
        "bugprone-uninitialized-variable");
    CheckFactories.registerCheck<UnusedIntentCheck>("bugprone-unused-intent");
  }
};

} // namespace bugprone

// Register the BugproneTidyModule using this statically initialized variable.
static FlangTidyModuleRegistry::Add<bugprone::BugproneModule>
    X("bugprone-module", "Adds checks for bugprone code constructs.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the BugproneModule.
volatile int BugproneModuleAnchorSource = 0;

} // namespace Fortran::tidy
