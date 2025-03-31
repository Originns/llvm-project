#include "../FlangTidyModule.h"
#include "../FlangTidyModuleRegistry.h"
#include "ArithmeticGotoCheck.h"
#include "ArithmeticIfStmtCheck.h"
#include "ImplicitDeclCheck.h"
#include "ImpliedSaveCheck.h"
#include "MissingActionCheck.h"
#include "MissingDefaultCheck.h"
#include "PrecisionLossCheck.h"
#include "SubprogramTrampolineCheck.h"
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
    CheckFactories.registerCheck<ImpliedSaveCheck>("bugprone-implied-save");
    CheckFactories.registerCheck<MissingActionCheck>(
        "bugprone-missing-action-stmt");
    CheckFactories.registerCheck<MissingDefaultCheck>(
        "bugprone-missing-default-case");
    CheckFactories.registerCheck<PrecisionLossCheck>("bugprone-precision-loss");
    CheckFactories.registerCheck<SubprogramTrampolineCheck>(
        "bugprone-subprogram-trampoline");
    CheckFactories.registerCheck<UndeclaredProcCheck>(
        "bugprone-undeclared-procedure");
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

// NOLINTNEXTLINE
volatile int BugproneModuleAnchorSource = 0;

} // namespace Fortran::tidy
