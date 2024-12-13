#include "FlangTidy.h"
#include "FlangTidyModule.h"
#include "bugprone/ArithmeticGotoCheck.h"
#include "bugprone/ArithmeticIfStmtCheck.h"
#include "bugprone/ImplicitDeclCheck.h"
#include "bugprone/PrecisionLossCheck.h"
#include "bugprone/UndeclaredProcCheck.h"
#include "bugprone/UninitializedVarCheck.h"
#include "bugprone/UnusedIntentCheck.h"
#include "flang/Common/Fortran-features.h"
#include "flang/Common/default-kinds.h"
#include "flang/Parser/dump-parse-tree.h"
#include "flang/Parser/message.h"
#include "flang/Parser/parsing.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "modernize/AvoidAssignStmt.h"
#include "modernize/AvoidBackspaceStmt.h"
#include "modernize/AvoidCommonBlocks.h"
#include "modernize/AvoidDataConstructs.h"
#include "utils/SemanticsVisitor.h"
#include "llvm/ADT/SmallString.h"
// include llvm::sys::path
#include "FlangTidyModuleRegistry.h"
#include "MultiplexVisitor.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

#include "FlangTidyContext.h"

LLVM_INSTANTIATE_REGISTRY(Fortran::tidy::FlangTidyModuleRegistry)

namespace Fortran::tidy {

/* Checks to implement
 * 1. Warn about implicit declarations(implicit none) (done)
 * 2. Warn about intent(inout) variables that are never assigned a value (done)
 * 3. Warn about using the same variable with different capitalization (?)
 * 4. Warn about using common blocks (done)
 * 5. Warn about arithmetic goto statements (done)
 * 6. Warn about non allocated allocatable local variables (done)
 * 7. Warn about uninitialized variables that are used (same scope) (done?)
 * 8. Warn about precision loss in assignments (done)
 * 9. Warn about implicit function declarations - require explicit interface
 * 10. Keep track of derived types in a tree for uninitialized variables
 *
 * Codee checks:
 * PWR001 Declare global variables as function parameters
 * PWR003	Explicitly declare pure functions
 * PWR007	Disable implicit declaration of variables (done)
 * PWR008	Declare the intent for each procedure parameter (done)
 * PWR012	Pass only required fields from derived type as parameters
 * PWR063	Avoid using legacy Fortran constructs (semi done)
 * PWR071	Prefer real(kind=kind_value) for declaring consistent floating
 * types
 */

// Factory to populate MultiplexVisitor with all registered checks
class MultiplexVisitorFactory {
public:
  MultiplexVisitorFactory();

public:
  std::unique_ptr<FlangTidyCheckFactories> CheckFactories;
};

MultiplexVisitorFactory::MultiplexVisitorFactory()
    : CheckFactories(new FlangTidyCheckFactories) {
  // Traverse the FlangTidyModuleRegistry to register all checks
  for (auto &entry : FlangTidyModuleRegistry::entries()) {
    // Instantiate the module
    std::unique_ptr<FlangTidyModule> module = entry.instantiate();
    module->addCheckFactories(*CheckFactories);
  }
}

static std::string getIntrinsicDir(const char *argv) {
  // TODO: Find a system independent API
  llvm::SmallString<128> driverPath;
  driverPath.assign(llvm::sys::fs::getMainExecutable(argv, nullptr));
  llvm::sys::path::remove_filename(driverPath);
  driverPath.append("/../include/flang/");
  return std::string(driverPath);
}

int runFlangTidy(const FlangTidyOptions &options) {
  parser::AllSources allSources;
  parser::AllCookedSources allCookedSources{allSources};
  parser::Parsing parsing{allCookedSources};
  parser::Options parserOptions;

  parserOptions.intrinsicModuleDirectories.emplace_back(
      getIntrinsicDir(options.argv[0]));

  if (options.enableAllWarnings) {
    parserOptions.features.WarnOnAllNonstandard();
    parserOptions.expandIncludeLinesInPreprocessedOutput = false;
  }

  // process files
  parsing.Prescan(options.fileName, parserOptions);
  parsing.Parse(llvm::outs());

  if (parsing.messages().AnyFatalError()) {
    parsing.messages().Emit(llvm::errs(), allCookedSources);
    return 1;
  }

  // get parse tree
  parser::Program &program{*parsing.parseTree()};
  common::IntrinsicTypeDefaultKinds defaultKinds;
  common::LanguageFeatureControl languageFeatures;
  common::LangOptions langOptions;
  semantics::SemanticsContext semanticsContext{defaultKinds, languageFeatures,
                                               langOptions, allCookedSources};
  semantics::Semantics semantics{semanticsContext, program};

  semanticsContext.set_intrinsicModuleDirectories(
      parserOptions.intrinsicModuleDirectories);

  // semantic analysis
  if (!semantics.Perform()) {
    llvm::errs() << "Semantic analysis failed\n";
  }

  // is it fatal?
  if (semantics.AnyFatalError()) {
    semantics.EmitMessages(llvm::errs());
    return 1;
  }

  // -dump-parse-tree
  if (options.dumpParseTree) {
    parser::DumpTree(llvm::outs(), program);
  }

  FlangTidyContext context{options, &semanticsContext};

  MultiplexVisitorFactory visitorFactory{};

  MultiplexVisitor visitor{semanticsContext};

  auto checks = visitorFactory.CheckFactories->createChecks(&context);

  for (auto &&check : checks) {
    llvm::outs() << "Adding check " << check->name()
                 << " with addr: " << check.get() << "\n";
    visitor.AddChecker(std::move(check));
  }

  for (auto &&check : visitor.checkers_) {
    llvm::outs() << "check " << check->name()
                 << " with addr: " << check.get() << "\n";
  }

  visitor.Walk(program);

  llvm::outs() << "Done\n";

  /*
  // TODO: make a global context to enable/disable visitor-based checks
  utils::SemanticsVisitor<
      bugprone::ArithmeticGotoCheck, bugprone::ArithmeticIfStmtCheck,
      bugprone::ImplicitDeclCheck, bugprone::PrecisionLossCheck,
      bugprone::UndeclaredProcCheck, bugprone::UninitializedVarCheck,
      bugprone::UnusedIntentCheck, modernize::AvoidAssignStmtCheck,
      modernize::AvoidBackspaceStmtCheck, modernize::AvoidCommonBlocksCheck,
      modernize::AvoidDataConstructsCheck>
      visitor{semanticsContext};
  visitor.Walk(program);
   */
  semantics.EmitMessages(llvm::errs());

  llvm::outs() << "Done done" << "\n";

  return 0;
}

} // namespace Fortran::tidy
