#include "FlangTidy.h"
#include "FlangTidyModule.h"
#include "FlangTidyModuleRegistry.h"
#include "MultiplexVisitor.h"
#include "flang/Common/Fortran-features.h"
#include "flang/Common/default-kinds.h"
#include "flang/Parser/dump-parse-tree.h"
#include "flang/Parser/message.h"
#include "flang/Parser/parsing.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

// llvm make array ref
#include "llvm/ADT/ArrayRef.h"

#include "flang/Frontend/CompilerInstance.h"
#include "flang/Frontend/CompilerInvocation.h"
#include "flang/Frontend/TextDiagnosticBuffer.h"
#include "flang/FrontendTool/Utils.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/TargetSelect.h"

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

int runFlangTidy(const FlangTidyOptions &options) {
  auto flang = std::make_unique<Fortran::frontend::CompilerInstance>();

  // create diagnostics engine
  flang->createDiagnostics();
  if (!flang->hasDiagnostics()) {
    llvm::errs() << "Failed to create diagnostics engine\n";
    return 1;
  }

  // convert the filename to StringRef
  llvm::StringRef fileName(options.fileName);

  // pass the files
  flang->getFrontendOpts().inputs.emplace_back(frontend::FrontendInputFile{
    fileName, frontend::Language::Fortran});

  frontend::TextDiagnosticBuffer *diagsBuffer = new frontend::TextDiagnosticBuffer;
  llvm::IntrusiveRefCntPtr<clang::DiagnosticIDs> diagID(
    new clang::DiagnosticIDs());
  llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> diagOpts =
      new clang::DiagnosticOptions();
  clang::DiagnosticsEngine diags(diagID, &*diagOpts, diagsBuffer);

  // convert the options to a format that can be passed to the compiler invocation
  llvm::ArrayRef<const char *> argv;
  std::vector<std::string> args = options.extraArgs;

  // print extraArgs
  for (const auto &arg : args) {
    llvm::outs() << arg << "\n";
  }

  // turn extra args into a format that can be passed to the compiler invocation
  std::vector<const char *> cstrArgs;
  // add input files
  cstrArgs.push_back(options.fileName.c_str());

  for (const auto &arg : args) {
    cstrArgs.push_back(arg.c_str());
  }

  argv = llvm::ArrayRef<const char *>(cstrArgs);
  flang->getFrontendOpts().programAction = frontend::ParseSyntaxOnly;

  // parse arguments
  const char *argv0 = options.argv[0];
  bool success = Fortran::frontend::CompilerInvocation::createFromArgs(
      flang->getInvocation(), argv, diags, argv0);

  // initialize targets
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();

  // emit diagnostics
  diagsBuffer->flushDiagnostics(flang->getDiagnostics());

  if (!success) {
    llvm::errs() << "Failed to parse arguments\n";
    return 1;
  }

  // run the compiler instance
  if (!Fortran::frontend::executeCompilerInvocation(flang.get())) {
    llvm::errs() << "Failed to execute compiler invocation\n";
    return 1;
  }

  // handle help and version flags
  if (flang->getFrontendOpts().showHelp ||
      flang->getFrontendOpts().showVersion) {
    return 0;
  }

  // get the parse tree
  auto &parsing = flang->getParsing();
  auto &parseTree = parsing.parseTree();
  if (!parseTree) {
    llvm::errs() << "Failed to retrieve the parse tree\n";
    return 1;
  }

  auto &semantics = flang->getSemantics();
  auto &semanticsContext = semantics.context();

  FlangTidyContext context{options, &semanticsContext};

  MultiplexVisitorFactory visitorFactory{};
  MultiplexVisitor visitor{semanticsContext};
  auto checks = visitorFactory.CheckFactories->createChecks(&context);

  for (auto &&check : checks) {
    visitor.AddChecker(std::move(check));
  }

  visitor.Walk(*parseTree);  

  semantics.EmitMessages(llvm::errs());
  //diagsBuffer->flushDiagnostics(flang->getDiagnostics());

  return 0;
}

} // namespace Fortran::tidy
