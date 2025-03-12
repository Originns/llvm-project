#include "FlangTidy.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <sstream>
#include <vector>

// flang frontend driver
#include "flang/Frontend/CompilerInstance.h"
#include "flang/Frontend/CompilerInvocation.h"
#include "flang/Frontend/TextDiagnosticBuffer.h"

namespace Fortran::tidy {

static llvm::cl::list<std::string>
    SourcePaths(llvm::cl::Positional,
                llvm::cl::desc("<source0> [... <sourceN>]"),
                llvm::cl::OneOrMore, llvm::cl::value_desc("source files"),
                llvm::cl::sub(llvm::cl::SubCommand::getAll()));

static llvm::cl::opt<std::string>
    CheckOption("checks",
                llvm::cl::desc("Comma-separated list of checks to enable"),
                llvm::cl::init("*"), llvm::cl::value_desc("check list"));

static llvm::cl::list<std::string> ArgsBefore(
    "extra-arg-before",
    llvm::cl::desc(
        "Additional argument to prepend to the compiler command line"),
    llvm::cl::ZeroOrMore, llvm::cl::sub(llvm::cl::SubCommand::getAll()));

static llvm::cl::list<std::string>
    ArgsAfter("extra-arg",
              llvm::cl::desc(
                  "Additional argument to append to the compiler command line"),
              llvm::cl::ZeroOrMore,
              llvm::cl::sub(llvm::cl::SubCommand::getAll()));

std::string GetFlangToolCommand() {
  static int Dummy;
  std::string FlangExecutable =
      llvm::sys::fs::getMainExecutable("flang", (void *)&Dummy);
  llvm::SmallString<128> FlangToolPath;
  FlangToolPath = llvm::sys::path::parent_path(FlangExecutable);
  llvm::sys::path::append(FlangToolPath, "flang-tool");
  return std::string(FlangToolPath);
}

static bool stripPositionalArgs(std::vector<const char *> Args,
                                std::vector<std::string> &Result,
                                std::string &ErrorMsg) {
  auto flang = std::make_unique<Fortran::frontend::CompilerInstance>();

  // Create diagnostics engine
  flang->createDiagnostics();
  if (!flang->hasDiagnostics()) {
    llvm::errs() << "Failed to create diagnostics engine\n";
    return false;
  }

  // Capture diagnostics
  frontend::TextDiagnosticBuffer *diagsBuffer =
      new frontend::TextDiagnosticBuffer;
  llvm::IntrusiveRefCntPtr<clang::DiagnosticIDs> diagID(
      new clang::DiagnosticIDs());
  llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> diagOpts =
      new clang::DiagnosticOptions();
  clang::DiagnosticsEngine diags(diagID, &*diagOpts, diagsBuffer);

  // Insert Flang tool command
  std::string Argv0 = GetFlangToolCommand();
  Args.insert(Args.begin(), Argv0.c_str());

  // Add a dummy file to ensure at least one compilation job
  Args.push_back("placeholder.f90");

  // Remove -c flags if present (incompatible with flang, inserted by CMake)
  Args.erase(std::remove_if(
                 Args.begin(), Args.end(),
                 [](const char *arg) { return llvm::StringRef(arg) == "-c"; }),
             Args.end());

  // Create compiler invocation
  bool success = Fortran::frontend::CompilerInvocation::createFromArgs(
      flang->getInvocation(), Args, diags, Argv0.c_str());

  if (!success) {
    ErrorMsg = "Failed to create compiler invocation\n";
    // flush flang diagnostic
    diagsBuffer->flushDiagnostics(flang->getDiagnostics());
    return false;
  }

  // Get the list of input files from Flang's frontend options
  std::vector<std::string> inputs;
  for (const auto &input : flang->getFrontendOpts().inputs) {
    inputs.push_back(input.getFile().str());
  }

  if (inputs.empty()) {
    ErrorMsg = "warning: no compile jobs found\n";
    return false;
  }

  // Remove input files from Args
  std::vector<const char *>::iterator End = llvm::remove_if(
      Args, [&](llvm::StringRef S) { return llvm::is_contained(inputs, S); });

  // Store the filtered arguments
  Result = std::vector<std::string>(Args.begin(), End);
  return true;
}

static std::vector<std::string>
loadFromCommandLine(int &Argc, const char *const *Argv, std::string &ErrorMsg) {
  ErrorMsg.clear();
  if (Argc == 0)
    return {};
  const char *const *DoubleDash =
      std::find(Argv, Argv + Argc, llvm::StringRef("--"));
  if (DoubleDash == Argv + Argc)
    return {};
  std::vector<const char *> CommandLine(DoubleDash + 1, Argv + Argc);
  Argc = DoubleDash - Argv;

  std::vector<std::string> StrippedArgs;
  if (!stripPositionalArgs(CommandLine, StrippedArgs, ErrorMsg))
    return {};
  return StrippedArgs;
}

extern int flangTidyMain(int &argc, const char **argv) {
  llvm::InitLLVM X(argc, argv);

  std::string ErrorMessage;
  auto Compilations = loadFromCommandLine(argc, argv, ErrorMessage);

  if (!ErrorMessage.empty()) {
    llvm::outs() << ErrorMessage << "\n";
    return 1;
  }

  llvm::cl::ParseCommandLineOptions(
      argc, argv, "flang-tidy: A Fortran source analysis tool\n");

  // for all source paths, check if they exist
  for (const auto &sourcePath : SourcePaths) {
    if (!llvm::sys::fs::exists(sourcePath)) {
      llvm::errs() << "Error: File not found: " << sourcePath << "\n";
      return 1;
    }
  }

  FlangTidyOptions options;
  options.sourcePaths.assign(SourcePaths.begin(), SourcePaths.end());
  options.argv0 = argv[0];

  // add checks
  std::stringstream ss(CheckOption);
  std::string check;
  while (std::getline(ss, check, ',')) {
    options.enabledChecks.push_back(check);
  }

  options.extraArgs.clear();
  for (const auto &arg : ArgsBefore) {
    std::istringstream stream(arg);
    std::string subArg;
    while (stream >> subArg) {
      options.extraArgs.push_back(subArg);
    }
  }

  if (!Compilations.empty()) {
    assert(options.sourcePaths.size() == 1);

    llvm::outs() << "Compilations: ";
    for (const auto &comp : Compilations) {
      llvm::outs() << comp << " ";
    }
    llvm::outs() << "\n";

    options.extraArgs.insert(options.extraArgs.end(), Compilations.begin(),
                             Compilations.end());
  }

  for (const auto &arg : ArgsAfter) {
    std::istringstream stream(arg);
    std::string subArg;
    while (stream >> subArg) {
      options.extraArgs.push_back(subArg);
    }
  }

  // remove anything starting with --driver-mode
  options.extraArgs.erase(
      std::remove_if(options.extraArgs.begin(), options.extraArgs.end(),
                     [](std::string const &arg) {
                       return llvm::StringRef(arg).starts_with("--driver-mode");
                     }),
      options.extraArgs.end());

  return runFlangTidy(options);
}

} // namespace Fortran::tidy
