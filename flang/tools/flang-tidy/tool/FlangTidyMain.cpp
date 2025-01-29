#include "FlangTidy.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"
#include <sstream>

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

static llvm::cl::list<std::string> ExtraArgs(
    "extra-arg", llvm::cl::desc("Additional arguments to pass to the frontend"),
    llvm::cl::ZeroOrMore, llvm::cl::sub(llvm::cl::SubCommand::getAll()));

extern int flangTidyMain(int argc, const char **argv) {
  llvm::InitLLVM X(argc, argv);

  llvm::cl::ParseCommandLineOptions(
      argc, argv, "flang-tidy: A Fortran source analysis tool\n");

  if (SourcePaths.empty()) {
    llvm::errs() << "Error: No input file specified. Use -file <filename>.\n";
    return 1;
  }

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
  for (const auto &arg : ExtraArgs) {
    std::istringstream stream(arg);
    std::string subArg;
    while (stream >> subArg) {
      options.extraArgs.push_back(subArg);
    }
  }

  return runFlangTidy(options);
}

} // namespace Fortran::tidy
