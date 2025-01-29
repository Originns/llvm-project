#include "FlangTidy.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include <sstream>

namespace Fortran::tidy {

// command line options
llvm::cl::list<std::string> FileOption(
    "file",
    llvm::cl::desc("Specify one or more Fortran source files to analyze"),
    llvm::cl::OneOrMore, llvm::cl::value_desc("filename"));

llvm::cl::opt<std::string>
    CheckOption("checks",
                llvm::cl::desc("Comma-separated list of checks to enable"),
                llvm::cl::init("*"), llvm::cl::value_desc("check list"));

llvm::cl::list<std::string>
    ExtraArgs("extra-arg",
              llvm::cl::desc("Additional arguments to pass to the frontend"),
              llvm::cl::ZeroOrMore, llvm::cl::value_desc("arg"));

llvm::cl::opt<bool> DumpParseTree("dump-parse-tree",
                                  llvm::cl::desc("Dump parse tree"),
                                  llvm::cl::init(false));

int flangTidyMain(int argc, const char **argv) {
  llvm::InitLLVM X(argc, argv);

  llvm::cl::ParseCommandLineOptions(
      argc, argv, "flang-tidy: A Fortran source analysis tool\n");

  if (FileOption.empty()) {
    llvm::errs() << "Error: No input file specified. Use -file <filename>.\n";
    return 1;
  }

  // only one file allowed for now
  if (FileOption.size() > 1) {
    llvm::errs() << "Error: Only one input file allowed.\n";
    return 1;
  }

  if (!llvm::sys::fs::exists(FileOption[0])) {
    llvm::errs() << "Error: File not found: " << FileOption[0] << "\n";
    return 1;
  }

  // parse command line options
  FlangTidyOptions options;
  options.fileName = FileOption[0];
  options.argv = argv;

  std::stringstream ss(CheckOption);
  std::string check;
  while (std::getline(ss, check, ',')) {
    options.enabledChecks.push_back(check);
  }

  options.dumpParseTree = DumpParseTree;
  options.extraArgs.assign(ExtraArgs.begin(), ExtraArgs.end());

  // run flang-tidy
  return runFlangTidy(options);
}

} // namespace Fortran::tidy
