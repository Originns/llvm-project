#include "FlangTidy.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include <sstream>

namespace Fortran::tidy {

// command line options
llvm::cl::list<std::string> FileOption(
    "file", llvm::cl::desc("Specify one or more Fortran source files to analyze"),
    llvm::cl::OneOrMore, llvm::cl::value_desc("filename"));

llvm::cl::opt<std::string> CheckOption(
    "checks", llvm::cl::desc("Comma-separated list of checks to enable"),
    llvm::cl::init("*"), llvm::cl::value_desc("check list"));

llvm::cl::list<std::string> ExtraArgs(
    "extra-arg", llvm::cl::desc("Additional arguments to pass to the frontend"),
    llvm::cl::ZeroOrMore, llvm::cl::value_desc("arg"));

llvm::cl::opt<bool> DumpParseTree(
    "dump-parse-tree", llvm::cl::desc("Dump parse tree"), llvm::cl::init(false));

llvm::cl::opt<bool> EnableAllWarnings(
    "Wall", llvm::cl::desc("Enable all warnings"), llvm::cl::init(false));

int flangTidyMain(int argc, const char **argv) { 
    llvm::cl::ParseCommandLineOptions(argc, argv, "flang-tidy: A Fortran source analysis tool\n");

    if (FileOption.empty()) {
        llvm::errs() << "Error: No input files specified. Use -file <filename>.\n";
        return 1;
    }

    // parse command line options
    FlangTidyOptions options;
    options.fileNames.assign(FileOption.begin(), FileOption.end());

    std::stringstream ss(CheckOption);
    std::string check;
    while (std::getline(ss, check, ',')) {
        options.enabledChecks.push_back(check);
    }
    
    options.dumpParseTree = DumpParseTree;
    options.enableAllWarnings = EnableAllWarnings;
    options.extraArgs.assign(ExtraArgs.begin(), ExtraArgs.end());

    // run flang-tidy
    return runFlangTidy(options);
}

} // namespace Fortran::tidy
