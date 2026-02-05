//===--- FlangTidyMain.cpp - flang-tidy -----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "../FlangTidy.h"
#include "../FlangTidyForceLinker.h"
#include "../FlangTidyOptions.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TargetParser/Host.h"
#include <cassert>
#include <clang/Tooling/CompilationDatabase.h>
#include <sstream>
#include <vector>

namespace Fortran::tidy {

static llvm::cl::opt<std::string>
    BuildPath("p", llvm::cl::desc("Build path"), llvm::cl::Optional,
              llvm::cl::ZeroOrMore,
              llvm::cl::sub(llvm::cl::SubCommand::getAll()));

static llvm::cl::list<std::string>
    SourcePaths(llvm::cl::Positional,
                llvm::cl::desc("<source0> [... <sourceN>]"),
                llvm::cl::OneOrMore, llvm::cl::value_desc("source files"),
                llvm::cl::sub(llvm::cl::SubCommand::getAll()));

static llvm::cl::opt<std::string>
    CheckOption("checks",
                llvm::cl::desc("Comma-separated list of checks to enable. "
                               "Overrides configuration file settings."),
                llvm::cl::init(""), llvm::cl::value_desc("check list"));

static llvm::cl::opt<std::string> ConfigOption(
    "config",
    llvm::cl::desc("Specify configuration in YAML format: "
                   "-config=\"{Checks: '*', ...}\" "
                   "When empty, flang-tidy will look for .flang-tidy files."),
    llvm::cl::init(""), llvm::cl::value_desc("yaml config"));

static llvm::cl::opt<std::string> ConfigFile(
    "config-file",
    llvm::cl::desc("Specify the path of .flang-tidy or custom config file"),
    llvm::cl::init(""), llvm::cl::value_desc("filename"));

static llvm::cl::opt<bool>
    DumpConfig("dump-config",
               llvm::cl::desc("Dump configuration in YAML format to stdout"),
               llvm::cl::init(false));

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

static llvm::cl::opt<std::string>
    WarningsAsErrors("warnings-as-errors",
                     llvm::cl::desc("Comma-separated list of checks for which "
                                    "to turn warnings into errors"),
                     llvm::cl::init(""));

static std::unique_ptr<FlangTidyOptionsProvider>
createOptionsProvider(llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FS) {
  FlangTidyGlobalOptions GlobalOptions;

  FlangTidyOptions DefaultOptions = FlangTidyOptions::getDefaults();

  FlangTidyOptions OverrideOptions;
  if (!CheckOption.empty())
    OverrideOptions.Checks = CheckOption;

  if (!WarningsAsErrors.empty())
    OverrideOptions.WarningsAsErrors = WarningsAsErrors;

  auto LoadConfig =
      [&](llvm::StringRef Configuration,
          llvm::StringRef Source) -> std::unique_ptr<FlangTidyOptionsProvider> {
    llvm::ErrorOr<FlangTidyOptions> ParsedConfig =
        parseConfiguration(llvm::MemoryBufferRef(Configuration, Source));
    if (ParsedConfig)
      return std::make_unique<ConfigOptionsProvider>(
          std::move(GlobalOptions),
          FlangTidyOptions::getDefaults().merge(DefaultOptions, 0),
          std::move(*ParsedConfig), std::move(OverrideOptions), std::move(FS));
    llvm::errs() << "Error: invalid configuration specified.\n"
                 << ParsedConfig.getError().message() << "\n";
    return nullptr;
  };

  if (!ConfigFile.empty()) {
    if (!ConfigOption.empty()) {
      llvm::errs() << "Error: --config-file and --config are "
                      "mutually exclusive. Specify only one.\n";
      return nullptr;
    }

    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> Text =
        llvm::MemoryBuffer::getFile(ConfigFile);
    if (std::error_code EC = Text.getError()) {
      llvm::errs() << "Error: can't read config-file '" << ConfigFile
                   << "': " << EC.message() << "\n";
      return nullptr;
    }

    return LoadConfig((*Text)->getBuffer(), ConfigFile);
  }

  if (!ConfigOption.empty())
    return LoadConfig(ConfigOption, "<command-line-config>");

  return std::make_unique<FileOptionsProvider>(
      std::move(GlobalOptions), std::move(DefaultOptions),
      std::move(OverrideOptions), std::move(FS));
}

static void filterArgsForFC1(std::vector<std::string> &Args) {
  if (Args.empty())
    return;

  llvm::SmallVector<const char *, 64> Argv;
  Argv.reserve(Args.size());
  for (const auto &S : Args)
    Argv.push_back(S.c_str());

  unsigned MissingArgIndex = 0, MissingArgCount = 0;

  llvm::opt::InputArgList Parsed = clang::getDriverOptTable().ParseArgs(
      Argv, MissingArgIndex, MissingArgCount,
      llvm::opt::Visibility(clang::options::FC1Option));

  llvm::opt::ArgStringList Rendered;
  for (const llvm::opt::Arg *A : Parsed) {
    // drop invalid options
    if (A->getOption().getKind() == llvm::opt::Option::UnknownClass)
      continue;

    A->render(Parsed, Rendered);
  }

  std::vector<std::string> Filtered;
  Filtered.reserve(Rendered.size());
  for (const char *S : Rendered) {
    Filtered.emplace_back(S);
  }

  Args.swap(Filtered);
}

extern int flangTidyMain(int &argc, const char **argv) {
  llvm::InitLLVM X(argc, argv);

  std::string ErrorMessage;
  std::unique_ptr<clang::tooling::CompilationDatabase> Compilations;
  Compilations = clang::tooling::FixedCompilationDatabase::loadFromCommandLine(
      argc, argv, ErrorMessage);

  if (!ErrorMessage.empty()) {
    llvm::outs() << ErrorMessage << "\n";
    return 1;
  }

  llvm::cl::ParseCommandLineOptions(
      argc, argv, "flang-tidy: A Fortran source analysis tool\n");

  llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> BaseFS =
      llvm::vfs::getRealFileSystem();

  auto OwningOptionsProvider = createOptionsProvider(BaseFS);
  auto *OptionsProvider = OwningOptionsProvider.get();
  if (!OptionsProvider)
    return 1;

  llvm::StringRef FileName = "dummy.f90";
  if (!SourcePaths.empty())
    FileName = SourcePaths[0];

  FlangTidyOptions EffectiveOptions = OptionsProvider->getOptions(FileName);

  if (DumpConfig) {
    llvm::outs() << configurationAsText(EffectiveOptions) << "\n";
    return 0;
  }

  bool usingFixed = false;
  if (!Compilations) {
    // llvm::outs() << "Searching for compilation database...\n";
    if (!BuildPath.empty()) {
      // llvm::outs() << "Using build path from command line: " << BuildPath
      //              << "\n";
      Compilations =
          clang::tooling::CompilationDatabase::autoDetectFromDirectory(
              BuildPath, ErrorMessage);
    } else {
      // llvm::outs() << "Using build path from source file: " << SourcePaths[0]
      //              << "\n";
      Compilations = clang::tooling::CompilationDatabase::autoDetectFromSource(
          SourcePaths[0], ErrorMessage);
    }
    if (!Compilations) {
      // llvm::errs() << "Error while trying to load a compilation database:\n"
      //              << ErrorMessage << "Running without flags.\n";
      Compilations.reset(new clang::tooling::FixedCompilationDatabase(
          ".", std::vector<std::string>()));
    }
  } else {
    // llvm::outs() << "Using compilation database from command line.\n";
    usingFixed = true;
  }

  EffectiveOptions.sourcePaths.assign(SourcePaths.begin(), SourcePaths.end());
  EffectiveOptions.argv0 = argv[0];

  EffectiveOptions.parseChecksString();
  EffectiveOptions.parseWarningsAsErrorsString();

  for (const auto &sourcePath : EffectiveOptions.sourcePaths) {
    if (!llvm::sys::fs::exists(sourcePath)) {
      llvm::errs() << "Error: File not found: " << sourcePath << "\n";
      return 1;
    }
  }

  if (Compilations) {
    assert(EffectiveOptions.sourcePaths.size() == 1);
    if (!EffectiveOptions.ExtraArgs)
      EffectiveOptions.ExtraArgs = std::vector<std::string>();
    std::vector<clang::tooling::CompileCommand> commands;
    if (usingFixed) {
      // llvm::outs() << "Compilation database is FixedCompilationDatabase.\n";
      auto fixedCommands =
          Compilations->getCompileCommands("").front().CommandLine;
      // remove the first argument which is the source file
      for (size_t i = 1; i < fixedCommands.size(); ++i) {
        EffectiveOptions.ExtraArgs->push_back(fixedCommands[i]);
      }
    }

    llvm::SmallString<128> NativeFilePath;
    llvm::sys::path::native(EffectiveOptions.sourcePaths[0], NativeFilePath);

    for (const auto &cmd : Compilations->getAllCompileCommands()) {
      llvm::SmallString<128> CmdFilePath;
      llvm::sys::path::native(cmd.Filename, CmdFilePath);
      if (CmdFilePath == NativeFilePath) {
        commands.push_back(cmd);
        break;
      }
    }

    if (!commands.empty()) {
      const auto &command = commands.front();
      for (const auto &arg : command.CommandLine) {
        EffectiveOptions.ExtraArgs->push_back(arg);
      }
    }
  }

  for (const auto &arg : ArgsBefore) {
    std::istringstream stream(arg);
    std::string subArg;
    while (stream >> subArg) {
      if (!EffectiveOptions.ExtraArgsBefore)
        EffectiveOptions.ExtraArgsBefore = std::vector<std::string>();
      EffectiveOptions.ExtraArgsBefore->push_back(subArg);
    }
  }

  for (const auto &arg : ArgsAfter) {
    std::istringstream stream(arg);
    std::string subArg;
    while (stream >> subArg) {
      if (!EffectiveOptions.ExtraArgs)
        EffectiveOptions.ExtraArgs = std::vector<std::string>();
      EffectiveOptions.ExtraArgs->push_back(subArg);
    }
  }

  if (EffectiveOptions.ExtraArgs)
    filterArgsForFC1(*EffectiveOptions.ExtraArgs);

  if (EffectiveOptions.ExtraArgsBefore)
    filterArgsForFC1(*EffectiveOptions.ExtraArgsBefore);

  return runFlangTidy(EffectiveOptions);
}

} // namespace Fortran::tidy
