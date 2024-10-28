#include "FlangTidy.h"
#include "bugprone/ArithmeticGotoCheck.h"
#include "bugprone/ImplicitDeclCheck.h"
#include "bugprone/UnusedIntentCheck.h"
#include "flang/Common/Fortran-features.h"
#include "flang/Common/default-kinds.h"
#include "flang/Parser/dump-parse-tree.h"
#include "flang/Parser/message.h"
#include "flang/Parser/parsing.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "modernize/AvoidCommonBlocks.h"
#include "utils/SemanticsVisitor.h"
#include "llvm/Support/raw_ostream.h"

namespace Fortran::tidy {

int runFlangTidy(const FlangTidyOptions &options) {
  parser::AllSources allSources;
  parser::AllCookedSources allCookedSources{allSources};
  parser::Parsing parsing{allCookedSources};
  parser::Options parserOptions;

  if (options.enableAllWarnings) {
    parserOptions.features.WarnOnAllNonstandard();
  }

  // process files
  for (const auto &fileName : options.fileNames) {
    // parse file
    parsing.Prescan(fileName, parserOptions);
    parsing.Parse(llvm::outs());

    if (parsing.messages().AnyFatalError()) {
      parsing.messages().Emit(llvm::errs(), allCookedSources);
      continue;
    }

    // get parse tree
    parser::Program &program{*parsing.parseTree()};
    common::IntrinsicTypeDefaultKinds defaultKinds;
    common::LanguageFeatureControl languageFeatures;
    common::LangOptions langOptions;
    semantics::SemanticsContext semanticsContext{defaultKinds, languageFeatures,
                                                 langOptions, allCookedSources};
    semantics::Semantics semantics{semanticsContext, program};

    // semantic analysis
    if (!semantics.Perform()) {
      llvm::errs() << "Semantic analysis failed\n";
    }

    // -dump-parse-tree
    if (options.dumpParseTree) {
      parser::DumpTree(llvm::outs(), program);
    }

    // run checks
    if (std::find(options.enabledChecks.begin(), options.enabledChecks.end(),
                  "implicit-declaration") != options.enabledChecks.end() ||
        options.enableAllWarnings) {
      CheckImplicitDecl(semanticsContext);
    }

    // TODO: make a global context to enable/disable visitor-based checks
    utils::SemanticsVisitor<UnusedIntentCheck, AvoidCommonBlocksCheck,
                            ArithmeticGotoCheck>
        visitor{semanticsContext};
    visitor.Walk(program);

    semantics.EmitMessages(llvm::errs());
  }

  return 0;
}

} // namespace Fortran::tidy
