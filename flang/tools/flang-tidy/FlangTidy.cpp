#include "FlangTidy.h"
#include "bugprone/ArithmeticGotoCheck.h"
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
#include "modernize/AvoidCommonBlocks.h"
#include "utils/SemanticsVisitor.h"
#include "llvm/Support/raw_ostream.h"

namespace Fortran::tidy {

/* Checks to implement
 * 1. Warn about implicit declarations(implicit none) (done)
 * 2. Warn about intent(inout) variables that are never assigned a value (done)
 * 3. Warn about using the same variable with different capitalization (?)
 * 4. Warn about using common blocks (done)
 * 5. Warn about arithmetic goto statements (done)
 * 6. Warn about non allocated allocatable local variables (in the same scope)
 * 7. Warn about uninitialized variables that are used (same scope) (done?)
 * 8. Warn about short circuit statements if (allocated(a) and a(1) == 0)
 * 9. Warn about precision loss in assignments (done)
 * 10. Warn about implicit function declarations (done)
 * 11. Warn about MPI asynchronus function calls with temporary buffers
 * (stack-array)
 * 12. Dont use include, instead use "use" (preprocessor flags, (for now
 * "mpif.h" to "use mpi(_f08)"))
 */

int runFlangTidy(const FlangTidyOptions &options) {
  parser::AllSources allSources;
  parser::AllCookedSources allCookedSources{allSources};
  parser::Parsing parsing{allCookedSources};
  parser::Options parserOptions;

  if (options.enableAllWarnings) {
    parserOptions.features.WarnOnAllNonstandard();
    parserOptions.expandIncludeLinesInPreprocessedOutput = false;
  }

  // process files
  for (const auto &fileName : options.fileNames) {
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
      bugprone::CheckImplicitDecl(semanticsContext);
    }

    // implicit proc decl
    if (std::find(options.enabledChecks.begin(), options.enabledChecks.end(),
                  "undeclared-proc") != options.enabledChecks.end() ||
        options.enableAllWarnings) {
      bugprone::CheckUndeclaredProc(semanticsContext);
    }

    // unused intent
    if (std::find(options.enabledChecks.begin(), options.enabledChecks.end(),
                  "unused-intent") != options.enabledChecks.end() ||
        options.enableAllWarnings) {
      bugprone::CheckUnusedIntent(semanticsContext);
    }

    // TODO: make a global context to enable/disable visitor-based checks
    utils::SemanticsVisitor<
        bugprone::ArithmeticGotoCheck, bugprone::PrecisionLossCheck,
        bugprone::UninitializedVarCheck, modernize::AvoidCommonBlocksCheck>
        visitor{semanticsContext};
    visitor.Walk(program);

    semantics.EmitMessages(llvm::errs());
  }

  return 0;
}

} // namespace Fortran::tidy
