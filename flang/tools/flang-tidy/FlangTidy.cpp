#include "flang/Common/Fortran-features.h"
#include "flang/Common/default-kinds.h"
#include "flang/Parser/dump-parse-tree.h"
#include "flang/Parser/parsing.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "llvm/Support/raw_ostream.h"
#include <unordered_set>
#include <vector>

struct ParseTreeVisitor {
  template <typename A>
  bool Pre(const A &) {
    return true;
  }
  template <typename A>
  void Post(const A &) {}

  std::vector<std::unordered_set<std::string>> scopeStack;

  void PushScope() { scopeStack.emplace_back(); }

  void PopScope() {
    if (!scopeStack.empty()) {
      scopeStack.pop_back();
    }
  }

  void AddVariableToCurrentScope(const std::string &varName) {
    if (!scopeStack.empty()) {
      scopeStack.back().insert(varName);
    }
  }

  bool IsVariableDeclared(const std::string &varName) const {
    // iterate through the scopes from the most recent to the oldest
    for (auto it = scopeStack.rbegin(); it != scopeStack.rend(); ++it) {
      if (it->find(varName) != it->end()) {
        return true;
      }
    }
    return false;
  }

  bool Pre(const Fortran::parser::ImplicitStmt &implicit) {
    // check if we hold variant std::list<ImplicitNoneNameSpec>
    if (std::holds_alternative<
            std::list<Fortran::parser::ImplicitStmt::ImplicitNoneNameSpec>>(
            implicit.u)) {
      llvm::outs() << "Found implicit none in current scope\n";
    }
    return true;
  }

  bool Pre(const Fortran::parser::SubroutineSubprogram &) {
    llvm::outs() << "Entering Subroutine\n";
    PushScope();
    return true;
  }

  bool Pre(const Fortran::parser::FunctionSubprogram &) {
    llvm::outs() << "Entering Function\n";
    PushScope();
    return true;
  }

  bool Pre(const Fortran::parser::MainProgram &) {
    llvm::outs() << "Entering Program\n";
    PushScope();
    return true;
  }

  void Post(const Fortran::parser::SubroutineSubprogram &) {
    llvm::outs() << "Leaving Subroutine\n";
    PopScope();
  }

  void Post(const Fortran::parser::FunctionSubprogram &) {
    llvm::outs() << "Leaving Function\n";
    PopScope();
  }

  void Post(const Fortran::parser::MainProgram &) {
    llvm::outs() << "Leaving Program\n";
    PopScope();
  }

  bool Pre(const Fortran::parser::TypeDeclarationStmt &stmt) {
    const auto &entityList = std::get<2>(stmt.t);
    for (const auto &entity : entityList) {
      const auto &objectName = std::get<0>(entity.t);
      llvm::outs() << "Declaring variable: " << objectName.source.ToString()
                   << "\n";
      AddVariableToCurrentScope(objectName.ToString());
    }
    return true;
  }

  void Post(const Fortran::parser::AssignmentStmt &stmt) {
    const auto &variable = std::get<0>(stmt.t);

    // get the variable name from the designator inside the variable
    if (const auto *designator = std::get_if<
            Fortran::common::Indirection<Fortran::parser::Designator>>(
            &variable.u)) {
      const std::string varName = designator->value().source.ToString();

      // check if the variable is declared in the current or any enclosing scope
      if (!IsVariableDeclared(varName)) {
        llvm::errs() << "Warning: Variable '" << varName
                     << "' is used without an explicit declaration.\n";
      }
    }
  }
};

int main(int argc, char *argv[]) {
  Fortran::parser::AllSources allSources;
  Fortran::parser::AllCookedSources allCookedSources{allSources};
  Fortran::parser::Parsing parsing{allCookedSources};
  Fortran::parser::Options options;
  options.features
      .WarnOnAllNonstandard();

  // parse files
  for (int i = 1; i < argc; ++i) {
    parsing.Prescan(argv[i], options);
    parsing.Parse(llvm::outs());

    // if we encountered an error while parsing, warn about it
    if (parsing.messages().AnyFatalError()) {
      parsing.messages().Emit(llvm::errs(), allCookedSources);
      continue;
    }

    Fortran::parser::Program &program{*parsing.parseTree()};

    // create the parse tree visitor walk the tree
    ParseTreeVisitor visitor;
    Fortran::parser::Walk(program, visitor);

    // create semantics context and semantics objects
    Fortran::common::IntrinsicTypeDefaultKinds defaultKinds;
    Fortran::common::LanguageFeatureControl languageFeatures;
    Fortran::semantics::SemanticsContext semanticsContext{
        defaultKinds, languageFeatures, allCookedSources};
    Fortran::semantics::Semantics semantics{semanticsContext, program};

    // dump the parse tree
    Fortran::parser::DumpTree(llvm::outs(), program);

    // perform semantic analysis
    if (!semantics.Perform()) {
      llvm::errs() << "Semantic analysis failed\n";
      semantics.EmitMessages(llvm::errs());
    } else {
      llvm::outs() << "Semantic analysis succeeded\n";
      Fortran::parser::DumpTree(llvm::outs(), program);
    }
  }

  return 0;
}
