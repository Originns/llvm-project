#include "UnusedUseCheck.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/scope.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::misc {

// a function to get a module symbol by recursively searching a symbols owner
static const semantics::Symbol *
GetModuleSymbol(const semantics::Symbol &symbol) {
  if (symbol.test(semantics::Symbol::Flag::ModFile)) {
    return &symbol;
  }
  if (!symbol.owner().IsTopLevel()) {
    return GetModuleSymbol(*symbol.owner().symbol());
  }
  return nullptr;
}

static std::set<const semantics::Symbol *> usedModules;

static void MakeUsedModulesList(const semantics::Scope &scope) {
  if (!scope.IsModule()) {
    for (const auto &pair : scope) {
      const semantics::Symbol &symbol = *pair.second;
      // only check the symbol if we are in our source file and not the module
      // file

      // if the symbol is from a module, get the module
      if (symbol.has<semantics::UseDetails>()) {
        llvm::outs() << "symbol has use details: " << symbol.name().ToString()
                     << "\n";
        // the ultimate must be different from the symbol, else its inside the
        // module

        const auto &ultimate = symbol.GetUltimate();
        if (&ultimate != &symbol) {
          const auto *module{GetModuleSymbol(ultimate)};
          if (module) {
            llvm::outs() << "Adding module: " << module->name().ToString()
                         << "\n";
            usedModules.insert(module);
          }
        }
      }
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    MakeUsedModulesList(child);
  }
}

UnusedUseCheck::UnusedUseCheck(llvm::StringRef name, FlangTidyContext *context)
    : FlangTidyCheck{name, context} {
  MakeUsedModulesList(context->getSemanticsContext().globalScope());
}

using namespace parser::literals;
void UnusedUseCheck::Leave(const parser::UseStmt &useStmt) {
  const auto *symbol{useStmt.moduleName.symbol};
  if (!symbol || !context()->getSemanticsContext().location()) {
    return;
  }

  // print the thing
  llvm::outs() << "symbol: " << symbol->name().ToString() << "\n";

  // if the symbol is not in our list, warn
  if (usedModules.find(symbol) == usedModules.end()) {
    Say(
        *(context()->getSemanticsContext().location()),
        "use of module '%s' is not needed"_warn_en_US,
        symbol->name().ToString());
  }
}

void UnusedUseCheck::Leave(const parser::Program &) {}

} // namespace Fortran::tidy::misc
