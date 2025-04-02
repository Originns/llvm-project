#include "UnusedIntentCheck.h"
#include "flang/Semantics/attr.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

// a map mapping a symbol to its ProcBindingDetails
static std::unordered_map<const semantics::Symbol *, const semantics::Symbol *>
    procBindingDetailsSymbolsMap;

void UnusedIntentCheck::CheckUnusedIntentHelper(semantics::SemanticsContext &context,
                                    const semantics::Scope &scope) {
  if (scope.IsModuleFile())
    return;

  auto WasDefined{[&context](const semantics::Symbol &symbol) {
    return context.IsSymbolDefined(symbol) ||
           semantics::IsInitialized(symbol, /*ignoreDataStatements=*/false,
                                    /*ignoreAllocatable=*/false,
                                    /*ignorePointer=*/false);
  }};
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    // is the symbol a function argument
    if (const auto *details{symbol.detailsIf<semantics::ObjectEntityDetails>()};
        details && details->isDummy()) {
      // TODO: check if we are used in a derived procedure (procedure => A_f)

      const auto &owningProcScope = symbol.owner();
      // get the class of the symbol

      const auto &owningProc = owningProcScope.symbol();

      // if the owning proc is a derived type ignore it
      /*
      maybe check if its actually overridden
      bool isInaccessibleDeferred{false};
      const auto *overridden{semantics::FindOverriddenBinding(
          *owningProc, isInaccessibleDeferred)};
      if (overridden) {
        llvm::outs() << "Overridden\n";
        return;
      }
      */
      if (procBindingDetailsSymbolsMap.find(owningProc) !=
          procBindingDetailsSymbolsMap.end()) {
        continue;
        /* TODO: this
        const auto *sym = procBindingDetailsSymbolsMap.at(owningProc);
        bool isInaccessibleDeferred{false};
        const auto *overridden{
            semantics::FindOverriddenBinding(*sym, isInaccessibleDeferred)};
        if (overridden) {
          llvm::outs() << "method " << owningProc->name()
                       << " is overridden by " << overridden->name() << "\n";
          continue;
        }
        */
      }

      if (!WasDefined(symbol) && semantics::IsIntentInOut(symbol)) {
        Say(
            symbol.name(),
            "Dummy argument '%s' with intent(inout) is never written to, consider changing to intent(in)"_warn_en_US,
            symbol.name());
      }
      if (!symbol.attrs().HasAny({semantics::Attr::INTENT_IN,
                                  semantics::Attr::INTENT_INOUT,
                                  semantics::Attr::INTENT_OUT})) {
        // warn about dummy arguments without explicit intent
        Say(symbol.name(),
                    "Dummy argument '%s' has no explicit intent"_warn_en_US,
                    symbol.name());
      }
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckUnusedIntentHelper(context, child);
  }
}

static void MakeProcBindingSymbolSet(semantics::SemanticsContext &context,
                                     const semantics::Scope &scope) {
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    if (auto *details{symbol.detailsIf<semantics::ProcBindingDetails>()}) {
      procBindingDetailsSymbolsMap[&details->symbol()] = &symbol;
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    MakeProcBindingSymbolSet(context, child);
  }
}

UnusedIntentCheck::UnusedIntentCheck(llvm::StringRef name,
                                     FlangTidyContext *context)
    : FlangTidyCheck{name, context} {

  // go through all scopes, check if they own a derived type with
  // ProcBindingDetails and make a list of those
  MakeProcBindingSymbolSet(context->getSemanticsContext(),
                           context->getSemanticsContext().globalScope());

  CheckUnusedIntentHelper(context->getSemanticsContext(),
                          context->getSemanticsContext().globalScope());
}

} // namespace Fortran::tidy::bugprone
