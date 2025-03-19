#include "PureProcedureCheck.h"
#include "flang/Evaluate/tools.h"
#include "flang/Semantics/scope.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"

namespace Fortran::tidy::misc {

static void PopulateProcedures(
    const semantics::Scope &scope,
    std::unordered_map<const semantics::Symbol *, bool> &pureProcedures) {
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol{*pair.second};
    if (semantics::IsProcedure(symbol)) {
      pureProcedures[&symbol.GetUltimate()] = true;
    }
  }
  for (const auto &child : scope.children()) {
    PopulateProcedures(child, pureProcedures);
  }
}

static bool CheckSymbolIsPure(const semantics::Symbol &symbol) {
  // get the procedure
  // if (semantics::IsProcedure(unit)) {
  if (!semantics::FindCommonBlockContaining(symbol) && IsSaved(symbol)) {
    return false;
  }
  if (symbol.attrs().test(semantics::Attr::VOLATILE) &&
      (IsDummy(symbol) /*|| !InInterface()*/)) {
    return false;
  }
  // if (innermostSymbol_ && innermostSymbol_->name() == "__builtin_c_funloc")
  //  The intrinsic procedure C_FUNLOC() gets a pass on this check.
  if (IsProcedure(symbol) && !IsPureProcedure(symbol) && IsDummy(symbol)) {
    return false;
  }
  //}
  return true;
}

static void CheckPureSymbols(
    const semantics::Scope &scope,
    std::unordered_map<const semantics::Symbol *, bool> &pureProcedures) {
  if (!scope.IsTopLevel() && !scope.IsModuleFile()) {
    for (const auto &pair : scope) {
      const semantics::Symbol &symbol{*pair.second};
      const semantics::Scope &scope{symbol.owner()};
      const semantics::Scope &unit{GetProgramUnitContaining(scope)};
      if (!CheckSymbolIsPure(symbol)) {
        pureProcedures[&unit.symbol()->GetUltimate()] = false;
      }
    }
  }
  for (const auto &child : scope.children()) {
    CheckPureSymbols(child, pureProcedures);
  }
}

static std::unordered_map<const semantics::Symbol *, const semantics::Symbol *>
    procBindingDetailsSymbolsMap;

static void MakeProcBindingSymbolSet(semantics::SemanticsContext &context,
                                     const semantics::Scope &scope) {
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    if (auto *details{symbol.detailsIf<semantics::ProcBindingDetails>()}) {
      procBindingDetailsSymbolsMap[&details->symbol().GetUltimate()] = &symbol;
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    MakeProcBindingSymbolSet(context, child);
  }
}

PureProcedureCheck::PureProcedureCheck(llvm::StringRef name,
                                       FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {
  MakeProcBindingSymbolSet(context_->getSemanticsContext(),
                           context_->getSemanticsContext().globalScope());

  PopulateProcedures(context_->getSemanticsContext().globalScope(),
                     pureProcedures_);
  CheckPureSymbols(context_->getSemanticsContext().globalScope(),
                   pureProcedures_);
}

using namespace parser::literals;
void PureProcedureCheck::SetUnpure() {
  // if theres no location, we cant do anything
  const auto location{context_->getSemanticsContext().location()};
  if (!location) {
    return;
  }

  const semantics::Scope &scope{
      context_->getSemanticsContext().FindScope(*location)};

  if (scope.IsTopLevel())
    return;

  const semantics::Scope &unit{GetProgramUnitContaining(scope)};
  const auto &ultimateSymbol = unit.symbol()->GetUltimate();

  if (semantics::IsProcedure(unit)) {
    // mark the procedure as impure
    pureProcedures_[&ultimateSymbol] = false;
  }

  // Ensure impurity is applied to all related symbols (BaseType & DerivedType)
  for (const auto &pair : procBindingDetailsSymbolsMap) {
    if (pair.first == &ultimateSymbol) {
      pureProcedures_[pair.second] = false;
    }
  }
}

// C1596: external I/O
void PureProcedureCheck::Leave(const parser::BackspaceStmt &) { SetUnpure(); }
void PureProcedureCheck::Leave(const parser::CloseStmt &) { SetUnpure(); }
void PureProcedureCheck::Leave(const parser::EndfileStmt &) { SetUnpure(); }
void PureProcedureCheck::Leave(const parser::FlushStmt &) { SetUnpure(); }
void PureProcedureCheck::Leave(const parser::InquireStmt &) { SetUnpure(); }
void PureProcedureCheck::Leave(const parser::OpenStmt &) { SetUnpure(); }
void PureProcedureCheck::Leave(const parser::PrintStmt &) { SetUnpure(); }
void PureProcedureCheck::Leave(const parser::RewindStmt &) { SetUnpure(); }
void PureProcedureCheck::Leave(const parser::WaitStmt &) { SetUnpure(); }
// C1597: read/write
void PureProcedureCheck::Leave(const parser::ReadStmt &) { SetUnpure(); }
void PureProcedureCheck::Leave(const parser::WriteStmt &) { SetUnpure(); }

// assignment
static bool IsPointerDummyOfPureFunction(const semantics::Symbol &x) {
  return IsPointerDummy(x) && FindPureProcedureContaining(x.owner()) &&
         x.owner().symbol() && IsFunction(*x.owner().symbol());
}

static const char *WhyBaseObjectIsSuspicious(const semantics::Symbol &x,
                                             const semantics::Scope &scope) {
  if (IsHostAssociatedIntoSubprogram(x, scope)) {
    return "host-associated";
  }
  if (IsUseAssociated(x, scope)) {
    return "USE-associated";
  }
  if (IsPointerDummyOfPureFunction(x)) {
    return "a POINTER dummy argument of a pure function";
  }
  if (IsIntentIn(x)) {
    return "an INTENT(IN) dummy argument";
  }
  if (FindCommonBlockContaining(x)) {
    return "in a COMMON block";
  }
  return nullptr;
}

static std::optional<std::string>
GetPointerComponentDesignatorName(const semantics::SomeExpr &expr) {
  if (const auto *derived{
          evaluate::GetDerivedTypeSpec(evaluate::DynamicType::From(expr))}) {
    semantics::PotentialAndPointerComponentIterator potentials{*derived};
    if (auto pointer{std::find_if(potentials.begin(), potentials.end(),
                                  semantics::IsPointer)}) {
      return pointer.BuildResultDesignatorName();
    }
  }
  return std::nullopt;
}

// C1593 (4)
void PureProcedureCheck::Leave(const parser::AssignmentStmt &assignment) {
  // get the rhs
  const auto &rhs{std::get<parser::Expr>(assignment.t)};
  // get the evaluate::Expr
  const auto *expr{semantics::GetExpr(context_->getSemanticsContext(), rhs)};

  const semantics::Scope &scope{
      context_->getSemanticsContext().FindScope(rhs.source)};

  if (const semantics::Symbol * base{GetFirstSymbol(expr)}) {
    if (const char *why{
            WhyBaseObjectIsSuspicious(base->GetUltimate(), scope)}) {
      if (auto pointer{GetPointerComponentDesignatorName(*expr)}) {
        // mark the procedure as impure
        (void)why;
        SetUnpure();
      }
    }
  }
}

// C1592
void PureProcedureCheck::Leave(const parser::Name &n) {
  if (n.symbol && n.symbol->attrs().test(semantics::Attr::VOLATILE)) {
    SetUnpure();
  }
}

// C1598: pure procs cant have image control statements
void PureProcedureCheck::Enter(const parser::ExecutableConstruct &exec) {
  if (semantics::IsImageControlStmt(exec)) {
    SetUnpure();
  }
}

// check Call Stmt
void PureProcedureCheck::Enter(const parser::CallStmt &callStmt) {
  const auto *procedureRef = callStmt.typedCall.get();
  if (procedureRef) {
    const auto *symbol{procedureRef->proc().GetSymbol()};

    // if the called function isnt pure, we cant be pure
    if (!semantics::IsPureProcedure(*symbol))
      SetUnpure();
  }
}

void PureProcedureCheck::Leave(const parser::Program &program) {
  // tell us about all the procedure that could be pure, but arent
  for (const auto &pair : pureProcedures_) {
    // it should be pure, but isnt - and its not intrinsic, and not elemental,
    // and not an interface
    // check if the symbol is the Ultimate symbol
    const auto &symbol = pair.first->GetUltimate();
    if (symbol != *pair.first) {
      continue;
    }

    // make sure its not being mapped to from procBindingDetailsSymbolsMap
    // like if its the "second" of a pair
    bool cont = false;
    for (const auto &procBindingPair : procBindingDetailsSymbolsMap) {
      if (procBindingPair.second == pair.first) {
        cont = true;
        break;
      }
    }
    if (cont) {
      continue;
    }

    if (pair.second && !pair.first->attrs().test(semantics::Attr::PURE) &&
        !pair.first->attrs().test(semantics::Attr::INTRINSIC) &&
        !pair.first->attrs().test(semantics::Attr::ELEMENTAL) &&
        !pair.first->attrs().test(semantics::Attr::ABSTRACT) &&
        !pair.first->attrs().test(semantics::Attr::EXTERNAL) &&
        !pair.first->IsFromModFile()
        // if its been derived somewhere, we dont care
        && procBindingDetailsSymbolsMap.find(pair.first) ==
               procBindingDetailsSymbolsMap.end()) {
      context_->getSemanticsContext().Say(
          pair.first->name(), "Procedure '%s' could be PURE but is not"_en_US,
          pair.first->name());
    }
  }
}

} // namespace Fortran::tidy::misc
