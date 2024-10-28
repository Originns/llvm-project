#include "UnusedIntentCheck.h"

namespace Fortran::tidy {

/*
 * For each function, check if we have an intent(inout) variable
 * track the variable and check if its either never assigned or read from
 * if so, warn the user about it
 * in case of another function being called with that variable,
 * recursively track the variable in that function
 */

// TODO: Check for functions we are calling that might have intent(out), check
// loops (assignment), arrays, pointers, etc

UnusedIntentCheck::~UnusedIntentCheck() {}

UnusedIntentCheck::UnusedIntentCheck(semantics::SemanticsContext &context)
    : context_{context}, intentVars_{} {}

using namespace parser::literals;

// fill up the intent vars set with all variables declared as intent(inout)
// std::tuple<DeclarationTypeSpec, std::list<AttrSpec>, std::list<EntityDecl>>
// t;
void UnusedIntentCheck::Enter(const parser::EntityDecl &decl) {
  const auto &name = std::get<parser::Name>(decl.t);
  const auto &symbol = utils::UnwrapSymbol(name);
  if (symbol.has_value()) {
    if (semantics::IsIntentInOut(*symbol)) {
      intentVars_.insert({symbol.value(), {false, false}});
    }
  }
}

void UnusedIntentCheck::Enter(const parser::AssignmentStmt &assignment) {
  const auto &var{std::get<parser::Variable>(assignment.t)};
  const auto &expr{std::get<parser::Expr>(assignment.t)};
  const auto *lhs{semantics::GetExpr(context_, var)};
  const auto *rhs{semantics::GetExpr(context_, expr)};
  if (lhs && rhs) {
    for (const auto &symbol : evaluate::CollectSymbols(*lhs)) {
      if (semantics::IsIntentInOut(symbol.get())) {
        // is being written to
        intentVars_[symbol.get()].second = true;
        // llvm::outs() << "Symbol: " << symbol.get().name().ToString() << " is
        // being written to\n";
      }
    }
    for (const auto &symbol : evaluate::CollectSymbols(*rhs)) {
      if (semantics::IsIntentInOut(symbol.get())) {
        // is being read
        intentVars_[symbol.get()].first = true;
      }
    }
  }
}

// check for functions (call and

void UnusedIntentCheck::Enter(const parser::Expr &x) {
  const auto *expr = semantics::GetExpr(x);
  if (expr) {
    for (const auto &symbol : evaluate::CollectSymbols(*expr)) {
      if (semantics::IsIntentInOut(symbol.get())) {
        // is being read
        intentVars_[symbol.get()].first = true;
      }
    }
  }
}

// TODO: find a better way to do this...
void UnusedIntentCheck::Leave(const parser::Program &program) {
  for (const auto &[symbol, usage] : intentVars_) {
    if (!usage.first) {
      context_.messages().Say(
          symbol.get().name(),
          "symbol %s is intent(inout) but never read from\n"_warn_en_US,
          symbol.get().name());
    }
    if (!usage.second) {
      context_.messages().Say(
          symbol.get().name(),
          "symbol %s is intent(inout) but never written to\n"_warn_en_US,
          symbol.get().name());
    }
  }
}

} // namespace Fortran::tidy
