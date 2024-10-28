#ifndef FORTRAN_TIDY_UNUSEDINTENTCHECK
#define FORTRAN_TIDY_UNUSEDINTENTCHECK

#include "flang/Evaluate/expression.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

#include "utils/UnwrapSymbol.h"

namespace Fortran::tidy {

struct SymbolRefHash {
  std::size_t operator()(const Fortran::semantics::SymbolRef &symbolRef) const {
    // hash the address of the symbol
    return std::hash<const Fortran::semantics::Symbol *>{}(&symbolRef.get());
  }
};

class UnusedIntentCheck : public virtual semantics::BaseChecker {
public:
  explicit UnusedIntentCheck(semantics::SemanticsContext &);
  ~UnusedIntentCheck();
  void Enter(const parser::EntityDecl &);
  void Enter(const parser::AssignmentStmt &);
  void Enter(const parser::Expr &);
  void Leave(const parser::Program &);

private:
  semantics::SemanticsContext &context_;
  // an unordered map containing symbol to "usage" mappings (to indicate if its
  // used for reading and/or writing) with the compare fuc
  std::unordered_map<semantics::SymbolRef, std::pair<bool, bool>, SymbolRefHash>
      intentVars_;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_UNUSEDINTENTCHECK
