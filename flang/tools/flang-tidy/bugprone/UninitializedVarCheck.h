#ifndef FORTRAN_TIDY_SHORTCIRCUIT
#define FORTRAN_TIDY_SHORTCIRCUIT

#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"

namespace Fortran::tidy::bugprone {

class UninitializedVarCheck : public virtual semantics::BaseChecker {
public:
  explicit UninitializedVarCheck(semantics::SemanticsContext &);
  ~UninitializedVarCheck();
  void Leave(const parser::AssignmentStmt &);
  void Leave(const parser::PointerAssignmentStmt &);
  void Enter(const parser::DoConstruct &);
  void Leave(const parser::AllocateStmt &);
  void Leave(const parser::CommonStmt &);
  void Leave(const parser::CallStmt &);
  void Enter(const parser::Expr &);

private:
  semantics::SemanticsContext &context_;
  semantics::UnorderedSymbolSet definedVars_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_SHORTCIRCUIT
