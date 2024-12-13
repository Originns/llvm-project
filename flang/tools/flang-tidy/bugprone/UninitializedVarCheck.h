#ifndef FORTRAN_TIDY_SHORTCIRCUIT
#define FORTRAN_TIDY_SHORTCIRCUIT

#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"

#include "../FlangTidyContext.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy::bugprone {

class UninitializedVarCheck : public virtual FlangTidyCheck {
public:
  explicit UninitializedVarCheck(llvm::StringRef name,
                                 FlangTidyContext *context);
  ~UninitializedVarCheck();
  void Leave(const parser::AssignmentStmt &);
  void Leave(const parser::PointerAssignmentStmt &);
  void Enter(const parser::DoConstruct &);
  void Leave(const parser::AllocateStmt &);
  void Leave(const parser::CommonStmt &);
  void Leave(const parser::CallStmt &);
  void Enter(const parser::Expr &);

private:
  FlangTidyContext *context_;
  semantics::UnorderedSymbolSet definedVars_;
  semantics::UnorderedSymbolSet allocatedVars_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_SHORTCIRCUIT
