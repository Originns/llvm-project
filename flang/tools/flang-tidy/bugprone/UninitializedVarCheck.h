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
  virtual ~UninitializedVarCheck() = default;
  void Leave(const parser::AssignmentStmt &) override;
  void Leave(const parser::PointerAssignmentStmt &) override;
  void Leave(const parser::AllocateStmt &) override;
  void Enter(const parser::DoConstruct &) override;
  void Enter(const parser::OutputImpliedDo &) override;
  void Enter(const parser::InputImpliedDo &) override;
  void Enter(const parser::AcImpliedDo &) override;
  void Enter(const parser::DataImpliedDo &) override;
  void Leave(const parser::CommonStmt &) override;
  void Leave(const parser::WriteStmt &) override;
  void Enter(const parser::CallStmt &) override;
  void Enter(const parser::Expr &) override;

private:
  FlangTidyContext *context_;
  semantics::UnorderedSymbolSet definedVars_;
  semantics::UnorderedSymbolSet allocatedVars_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_SHORTCIRCUIT
