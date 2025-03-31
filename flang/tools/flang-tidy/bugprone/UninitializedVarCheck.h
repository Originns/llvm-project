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
  // Variable definitions via assignments
  void Leave(const parser::AssignmentStmt &) override;
  void Leave(const parser::PointerAssignmentStmt &) override;

  // I/O statements
  void Leave(const parser::ReadStmt &) override;
  void Leave(const parser::WriteStmt &) override;
  void Leave(const parser::OpenStmt &) override;
  // void Leave(const parser::CloseStmt &) override;
  // void Leave(const parser::InquireStmt &) override;
  // void Leave(const parser::BackspaceStmt &) override;
  // void Leave(const parser::EndfileStmt &) override;
  // void Leave(const parser::RewindStmt &) override;
  // void Leave(const parser::FlushStmt &) override;
  // void Leave(const parser::WaitStmt &) override;

  // Memory allocation statements
  void Leave(const parser::AllocateStmt &) override;
  // void Leave(const parser::DeallocateStmt &) override;

  // Data declaration
  void Leave(const parser::CommonStmt &) override;

  // Loop and control constructs with implicit variable definitions
  void Enter(const parser::OutputImpliedDo &) override;
  void Enter(const parser::InputImpliedDo &) override;
  void Enter(const parser::AcImpliedDo &) override;
  void Enter(const parser::DataImpliedDo &) override;
  void Enter(const parser::DoConstruct &) override;
  // void Enter(const parser::ForallStmt &) override;
  // void Enter(const parser::ForallConstruct &) override;

  // Procedure calls
  void Enter(const parser::CallStmt &) override;

  // Expression checking for variable usage
  void Enter(const parser::Expr &) override;

private:
  FlangTidyContext *context_;
  semantics::UnorderedSymbolSet definedVars_;
  semantics::UnorderedSymbolSet allocatedVars_;
};

} // namespace Fortran::tidy::bugprone

#endif // FORTRAN_TIDY_SHORTCIRCUIT
