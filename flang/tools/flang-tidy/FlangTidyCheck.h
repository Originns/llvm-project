#ifndef FORTRAN_TIDY_FLANGTIDYCHECK_H
#define FORTRAN_TIDY_FLANGTIDYCHECK_H

#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/semantics.h"
#include "llvm/ADT/StringRef.h"

namespace Fortran::tidy {

// flang tidy check is a base class for all flang tidy checks
// it has Enter and Leave methods that are called by the flang
// semantic checker when entering and leaving a node in the AST

class FlangTidyCheck : public semantics::BaseChecker {
public:
  FlangTidyCheck(llvm::StringRef name) : name_{name} {}
  virtual ~FlangTidyCheck() = default;
  llvm::StringRef name() const { return name_; }

  using semantics::BaseChecker::Enter;
  using semantics::BaseChecker::Leave;
  virtual void Enter(const parser::ComputedGotoStmt &) {}
  virtual void Enter(const parser::ArithmeticIfStmt &) {}
  virtual void Enter(const parser::AssignmentStmt &) {}
  virtual void Leave(const parser::AssignmentStmt &) {}
  virtual void Leave(const parser::PointerAssignmentStmt &) {}
  virtual void Enter(const parser::DoConstruct &) {}
  virtual void Leave(const parser::AllocateStmt &) {}
  virtual void Leave(const parser::CommonStmt &) {}
  virtual void Leave(const parser::CallStmt &) {}
  virtual void Enter(const parser::Expr &) {}
  virtual void Enter(const parser::AssignStmt &) {}
  virtual void Enter(const parser::AssignedGotoStmt &) {}
  virtual void Enter(const parser::BackspaceStmt &) {}
  virtual void Enter(const parser::CommonStmt &) {}
  virtual void Enter(const parser::DataStmt &) {}
protected:
  bool fixAvailable_{false};
private:
  llvm::StringRef name_;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYCHECK_H
