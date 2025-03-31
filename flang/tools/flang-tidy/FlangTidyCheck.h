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
  virtual void Leave(const parser::WriteStmt &) {}
  virtual void Enter(const parser::CallStmt &) {}
  virtual void Enter(const parser::Expr &) {}
  virtual void Enter(const parser::AssignStmt &) {}
  virtual void Enter(const parser::AssignedGotoStmt &) {}
  virtual void Enter(const parser::BackspaceStmt &) {}
  virtual void Enter(const parser::CommonStmt &) {}
  virtual void Enter(const parser::DataStmt &) {}
  virtual void Enter(const parser::OutputImpliedDo &) {};
  virtual void Enter(const parser::InputImpliedDo &) {};
  virtual void Enter(const parser::AcImpliedDo &) {};
  virtual void Enter(const parser::DataImpliedDo &) {};
  virtual void Leave(const parser::BackspaceStmt &) {};
  virtual void Leave(const parser::CloseStmt &) {};
  virtual void Leave(const parser::EndfileStmt &) {};
  virtual void Leave(const parser::FlushStmt &) {};
  virtual void Leave(const parser::InquireStmt &) {};
  virtual void Leave(const parser::OpenStmt &) {};
  virtual void Leave(const parser::PrintStmt &) {};
  virtual void Leave(const parser::ReadStmt &) {};
  virtual void Leave(const parser::RewindStmt &) {};
  virtual void Leave(const parser::WaitStmt &) {};
  virtual void Leave(const parser::FileUnitNumber &) {};
  virtual void Leave(const parser::Name &) {};
  virtual void Enter(const parser::ExecutableConstruct &) {};
  virtual void Leave(const parser::Program &) {};
  virtual void Leave(const parser::UseStmt &) {};
  virtual void Enter(const parser::CaseConstruct &) {};
  virtual void Enter(const parser::PauseStmt &) {};
  virtual void Enter(const parser::ForallStmt &) {};
  virtual void Enter(const parser::ForallConstruct &) {};
  virtual void Leave(const parser::DeallocateStmt &) {};

  virtual void Leave(const parser::EventPostStmt &) {};
  virtual void Leave(const parser::EventWaitStmt &) {};
  virtual void Leave(const parser::LockStmt &) {};
  virtual void Leave(const parser::UnlockStmt &) {};
  virtual void Leave(const parser::SyncAllStmt &) {};
  virtual void Leave(const parser::SyncImagesStmt &) {};
  virtual void Leave(const parser::SyncMemoryStmt &) {};
  virtual void Leave(const parser::SyncTeamStmt &) {};
  virtual void Leave(const parser::FormTeamStmt &) {};

protected:
  bool fixAvailable_{false};

private:
  llvm::StringRef name_;
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYCHECK_H
