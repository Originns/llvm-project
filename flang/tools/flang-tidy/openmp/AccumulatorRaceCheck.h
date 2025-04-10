#ifndef FORTRAN_TIDY_ACCUMULATORRACECHECK
#define FORTRAN_TIDY_ACCUMULATORRACECHECK
#include "../FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::openmp {

class AccumulatorRaceCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~AccumulatorRaceCheck() = default;

  void Enter(const parser::OpenMPBlockConstruct &) override;
  void Leave(const parser::OpenMPBlockConstruct &) override;
  void Enter(const parser::OmpAtomicUpdate &) override;
  void Leave(const parser::OmpAtomicUpdate &) override;
  void Enter(const parser::OpenMPCriticalConstruct &) override;
  void Leave(const parser::OpenMPCriticalConstruct &) override;
  void Enter(const parser::AssignmentStmt &) override;

private:
  bool inParallelRegion_ = false;
  bool inCriticalSection_ = false;
  bool inAtomicUpdate_ = false;
};

} // namespace Fortran::tidy::openmp
#endif // FORTRAN_TIDY_ACCUMULATORRACECHECK
