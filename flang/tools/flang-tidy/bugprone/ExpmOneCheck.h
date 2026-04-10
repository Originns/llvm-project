//===--- ExpmOneCheck.h - flang-tidy ----------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_EXPMONECHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_EXPMONECHECK_H

#include "FlangTidyCheck.h"

namespace Fortran::tidy::bugprone {

/// Detects expressions of the form \c exp(x)-1 or \c -1+exp(x) where
/// \c expm1(x) would give a numerically more accurate result when \c x is
/// near zero (catastrophic cancellation).
///
/// The check always warns when \c x is a non-constant expression.  When \c x
/// is a compile-time constant the warning is only emitted if \c |x| <
/// \c WarnThreshold (default 1e-5), since for large arguments the relative
/// error from the naive formula is negligible.
///
/// Option: \c WarnThreshold — absolute threshold below which a constant
/// argument is considered "near zero" (default \c 1e-5).
class ExpmOneCheck : public virtual FlangTidyCheck {
public:
  explicit ExpmOneCheck(llvm::StringRef Name, FlangTidyContext *Context);
  virtual ~ExpmOneCheck() = default;

  void storeOptions(FlangTidyOptions::OptionMap &Opts) override;

  void Enter(const parser::Expr::Subtract &) override;
  void Enter(const parser::Expr::Add &) override;

private:
  /// Absolute threshold below which a constant \c x is considered near zero.
  const double WarnThreshold;

  static constexpr double DefaultWarnThreshold = 1e-5;
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_EXPMONECHECK_H
