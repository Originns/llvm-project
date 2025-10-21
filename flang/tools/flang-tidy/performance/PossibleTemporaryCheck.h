//===--- PossibleTemporaryCheck.h - flang-tidy ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_PERFORMANCE_POSSIBLETEMPORARYCHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_PERFORMANCE_POSSIBLETEMPORARYCHECK_H

#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::performance {

/// This check warns when a procedure call might create a temporary
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/possible-temporary.html
class PossibleTemporaryCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~PossibleTemporaryCheck() = default;

  void Enter(const parser::CallStmt &) override;
};

} // namespace Fortran::tidy::performance

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_PERFORMANCE_POSSIBLETEMPORARYCHECK_H
