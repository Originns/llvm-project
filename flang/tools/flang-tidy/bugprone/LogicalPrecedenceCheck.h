//===--- LogicalPrecedenceCheck.h - flang-tidy ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_LOGICALPRECEDENCECHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_LOGICALPRECEDENCECHECK_H

#include "FlangTidyCheck.h"

namespace Fortran::tidy::bugprone {

// This check warns when logical AND and OR are mixed without parentheses, which
// can lead to readability issues due to the tighter binding of .AND. over .OR.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/logical-precedence.html
class LogicalPrecedenceCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~LogicalPrecedenceCheck() = default;

  void Enter(const parser::Expr &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_LOGICALPRECEDENCECHECK_H
