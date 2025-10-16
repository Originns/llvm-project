//===--- PointerTemporaryCheck.h - flang-tidy -------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_POINTERTEMPORARYCHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_POINTERTEMPORARYCHECK_H

#include "FlangTidyCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::bugprone {

/// This check checks for pointer variables that might point to
/// a temporary object.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/pointer-temporary.html
class PointerTemporaryCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~PointerTemporaryCheck() = default;

  void Leave(const parser::PointerAssignmentStmt &) override;
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_POINTERTEMPORARYCHECK_H
