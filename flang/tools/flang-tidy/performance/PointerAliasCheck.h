//===--- PointerAliasCheck.h - flang-tidy -----------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_PERFORMANCE_POINTERALIASCHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_PERFORMANCE_POINTERALIASCHECK_H

#include "FlangTidyCheck.h"
#include <set>
#include <utility>

namespace Fortran::tidy::performance {

/// This check reports loops in which one array pointer is written while a
/// different array pointer is read.  Fortran gives no guarantee that two
/// pointers designate disjoint storage, so such a loop carries a potential
/// dependence that the compiler has to account for.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/pointer-alias.html
class PointerAliasCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~PointerAliasCheck() = default;
  void Enter(const parser::DoConstruct &) override;

private:
  /// Nested loops are visited once per enclosing DO construct, so each
  /// (assignment, aliasing pointer) pair is remembered to report it only once.
  std::set<std::pair<const char *, const semantics::Symbol *>> reported_;
};

} // namespace Fortran::tidy::performance

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_PERFORMANCE_POINTERALIASCHECK_H
