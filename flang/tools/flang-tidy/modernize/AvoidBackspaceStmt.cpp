//===--- AvoidBackspaceStmt.cpp - flang-tidy ------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "AvoidBackspaceStmt.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::modernize {

using namespace parser::literals;
void AvoidBackspaceStmtCheck::Enter(const parser::BackspaceStmt &) {
  if (context()->getSemanticsContext().location().has_value()) {
    Say(context()->getSemanticsContext().location().value(),
        "Assign statements are not recommended"_warn_en_US);
  }
}

} // namespace Fortran::tidy::modernize
