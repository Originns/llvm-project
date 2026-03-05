//===-- SourceEditUtils.h - Helpers for source-based fix-its ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_UTILS_SOURCEEDITUTILS_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_UTILS_SOURCEEDITUTILS_H

#include "flang/Parser/char-block.h"
#include "flang/Semantics/semantics.h"
#include "llvm/ADT/StringRef.h"
#include <optional>
#include <string>

namespace Fortran::tidy::utils {

struct SourceLineInfo {
  const char *lineBegin{nullptr};
  const char *lineEnd{nullptr};
  std::string lineText;
};

std::string inferKeywordSpelling(llvm::StringRef sourceText,
                                 llvm::StringRef upperKeyword);

std::string getLeadingWhitespace(llvm::StringRef text);

std::string getLeadingWhitespaceFromSourceLine(semantics::SemanticsContext &ctx,
                                               parser::CharBlock anchor);

std::string getDoubleColonSeparator(llvm::StringRef lineText);

std::optional<const char *> findDeclAttrInsertionPoint(llvm::StringRef lineText,
                                                       const char *lineBegin);

std::optional<SourceLineInfo>
getSourceLineInfo(semantics::SemanticsContext &ctx, parser::CharBlock anchor);

} // namespace Fortran::tidy::utils

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_UTILS_SOURCEEDITUTILS_H
