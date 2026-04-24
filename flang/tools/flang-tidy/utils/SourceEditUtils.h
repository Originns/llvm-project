//===-- SourceEditUtils.h - Helpers for source-based fix-its ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_UTILS_SOURCEEDITUTILS_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_UTILS_SOURCEEDITUTILS_H

#include "utils/FixIt.h"
#include "flang/Parser/char-block.h"
#include "flang/Semantics/semantics.h"
#include "llvm/ADT/StringRef.h"
#include <optional>
#include <string>
#include <vector>

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

/// Returns true if \p source contains no embedded newline characters, i.e.
/// the construct fits on a single physical line.
bool isSingleLine(parser::CharBlock source);

/// Returns a CharBlock spanning the entire physical line (up to and including
/// the trailing '\\n') that contains \p anchor.  Returns nullopt when the
/// source location cannot be resolved or the statement spans multiple lines.
std::optional<parser::CharBlock>
getEntireSourceLine(semantics::SemanticsContext &ctx, parser::CharBlock anchor);

/// Returns the portion of \p line before any Fortran inline comment ('!'),
/// with trailing whitespace stripped.  If there is no comment the whole
/// string is returned unchanged.
llvm::StringRef stripTrailingComment(llvm::StringRef line);

/// Splits \p line at the first Fortran inline comment character ('!').
/// Returns {declPart, commentPart} where \p commentPart is empty when there
/// is no comment, and starts with '!' when a comment is present.  Unlike
/// stripTrailingComment(), neither part is trimmed — the split is exact.
std::pair<llvm::StringRef, llvm::StringRef>
splitLineAtComment(llvm::StringRef line);

/// Convenience wrapper: verify that \p stmtSource fits on a single line,
/// locate the full physical line (including trailing '\\n'), and return a
/// removal fix-it for it.  Returns nullopt when any step fails.
std::optional<FixItHint>
removeStatementLine(semantics::SemanticsContext &ctx,
                    parser::CharBlock stmtSource);

// ---------------------------------------------------------------------------
// Declaration-line parsing helpers
// ---------------------------------------------------------------------------

/// Returns true if \p text contains a ',' that is not enclosed in parentheses.
bool hasTopLevelComma(llvm::StringRef text);

/// Returns true if the declaration line has exactly one entity after '::',
/// i.e. the entity-decl-list contains no top-level commas.
bool hasSingleEntityAfterDoubleColon(llvm::StringRef lineText);

/// Returns true if \p token is a valid Fortran identifier (starts with a
/// letter or '_', followed by letters, digits, or '_').
bool isSimpleIdentifier(llvm::StringRef token);

/// One entry in a Fortran entity-decl-list, e.g. "x(10)" or "y = 0".
struct EntityDecl {
  std::string declarator; ///< Full source text of the declarator.
  std::string baseName;   ///< Just the leading identifier (no array spec etc.).
};

/// Parses a comma-separated Fortran entity-decl-list from \p entityText.
/// Returns nullopt when any token is malformed or the list is empty.
std::optional<std::vector<EntityDecl>>
parseEntityDeclList(llvm::StringRef entityText);

/// Structural decomposition of a Fortran type-declaration source line,
/// as needed by fix-it builders that insert or rewrite attributes / entities.
struct ParsedDeclLine {
  std::string declPart;    ///< Source text before any inline comment.
  std::string commentPart; ///< Inline comment including '!' (may be empty).
  std::string prefix;      ///< Text up to and including whitespace after '::'.
  std::vector<EntityDecl> entities; ///< Parsed entity-decl list.
};

/// Decompose a Fortran type-declaration source line into its structural parts.
/// Returns nullopt when the line has no '::' or the entity list cannot be
/// parsed.
std::optional<ParsedDeclLine> parseDeclarationLine(llvm::StringRef lineText);

} // namespace Fortran::tidy::utils

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_UTILS_SOURCEEDITUTILS_H
