//===-- SourceEditUtils.cpp - Helpers for source-based fix-its -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "utils/SourceEditUtils.h"
#include "flang/Parser/provenance.h"
#include <cctype>

namespace Fortran::tidy::utils {

std::string inferKeywordSpelling(llvm::StringRef sourceText,
                                 llvm::StringRef upperKeyword) {
  std::size_t tokenStart{0};
  while (tokenStart < sourceText.size() &&
         !std::isalpha(static_cast<unsigned char>(sourceText[tokenStart]))) {
    ++tokenStart;
  }
  if (tokenStart >= sourceText.size()) {
    return upperKeyword.str();
  }

  std::size_t tokenEnd{tokenStart};
  while (tokenEnd < sourceText.size() &&
         std::isalpha(static_cast<unsigned char>(sourceText[tokenEnd]))) {
    ++tokenEnd;
  }

  bool hasLower{false};
  bool hasUpper{false};
  for (std::size_t i{tokenStart}; i < tokenEnd; ++i) {
    const unsigned char ch{static_cast<unsigned char>(sourceText[i])};
    hasLower = hasLower || std::islower(ch);
    hasUpper = hasUpper || std::isupper(ch);
  }

  if (hasLower && !hasUpper) {
    return upperKeyword.lower();
  }
  return upperKeyword.upper();
}

std::string getLeadingWhitespace(llvm::StringRef text) {
  std::size_t i{0};
  while (i < text.size() && (text[i] == ' ' || text[i] == '\t')) {
    ++i;
  }
  return text.substr(0, i).str();
}

std::string getLeadingWhitespaceFromSourceLine(semantics::SemanticsContext &ctx,
                                               parser::CharBlock anchor) {
  if (anchor.begin() == nullptr) {
    return {};
  }

  const auto &allCooked = ctx.allCookedSources();
  auto provenance = allCooked.GetProvenanceRange(parser::CharBlock{
      anchor.begin(),
      1,
  });
  if (!provenance) {
    return {};
  }

  const auto &allSources = allCooked.allSources();
  auto sourcePos = allSources.GetSourcePosition(provenance->start());
  if (!sourcePos || sourcePos->trueLineNumber <= 0) {
    return {};
  }
  const auto *sourceFile = allSources.GetSourceFile(provenance->start());
  if (!sourceFile || static_cast<std::size_t>(sourcePos->trueLineNumber) >
                         sourceFile->lines()) {
    return {};
  }

  const std::size_t lineStart =
      sourceFile->GetLineStartOffset(sourcePos->trueLineNumber);
  const auto content = sourceFile->content();
  std::size_t i{lineStart};
  while (i < content.size() && (content[i] == ' ' || content[i] == '\t')) {
    ++i;
  }
  return std::string(content.data() + lineStart, i - lineStart);
}

std::string getDoubleColonSeparator(llvm::StringRef lineText) {
  const std::size_t doubleColonPos = lineText.find("::");
  if (doubleColonPos == llvm::StringRef::npos) {
    return " :: ";
  }

  std::size_t left{doubleColonPos};
  while (left > 0 &&
         std::isspace(static_cast<unsigned char>(lineText[left - 1]))) {
    --left;
  }
  std::size_t right{doubleColonPos + 2};
  while (right < lineText.size() &&
         std::isspace(static_cast<unsigned char>(lineText[right]))) {
    ++right;
  }

  std::string sep =
      lineText.substr(left, doubleColonPos - left).str() + "::" +
      lineText.substr(doubleColonPos + 2, right - (doubleColonPos + 2)).str();
  if (sep.back() != ' ' && sep.back() != '\t') {
    sep += " ";
  }
  return sep;
}

std::optional<const char *> findDeclAttrInsertionPoint(llvm::StringRef lineText,
                                                       const char *lineBegin) {
  if (lineBegin == nullptr) {
    return std::nullopt;
  }

  if (auto doubleColonPos = lineText.find("::");
      doubleColonPos != llvm::StringRef::npos) {
    std::size_t insertPosInLine = doubleColonPos;
    while (insertPosInLine > 0 && std::isspace(static_cast<unsigned char>(
                                      lineText[insertPosInLine - 1]))) {
      --insertPosInLine;
    }
    return lineBegin + insertPosInLine;
  }

  // Legacy form without '::': insert after declaration type-spec token.
  const auto firstSpaceOrTab = lineText.find_first_of(" \t");
  if (firstSpaceOrTab != llvm::StringRef::npos) {
    return lineBegin + firstSpaceOrTab;
  }

  return std::nullopt;
}

std::optional<SourceLineInfo>
getSourceLineInfo(semantics::SemanticsContext &ctx, parser::CharBlock anchor) {
  if (anchor.begin() == nullptr) {
    return std::nullopt;
  }

  const auto &allCooked = ctx.allCookedSources();
  const auto *cooked = allCooked.Find(anchor);
  if (!cooked) {
    return std::nullopt;
  }

  const parser::CharBlock source = cooked->AsCharBlock();
  const char *lineBegin = anchor.begin();
  const char *lineEnd = anchor.begin();

  while (lineBegin > source.begin() && lineBegin[-1] != '\n') {
    --lineBegin;
  }
  while (lineEnd < source.end() && *lineEnd != '\n') {
    ++lineEnd;
  }

  return SourceLineInfo{lineBegin, lineEnd,
                        std::string(lineBegin, lineEnd - lineBegin)};
}

} // namespace Fortran::tidy::utils
