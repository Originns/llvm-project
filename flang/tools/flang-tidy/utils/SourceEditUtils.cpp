//===-- SourceEditUtils.cpp - Helpers for source-based fix-its -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "utils/SourceEditUtils.h"
#include "utils/FixIt.h"
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

bool isSingleLine(parser::CharBlock source) {
  return !llvm::StringRef{source.begin(), source.size()}.contains('\n');
}

std::optional<parser::CharBlock>
getEntireSourceLine(semantics::SemanticsContext &ctx,
                    parser::CharBlock anchor) {
  if (anchor.begin() == nullptr)
    return std::nullopt;

  const auto &allCooked{ctx.allCookedSources()};
  const auto *cooked{allCooked.Find(anchor)};
  if (!cooked)
    return std::nullopt;

  const parser::CharBlock source{cooked->AsCharBlock()};
  const char *lineBegin{anchor.begin()};
  const char *lineEnd{anchor.end()};

  while (lineBegin > source.begin() && lineBegin[-1] != '\n')
    --lineBegin;
  while (lineEnd < source.end() && *lineEnd != '\n')
    ++lineEnd;
  if (lineEnd < source.end() && *lineEnd == '\n')
    ++lineEnd; // include the newline itself so deletion removes the whole line

  return parser::CharBlock{lineBegin,
                           static_cast<std::size_t>(lineEnd - lineBegin)};
}

llvm::StringRef stripTrailingComment(llvm::StringRef line) {
  const std::size_t pos{line.find('!')};
  if (pos == llvm::StringRef::npos)
    return line;
  return line.substr(0, pos).rtrim();
}

std::pair<llvm::StringRef, llvm::StringRef>
splitLineAtComment(llvm::StringRef line) {
  const std::size_t pos{line.find('!')};
  if (pos == llvm::StringRef::npos)
    return {line, {}};
  return {line.substr(0, pos), line.substr(pos)};
}

std::optional<FixItHint>
removeStatementLine(semantics::SemanticsContext &ctx,
                    parser::CharBlock stmtSource) {
  if (!isSingleLine(stmtSource))
    return std::nullopt;
  auto lineRange{getEntireSourceLine(ctx, stmtSource)};
  if (!lineRange)
    return std::nullopt;
  return FixItHint::CreateRemoval(*lineRange);
}

// ---------------------------------------------------------------------------
// Declaration-line parsing helpers
// ---------------------------------------------------------------------------

bool hasTopLevelComma(llvm::StringRef text) {
  int parenDepth{0};
  for (char ch : text) {
    if (ch == '(')
      ++parenDepth;
    else if (ch == ')') {
      if (parenDepth > 0)
        --parenDepth;
    } else if (ch == ',' && parenDepth == 0)
      return true;
  }
  return false;
}

bool hasSingleEntityAfterDoubleColon(llvm::StringRef lineText) {
  const std::size_t doubleColonPos = lineText.find("::");
  if (doubleColonPos == llvm::StringRef::npos)
    return false;
  llvm::StringRef suffix = lineText.substr(doubleColonPos + 2);
  // Strip any inline comment before checking for commas.
  suffix = splitLineAtComment(suffix).first;
  return !hasTopLevelComma(suffix);
}

bool isSimpleIdentifier(llvm::StringRef token) {
  if (token.empty())
    return false;
  const unsigned char first = static_cast<unsigned char>(token.front());
  if (!(std::isalpha(first) || token.front() == '_'))
    return false;
  for (char ch : token.drop_front()) {
    const unsigned char uch = static_cast<unsigned char>(ch);
    if (!(std::isalnum(uch) || ch == '_'))
      return false;
  }
  return true;
}

std::optional<std::vector<EntityDecl>>
parseEntityDeclList(llvm::StringRef entityText) {
  std::vector<EntityDecl> entities;
  std::size_t pos{0};
  while (pos < entityText.size()) {
    // Find the next top-level comma.
    std::size_t comma{llvm::StringRef::npos};
    int parenDepth{0};
    for (std::size_t i{pos}; i < entityText.size(); ++i) {
      const char ch = entityText[i];
      if (ch == '(')
        ++parenDepth;
      else if (ch == ')') {
        if (parenDepth > 0)
          --parenDepth;
      } else if (ch == ',' && parenDepth == 0) {
        comma = i;
        break;
      }
    }

    const llvm::StringRef token =
        (comma == llvm::StringRef::npos ? entityText.substr(pos)
                                        : entityText.substr(pos, comma - pos))
            .trim();
    if (token.empty())
      return std::nullopt;

    // Extract the base identifier (up to the first non-identifier char).
    std::size_t nameEnd{0};
    {
      const unsigned char first = static_cast<unsigned char>(token.front());
      if (!(std::isalpha(first) || token.front() == '_'))
        return std::nullopt;
      nameEnd = 1;
      while (nameEnd < token.size()) {
        const unsigned char uch =
            static_cast<unsigned char>(token[nameEnd]);
        if (!(std::isalnum(uch) || token[nameEnd] == '_'))
          break;
        ++nameEnd;
      }
    }
    const llvm::StringRef baseName = token.take_front(nameEnd);
    if (!isSimpleIdentifier(baseName))
      return std::nullopt;

    entities.push_back(EntityDecl{token.str(), baseName.str()});
    if (comma == llvm::StringRef::npos)
      break;
    pos = comma + 1;
  }

  if (entities.empty())
    return std::nullopt;
  return entities;
}

std::optional<ParsedDeclLine> parseDeclarationLine(llvm::StringRef lineText) {
  auto [declRef, commentRef] = splitLineAtComment(lineText);
  const std::string declPart = declRef.str();
  const std::string commentPart = commentRef.str();

  const std::size_t doubleColonPos = declPart.find("::");
  if (doubleColonPos == std::string::npos)
    return std::nullopt;

  // Advance past '::' and any following whitespace to find the entity list.
  std::size_t rhsStart = doubleColonPos + 2;
  while (rhsStart < declPart.size() &&
         std::isspace(static_cast<unsigned char>(declPart[rhsStart])))
    ++rhsStart;

  const std::string prefix = declPart.substr(0, rhsStart);
  const llvm::StringRef rhs =
      llvm::StringRef{declPart}.substr(rhsStart).trim();

  auto entities = parseEntityDeclList(rhs);
  if (!entities)
    return std::nullopt;

  return ParsedDeclLine{declPart, commentPart, prefix, std::move(*entities)};
}

} // namespace Fortran::tidy::utils
