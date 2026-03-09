//===--- UnusedIntentCheck.cpp - flang-tidy -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "UnusedIntentCheck.h"
#include "flang/Semantics/attr.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "utils/FixIt.h"
#include "utils/SourceEditUtils.h"
#include <algorithm>
#include <cctype>
#include <cstddef>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

static std::unordered_map<const semantics::Symbol *, const semantics::Symbol *>
    procBindingDetailsSymbolsMap;
static std::unordered_set<const char *> mixedIntentDeclsWithFix;
static std::unordered_set<const char *> mixedMissingIntentDeclsWithFix;
struct ParsedEntityDecl {
  std::string declarator;
  std::string baseName;
};

static std::string toLowerCopy(llvm::StringRef text) {
  std::string lower = text.str();
  for (char &ch : lower) {
    ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
  }
  return lower;
}

static std::string getIntentSpecSpelling(const std::string &lineText,
                                         bool IsInOut) {
  const std::string keyword = utils::inferKeywordSpelling(lineText, "INTENT");
  const std::string arg = keyword == "intent" ? (IsInOut ? "inout" : "in")
                                              : (IsInOut ? "INOUT" : "IN");
  return keyword + "(" + arg + ")";
}

static bool isSimpleIdentifier(llvm::StringRef token) {
  if (token.empty()) {
    return false;
  }
  const unsigned char first = static_cast<unsigned char>(token.front());
  if (!(std::isalpha(first) || token.front() == '_')) {
    return false;
  }
  for (char ch : token.drop_front()) {
    const unsigned char uch = static_cast<unsigned char>(ch);
    if (!(std::isalnum(uch) || ch == '_')) {
      return false;
    }
  }
  return true;
}

static bool hasTopLevelComma(llvm::StringRef text) {
  int parenDepth{0};
  for (char ch : text) {
    if (ch == '(') {
      ++parenDepth;
    } else if (ch == ')') {
      if (parenDepth > 0) {
        --parenDepth;
      }
    } else if (ch == ',' && parenDepth == 0) {
      return true;
    }
  }
  return false;
}

static std::optional<std::vector<ParsedEntityDecl>>
parseEntityDeclList(llvm::StringRef entityText) {
  std::vector<ParsedEntityDecl> entities;
  std::size_t pos{0};
  while (pos < entityText.size()) {
    std::size_t comma = llvm::StringRef::npos;
    int parenDepth{0};
    for (std::size_t i = pos; i < entityText.size(); ++i) {
      const char ch = entityText[i];
      if (ch == '(') {
        ++parenDepth;
      } else if (ch == ')') {
        if (parenDepth > 0) {
          --parenDepth;
        }
      } else if (ch == ',' && parenDepth == 0) {
        comma = i;
        break;
      }
    }

    const llvm::StringRef rawToken = comma == llvm::StringRef::npos
                                         ? entityText.substr(pos)
                                         : entityText.substr(pos, comma - pos);
    const llvm::StringRef token = rawToken.trim();
    if (token.empty()) {
      return std::nullopt;
    }

    std::size_t nameEnd{0};
    if (!token.empty()) {
      const unsigned char first = static_cast<unsigned char>(token.front());
      if (!(std::isalpha(first) || token.front() == '_')) {
        return std::nullopt;
      }
      nameEnd = 1;
      while (nameEnd < token.size()) {
        const char ch = token[nameEnd];
        const unsigned char uch = static_cast<unsigned char>(ch);
        if (!(std::isalnum(uch) || ch == '_')) {
          break;
        }
        ++nameEnd;
      }
    }
    const llvm::StringRef baseName = token.take_front(nameEnd);
    if (!isSimpleIdentifier(baseName)) {
      return std::nullopt;
    }
    entities.push_back(ParsedEntityDecl{token.str(), baseName.str()});

    if (comma == llvm::StringRef::npos) {
      break;
    }
    pos = comma + 1;
  }
  if (entities.empty()) {
    return std::nullopt;
  }
  return entities;
}

static std::optional<parser::CharBlock>
findIntentInoutRange(const utils::SourceLineInfo &line);

static std::optional<std::string>
buildMixedIntentReplacement(const utils::SourceLineInfo &line,
                            llvm::StringRef targetName,
                            llvm::StringRef explicitIndentation) {
  const std::string lineText = line.lineText;
  const std::size_t commentPos = lineText.find('!');
  const std::string declPart = commentPos == std::string::npos
                                   ? lineText
                                   : lineText.substr(0, commentPos);
  const std::string commentPart =
      commentPos == std::string::npos ? "" : lineText.substr(commentPos);

  const std::size_t doubleColonPos = declPart.find("::");
  if (doubleColonPos == std::string::npos) {
    return std::nullopt;
  }

  std::size_t rhsStart = doubleColonPos + 2;
  while (rhsStart < declPart.size() &&
         std::isspace(static_cast<unsigned char>(declPart[rhsStart]))) {
    ++rhsStart;
  }
  const std::string prefix = declPart.substr(0, rhsStart);
  auto inoutRange = findIntentInoutRange(line);
  if (!inoutRange) {
    return std::nullopt;
  }
  const std::ptrdiff_t inoutBeginOffset = inoutRange->begin() - line.lineBegin;
  const std::ptrdiff_t inoutEndOffset = inoutRange->end() - line.lineBegin;
  if (inoutBeginOffset < 0 || inoutEndOffset < inoutBeginOffset ||
      static_cast<std::size_t>(inoutEndOffset) > prefix.size()) {
    return std::nullopt;
  }
  std::string prefixWithIntentIn = prefix;
  prefixWithIntentIn.replace(
      static_cast<std::size_t>(inoutBeginOffset),
      static_cast<std::size_t>(inoutEndOffset - inoutBeginOffset),
      getIntentSpecSpelling(line.lineText, false));

  const llvm::StringRef rhs = llvm::StringRef{declPart}.substr(rhsStart).trim();
  auto parsedEntities = parseEntityDeclList(rhs);
  if (!parsedEntities || parsedEntities->size() < 2) {
    return std::nullopt;
  }

  const std::string targetLower = toLowerCopy(targetName);
  std::vector<std::string> keepDecls;
  std::string targetDecl;
  bool foundTarget{false};
  for (const ParsedEntityDecl &entity : *parsedEntities) {
    if (toLowerCopy(entity.baseName) == targetLower) {
      foundTarget = true;
      targetDecl = entity.declarator;
    } else {
      keepDecls.push_back(entity.declarator);
    }
  }
  if (!foundTarget || keepDecls.empty() || targetDecl.empty()) {
    return std::nullopt;
  }

  std::string keepList;
  for (std::size_t i{0}; i < keepDecls.size(); ++i) {
    if (i > 0) {
      keepList += ", ";
    }
    keepList += keepDecls[i];
  }

  std::string firstLine = prefix + keepList;
  if (!commentPart.empty()) {
    if (!firstLine.empty() &&
        !std::isspace(static_cast<unsigned char>(firstLine.back()))) {
      firstLine += " ";
    }
    firstLine += commentPart;
  }

  std::string indent = explicitIndentation.str();
  if (indent.empty()) {
    indent = utils::getLeadingWhitespace(firstLine);
  }
  const std::string secondLine =
      indent + llvm::StringRef{prefixWithIntentIn}.ltrim(" \t").str() +
      targetDecl;
  return firstLine + "\n" + secondLine;
}

[[maybe_unused]] static std::optional<std::string>
buildMissingIntentInsertionForMixedDecl(const utils::SourceLineInfo &line,
                                        llvm::StringRef targetName,
                                        llvm::StringRef intentSpec,
                                        llvm::StringRef explicitIndentation) {
  const std::string lineText = line.lineText;
  const std::size_t commentPos = lineText.find('!');
  const std::string declPart = commentPos == std::string::npos
                                   ? lineText
                                   : lineText.substr(0, commentPos);

  const std::size_t doubleColonPos = declPart.find("::");
  if (doubleColonPos == std::string::npos) {
    return std::nullopt;
  }

  std::size_t rhsStart = doubleColonPos + 2;
  while (rhsStart < declPart.size() &&
         std::isspace(static_cast<unsigned char>(declPart[rhsStart]))) {
    ++rhsStart;
  }
  const llvm::StringRef rhs = llvm::StringRef{declPart}.substr(rhsStart).trim();
  auto parsedEntities = parseEntityDeclList(rhs);
  if (!parsedEntities || parsedEntities->size() < 2) {
    return std::nullopt;
  }

  const std::string targetLower = toLowerCopy(targetName);
  std::string targetDecl;
  bool foundTarget{false};
  for (const ParsedEntityDecl &entity : *parsedEntities) {
    if (toLowerCopy(entity.baseName) == targetLower) {
      foundTarget = true;
      targetDecl = entity.declarator;
      break;
    }
  }
  if (!foundTarget || targetDecl.empty()) {
    return std::nullopt;
  }

  auto insertPos =
      utils::findDeclAttrInsertionPoint(line.lineText, line.lineBegin);
  if (!insertPos) {
    return std::nullopt;
  }
  const std::ptrdiff_t insertOffset = *insertPos - line.lineBegin;
  std::string prefixWithDeclAndAttrs = declPart.substr(0, rhsStart);
  if (insertOffset < 0 ||
      static_cast<std::size_t>(insertOffset) > prefixWithDeclAndAttrs.size()) {
    return std::nullopt;
  }
  prefixWithDeclAndAttrs.insert(static_cast<std::size_t>(insertOffset),
                                ", " + intentSpec.str());

  std::string indent = explicitIndentation.str();
  if (indent.empty()) {
    indent = utils::getLeadingWhitespace(prefixWithDeclAndAttrs);
  }
  return "\n" + indent +
         llvm::StringRef{prefixWithDeclAndAttrs}.ltrim(" \t").str() +
         targetDecl;
}

static const semantics::Symbol *
findSymbolInScopeByNameCaseInsensitive(const semantics::Scope &scope,
                                       llvm::StringRef name) {
  const std::string nameLower = toLowerCopy(name);
  for (const auto &pair : scope) {
    const semantics::Symbol &candidate = *pair.second;
    if (toLowerCopy(candidate.name().ToString()) == nameLower) {
      return &candidate;
    }
  }
  return nullptr;
}

[[maybe_unused]] static std::optional<std::string>
buildMissingIntentReplacementForMixedDecl(
    const utils::SourceLineInfo &line, const semantics::Scope &scope,
    llvm::StringRef explicitIndentation,
    llvm::function_ref<bool(const semantics::Symbol &)> wasDefined) {
  const std::string lineText = line.lineText;
  const std::size_t commentPos = lineText.find('!');
  const std::string declPart = commentPos == std::string::npos
                                   ? lineText
                                   : lineText.substr(0, commentPos);
  const std::string commentPart =
      commentPos == std::string::npos ? "" : lineText.substr(commentPos);

  const std::size_t doubleColonPos = declPart.find("::");
  if (doubleColonPos == std::string::npos) {
    return std::nullopt;
  }

  std::size_t rhsStart = doubleColonPos + 2;
  while (rhsStart < declPart.size() &&
         std::isspace(static_cast<unsigned char>(declPart[rhsStart]))) {
    ++rhsStart;
  }
  const std::string prefix = declPart.substr(0, rhsStart);
  const llvm::StringRef rhs = llvm::StringRef{declPart}.substr(rhsStart).trim();
  auto parsedEntities = parseEntityDeclList(rhs);
  if (!parsedEntities || parsedEntities->size() < 2) {
    return std::nullopt;
  }

  auto insertPos =
      utils::findDeclAttrInsertionPoint(line.lineText, line.lineBegin);
  if (!insertPos) {
    return std::nullopt;
  }
  const std::ptrdiff_t insertOffset = *insertPos - line.lineBegin;
  if (insertOffset < 0 ||
      static_cast<std::size_t>(insertOffset) > prefix.size()) {
    return std::nullopt;
  }

  std::string indent = explicitIndentation.str();
  if (indent.empty()) {
    indent = utils::getLeadingWhitespace(line.lineText);
  }
  const bool lineTextHasExplicitIndent =
      indent.empty() || llvm::StringRef{line.lineText}.starts_with(indent);

  std::vector<std::string> keepDecls;
  std::vector<std::string> splitLineBodies;
  bool changed{false};
  for (const ParsedEntityDecl &entity : *parsedEntities) {
    const semantics::Symbol *symbol =
        findSymbolInScopeByNameCaseInsensitive(scope, entity.baseName);
    if (!symbol) {
      keepDecls.push_back(entity.declarator);
      continue;
    }

    const auto *details = symbol->detailsIf<semantics::ObjectEntityDetails>();
    if (!details || !details->isDummy() ||
        symbol->attrs().HasAny(
            {semantics::Attr::INTENT_IN, semantics::Attr::INTENT_INOUT,
             semantics::Attr::INTENT_OUT, semantics::Attr::VALUE}) ||
        symbol->attrs().test(semantics::Attr::TARGET)) {
      keepDecls.push_back(entity.declarator);
      continue;
    }

    changed = true;
    const std::string intentSpec =
        getIntentSpecSpelling(line.lineText, wasDefined(*symbol));
    std::string prefixWithDeclAndAttrs = prefix;
    prefixWithDeclAndAttrs.insert(static_cast<std::size_t>(insertOffset),
                                  ", " + intentSpec);
    splitLineBodies.push_back(
        llvm::StringRef{prefixWithDeclAndAttrs}.ltrim(" \t").str() +
        entity.declarator);
  }
  if (!changed || splitLineBodies.empty()) {
    return std::nullopt;
  }

  std::string replacement;
  if (!keepDecls.empty()) {
    std::string firstLine = prefix;
    for (std::size_t i{0}; i < keepDecls.size(); ++i) {
      if (i > 0) {
        firstLine += ", ";
      }
      firstLine += keepDecls[i];
    }
    if (!commentPart.empty()) {
      if (!firstLine.empty() &&
          !std::isspace(static_cast<unsigned char>(firstLine.back()))) {
        firstLine += " ";
      }
      firstLine += commentPart;
    }
    replacement = firstLine;
  }

  for (std::size_t i{0}; i < splitLineBodies.size(); ++i) {
    if (!replacement.empty() || i > 0) {
      replacement += "\n";
    }
    const bool skipIndentOnFirstSplitLine =
        i == 0 && keepDecls.empty() && !lineTextHasExplicitIndent;
    if (!skipIndentOnFirstSplitLine) {
      replacement += indent;
    }
    replacement += splitLineBodies[i];
  }
  if (keepDecls.empty() && !commentPart.empty()) {
    replacement += " " + commentPart;
  }

  return replacement;
}

static bool hasSingleEntityAfterDoubleColon(const std::string &lineText) {
  const std::size_t doubleColonPos = lineText.find("::");
  if (doubleColonPos == std::string::npos) {
    return false;
  }
  std::string suffix = lineText.substr(doubleColonPos + 2);
  if (const std::size_t commentPos = suffix.find('!');
      commentPos != std::string::npos) {
    suffix.erase(commentPos);
  }
  return !hasTopLevelComma(suffix);
}

static std::optional<parser::CharBlock>
findIntentInoutRange(const utils::SourceLineInfo &line) {
  const std::string lowerLine = toLowerCopy(line.lineText);
  const std::size_t searchLimit = line.lineText.find("::");
  const std::size_t limit =
      searchLimit == std::string::npos ? line.lineText.size() : searchLimit;
  std::size_t i{0};

  while (i < limit) {
    i = lowerLine.find("intent", i);
    if (i == std::string::npos || i >= limit) {
      break;
    }
    std::size_t j = i + 6;
    while (j < limit &&
           std::isspace(static_cast<unsigned char>(line.lineText[j]))) {
      ++j;
    }
    if (j >= limit || line.lineText[j] != '(') {
      i += 6;
      continue;
    }
    std::size_t k = j + 1;
    while (k < limit && line.lineText[k] != ')') {
      ++k;
    }
    if (k >= limit) {
      break;
    }
    std::string arg = lowerLine.substr(j + 1, k - (j + 1));
    arg.erase(std::remove_if(arg.begin(), arg.end(),
                             [](unsigned char ch) { return std::isspace(ch); }),
              arg.end());
    if (arg == "inout") {
      return parser::CharBlock{line.lineBegin + i, line.lineBegin + k + 1};
    }
    i = k + 1;
  }
  return std::nullopt;
}

void UnusedIntentCheck::CheckUnusedIntentHelper(
    semantics::SemanticsContext &context, const semantics::Scope &scope) {

  if (scope.IsModuleFile())
    return;

  // ignore interfaces
  if (const auto *sym = scope.symbol();
      sym && sym->detailsIf<semantics::SubprogramDetails>() &&
      sym->detailsIf<semantics::SubprogramDetails>()->isInterface()) {
    return;
  }

  auto WasDefined{[&context](const semantics::Symbol &symbol) {
    return context.IsSymbolDefined(symbol) ||
           semantics::IsInitialized(symbol, false, false, false);
  }};
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;

    if (const auto *details{symbol.detailsIf<semantics::ObjectEntityDetails>()};
        details && details->isDummy()) {
      const auto &owningProcScope = symbol.owner();
      const auto &owningProc = owningProcScope.symbol();

      if (procBindingDetailsSymbolsMap.find(owningProc) !=
          procBindingDetailsSymbolsMap.end()) {
        continue;
      }
      const bool shouldCheckUnusedIntentInOut =
          semantics::IsIntentInOut(symbol) && symbol.Rank() == 0 &&
          !semantics::IsUnlimitedPolymorphic(symbol) &&
          !semantics::IsAssumedType(symbol);
      if (!WasDefined(symbol) && shouldCheckUnusedIntentInOut) {
        Say(symbol.name(),
            "Dummy argument '%s' with intent(inout) is never written to, consider changing to intent(in)"_warn_en_US,
            symbol.name());

        if (auto line = utils::getSourceLineInfo(context, symbol.name());
            line && hasSingleEntityAfterDoubleColon(line->lineText)) {
          if (auto inoutRange = findIntentInoutRange(*line)) {
            const std::string replacementText =
                getIntentSpecSpelling(line->lineText, false);
            Fortran::tidy::FixItHint fix =
                Fortran::tidy::FixItHint::CreateReplacement(
                    *inoutRange, llvm::StringRef{replacementText});
            this->context()->addFixIt("bugprone-unused-intent", symbol.name(),
                                      fix);
          }
        }
        if (auto line = utils::getSourceLineInfo(context, symbol.name());
            line && !hasSingleEntityAfterDoubleColon(line->lineText) &&
            mixedIntentDeclsWithFix.insert(line->lineBegin).second) {
          const std::string indentFromSource =

              utils::getLeadingWhitespaceFromSourceLine(context, symbol.name());
          if (auto replacement = buildMixedIntentReplacement(
                  *line, symbol.name().ToString(), indentFromSource)) {
            Fortran::tidy::FixItHint fix =
                Fortran::tidy::FixItHint::CreateReplacement(
                    parser::CharBlock{line->lineBegin, line->lineEnd},
                    llvm::StringRef{*replacement});
            this->context()->addFixIt("bugprone-unused-intent", symbol.name(),
                                      fix);
          }
        }
      }
      if (!symbol.attrs().HasAny(
              {semantics::Attr::INTENT_IN, semantics::Attr::INTENT_INOUT,
               semantics::Attr::INTENT_OUT, semantics::Attr::VALUE})) {
        // warn about dummy arguments without explicit intent
        Say(symbol.name(),
            "Dummy argument '%s' has no explicit intent"_warn_en_US,
            symbol.name());

        if (symbol.attrs().test(semantics::Attr::TARGET) ||
            symbol.attrs().test(semantics::Attr::POINTER)) {
          // Pointer association through TARGET dummies can be misclassified
          // as a definition by semantics. Avoid intent auto-fixes here.
          continue;
        }

        /*
        bool isWrittenTo = WasDefined(symbol);
        if (auto line = utils::getSourceLineInfo(context, symbol.name())) {
          const std::string intentSpec =
              getIntentSpecSpelling(line->lineText, isWrittenTo);
          if (hasSingleEntityAfterDoubleColon(line->lineText)) {
            if (auto insertPos = utils::findDeclAttrInsertionPoint(
                    line->lineText, line->lineBegin)) {
              const std::string fixText = ", " + intentSpec;
              Fortran::tidy::FixItHint fix =
                  Fortran::tidy::FixItHint::CreateInsertion(
                      parser::CharBlock{*insertPos, *insertPos}, fixText);
              this->context()->addFixIt("bugprone-unused-intent", symbol.name(),
                                        fix);
            }
          } else {
            if (mixedMissingIntentDeclsWithFix.insert(line->lineBegin).second) {
              const std::string indentFromSource =
                  utils::getLeadingWhitespaceFromSourceLine(context,
                                                            symbol.name());
              if (auto replacement = buildMissingIntentReplacementForMixedDecl(
                      *line, scope, indentFromSource, WasDefined)) {
                Fortran::tidy::FixItHint fix =
                    Fortran::tidy::FixItHint::CreateReplacement(
                        parser::CharBlock{line->lineBegin, line->lineEnd},
                        llvm::StringRef{*replacement});
                this->context()->addFixIt("bugprone-unused-intent",
                                          symbol.name(), fix);
              } else {
                const std::string fixText =
                    buildMissingIntentInsertionForMixedDecl(
                        *line, symbol.name().ToString(), intentSpec,
                        indentFromSource)
                        .value_or(
                            "\n" + utils::getLeadingWhitespace(line->lineText) +
                            intentSpec +
                            utils::getDoubleColonSeparator(line->lineText) +
                            symbol.name().ToString());
                Fortran::tidy::FixItHint fix =
                    Fortran::tidy::FixItHint::CreateInsertion(
                        parser::CharBlock{line->lineEnd, line->lineEnd},
                        fixText);
                this->context()->addFixIt("bugprone-unused-intent",
                                          symbol.name(), fix);
              }
            }
          }
        }
         */
      }
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    CheckUnusedIntentHelper(context, child);
  }
}

static void MakeProcBindingSymbolSet(semantics::SemanticsContext &context,
                                     const semantics::Scope &scope) {
  for (const auto &pair : scope) {
    const semantics::Symbol &symbol = *pair.second;
    if (auto *details{symbol.detailsIf<semantics::ProcBindingDetails>()}) {
      procBindingDetailsSymbolsMap[&details->symbol()] = &symbol;
    }
  }

  for (const semantics::Scope &child : scope.children()) {
    MakeProcBindingSymbolSet(context, child);
  }
}

UnusedIntentCheck::UnusedIntentCheck(llvm::StringRef name,
                                     FlangTidyContext *context)
    : FlangTidyCheck{name, context} {

  MakeProcBindingSymbolSet(context->getSemanticsContext(),
                           context->getSemanticsContext().globalScope());

  CheckUnusedIntentHelper(context->getSemanticsContext(),
                          context->getSemanticsContext().globalScope());
}

} // namespace Fortran::tidy::bugprone
