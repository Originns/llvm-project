//===--- FlangTidy.cpp - flang-tidy ---------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "FlangTidy.h"
#include "FlangTidyContext.h"
#include "FlangTidyModule.h"
#include "FlangTidyModuleRegistry.h"
#include "MultiplexVisitor.h"
#include "flang/Frontend/CompilerInstance.h"
#include "flang/Frontend/CompilerInvocation.h"
#include "flang/Frontend/TextDiagnosticPrinter.h"
#include "flang/FrontendTool/Utils.h"
#include "flang/Parser/parsing.h"
#include "flang/Parser/provenance.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <clang/Basic/DiagnosticOptions.h>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <system_error>
#include <vector>

LLVM_INSTANTIATE_REGISTRY(Fortran::tidy::FlangTidyModuleRegistry)

namespace Fortran::tidy {

// Factory to populate MultiplexVisitor with all registered checks
class MultiplexVisitorFactory {
public:
  MultiplexVisitorFactory();

public:
  std::unique_ptr<FlangTidyCheckFactories> CheckFactories;
};

MultiplexVisitorFactory::MultiplexVisitorFactory()
    : CheckFactories(new FlangTidyCheckFactories) {
  // Traverse the FlangTidyModuleRegistry to register all checks
  for (auto &entry : FlangTidyModuleRegistry::entries()) {
    // Instantiate the module
    std::unique_ptr<FlangTidyModule> module = entry.instantiate();
    module->addCheckFactories(*CheckFactories);
  }
}

static std::string extractCheckName(const std::string &message) {
  size_t openBracket = message.rfind('[');
  size_t closeBracket = message.rfind(']');

  if (openBracket != std::string::npos && closeBracket != std::string::npos &&
      openBracket < closeBracket && closeBracket == message.length() - 1) {
    return message.substr(openBracket + 1, closeBracket - openBracket - 1);
  }

  return "";
}

static bool shouldSuppressWarning(const parser::ProvenanceRange &source,
                                  const std::string &checkName,
                                  FlangTidyContext *context) {
  if (source.empty()) {
    return false;
  }

  const auto &cookedSources = context->getSemanticsContext().allCookedSources();
  const auto &allSources = cookedSources.allSources();

  auto srcPosition = allSources.GetSourcePosition(source.start());
  if (!srcPosition) {
    return false;
  }

  int lineNum = srcPosition->line;

  // Get the source file and line
  std::size_t offset;
  const auto *sourceFile = allSources.GetSourceFile(source.start(), &offset);
  if (!sourceFile) {
    return false;
  }

  // Extract the line content from the source file
  auto checkForNoLint = [&](llvm::StringRef line) -> bool {
    // Look for ! NOLINT or !NOLINT
    auto commentPos = line.find('!');
    if (commentPos == llvm::StringRef::npos) {
      return false;
    }

    auto comment = line.substr(commentPos);

    // Check for NOLINT
    if (comment.contains("NOLINT") || comment.contains("nolint")) {
      // If there's no specific check mentioned, it applies to all checks
      if (!comment.contains("(")) {
        return true;
      }

      // Check for specific check name in parentheses
      if (comment.contains("(" + checkName + ")")) {
        return true;
      }
    }
    return false;
  };

  // Try to get the current line
  const auto &content = sourceFile->content();
  std::size_t lineStart = 0;
  std::size_t lineEnd = 0;
  int currentLine = 1;

  // Find the start of the current line
  for (std::size_t i = 0; i < content.size(); i++) {
    if (currentLine == lineNum) {
      lineStart = i;

      // Find the end of the line
      while (i < content.size() && content[i] != '\n') {
        i++;
      }
      lineEnd = i;

      // Check if this line has a NOLINT comment
      llvm::StringRef line(content.data() + lineStart, lineEnd - lineStart);
      if (checkForNoLint(line)) {
        return true;
      }

      // Check previous line if we can
      if (lineNum > 1 && lineStart > 0) {
        // Find the start of the previous line
        std::size_t prevLineEnd = lineStart - 1;
        std::size_t prevLineStart = prevLineEnd;

        // Go back to find the start of the previous line
        while (prevLineStart > 0 && content[prevLineStart - 1] != '\n') {
          prevLineStart--;
        }

        llvm::StringRef prevLine(content.data() + prevLineStart,
                                 prevLineEnd - prevLineStart);
        if (checkForNoLint(prevLine)) {
          return true;
        }
      }

      // We've checked the current and previous lines, so we're done
      break;
    }

    if (content[i] == '\n') {
      currentLine++;
    }
  }

  return false;
}

static void filterMessagesWithNoLint(parser::Messages &messages,
                                     FlangTidyContext *context) {
  std::vector<parser::Message> filteredMessages;

  const auto &cookedSources = context->getSemanticsContext().allCookedSources();

  for (const auto &message : messages.messages()) {
    // Extract the check name from the message text
    std::string checkName = extractCheckName(message.ToString());

    // Skip messages that should be suppressed
    const auto &provenanceRange = message.GetProvenanceRange(cookedSources);

    // get the charBlock
    if (!checkName.empty() && provenanceRange &&
        shouldSuppressWarning(provenanceRange.value(), checkName, context)) {
      continue;
    }

    // Keep messages that aren't suppressed
    filteredMessages.push_back(message);
  }

  // Replace the original messages with the filtered ones
  messages.clear();
  for (const auto &msg : filteredMessages) {
    messages.Say(msg);
  }
}

struct ResolvedFixEdit {
  std::string Path;
  std::size_t Begin{0};
  std::size_t End{0};
  std::string Replacement;
  std::string BaseContent;
};

static std::optional<ResolvedFixEdit>
resolveFixEdit(const RecordedFixIt &fix, semantics::SemanticsContext &context) {
  const bool hasRemoveLocation = fix.Hint.RemoveRange.begin() != nullptr;
  parser::CharBlock anchor =
      hasRemoveLocation ? fix.Hint.RemoveRange : fix.Location;
  if (anchor.begin() == nullptr) {
    return std::nullopt;
  }

  const auto &cookedSources = context.allCookedSources();
  auto provenanceRange = cookedSources.GetProvenanceRange(anchor);
  // Zero-length insertion anchors can fail direct lookup; probe one char.
  if (!provenanceRange && anchor.empty()) {
    provenanceRange =
        cookedSources.GetProvenanceRange(parser::CharBlock{anchor.begin(), 1});
  }
  if (!provenanceRange) {
    return std::nullopt;
  }

  std::size_t anchorOffset{0};
  const auto &allSources = cookedSources.allSources();
  const auto *sourceFile =
      allSources.GetSourceFile(provenanceRange->start(), &anchorOffset);
  if (!sourceFile) {
    return std::nullopt;
  }

  const auto contentArray = sourceFile->content();
  const llvm::StringRef content{contentArray.data(), contentArray.size()};
  if (anchorOffset > content.size()) {
    return std::nullopt;
  }

  // Convert edit boundaries through provenance, not raw pointer arithmetic.
  // FixIt locations can point into cooked source buffers.
  std::size_t replaceBegin{anchorOffset};
  std::size_t replaceEnd{anchorOffset};

  if (!fix.Hint.RemoveRange.empty()) {
    auto removeRange = cookedSources.GetProvenanceRange(fix.Hint.RemoveRange);
    if (!removeRange || removeRange->empty()) {
      return std::nullopt;
    }

    const auto *beginFile =
        allSources.GetSourceFile(removeRange->start(), &replaceBegin);
    if (!beginFile || beginFile != sourceFile) {
      return std::nullopt;
    }

    std::size_t lastOffset{0};
    const auto lastProv = removeRange->start() + (removeRange->size() - 1);
    const auto *endFile = allSources.GetSourceFile(lastProv, &lastOffset);
    if (!endFile || endFile != sourceFile) {
      return std::nullopt;
    }
    replaceEnd = lastOffset + 1;
  }

  if (replaceBegin > replaceEnd || replaceEnd > content.size()) {
    return std::nullopt;
  }

  std::string replacement;
  if (!fix.Hint.CodeToInsert.empty()) {
    replacement = fix.Hint.CodeToInsert;
  } else if (!fix.Hint.InsertFromRange.empty()) {
    replacement = fix.Hint.InsertFromRange.ToString();
  }

  return ResolvedFixEdit{sourceFile->path(), replaceBegin, replaceEnd,
                         std::move(replacement), content.str()};
}

static std::optional<std::string>
renderFixedLine(const RecordedFixIt &fix,
                semantics::SemanticsContext &context) {
  auto resolved = resolveFixEdit(fix, context);
  if (!resolved) {
    return std::nullopt;
  }
  const llvm::StringRef content{resolved->BaseContent};
  if (resolved->Begin > content.size() || resolved->End > content.size()) {
    return std::nullopt;
  }
  std::size_t lineStart = resolved->Begin;
  while (lineStart > 0 && content[lineStart - 1] != '\n') {
    --lineStart;
  }
  std::size_t lineEnd = resolved->End;
  while (lineEnd < content.size() && content[lineEnd] != '\n') {
    ++lineEnd;
  }

  std::string fixedLine =
      std::string(content.substr(lineStart, lineEnd - lineStart));

  if (resolved->Begin < lineStart || resolved->End > lineEnd ||
      resolved->Begin > resolved->End) {
    return std::nullopt;
  }
  const std::size_t relBegin = resolved->Begin - lineStart;
  const std::size_t relLen = resolved->End - resolved->Begin;
  fixedLine.replace(relBegin, relLen, resolved->Replacement);

  return fixedLine;
}

struct ApplyFixesResult {
  std::size_t AppliedCount{0};
  std::size_t DeduplicatedCount{0};
  std::size_t FailedCount{0};
};

static ApplyFixesResult
applyFixesInPlace(const std::vector<RecordedFixIt> &fixes,
                  semantics::SemanticsContext &context) {
  std::map<std::string, std::vector<ResolvedFixEdit>> editsByFile;
  std::map<std::string, std::string> fileContent;
  ApplyFixesResult result;

  for (const auto &fix : fixes) {
    auto edit = resolveFixEdit(fix, context);
    if (!edit) {
      llvm::errs() << "warning: skipping fix with invalid location\n";
      ++result.FailedCount;
      continue;
    }
    if (edit->Path.empty()) {
      llvm::errs() << "warning: skipping fix with no associated file\n";
      ++result.FailedCount;
      continue;
    }
    if (!fileContent.count(edit->Path))
      fileContent[edit->Path] = edit->BaseContent;
    editsByFile[edit->Path].push_back(std::move(*edit));
  }

  for (auto &[path, edits] : editsByFile) {
    auto contentIt = fileContent.find(path);
    if (contentIt == fileContent.end()) {
      continue;
    }
    std::string &content = contentIt->second;

    std::sort(edits.begin(), edits.end(),
              [](const ResolvedFixEdit &a, const ResolvedFixEdit &b) {
                if (a.Begin != b.Begin)
                  return a.Begin < b.Begin;
                if (a.End != b.End)
                  return a.End < b.End;
                return a.Replacement < b.Replacement;
              });
    const std::size_t beforeDedupe = edits.size();
    edits.erase(
        std::unique(edits.begin(), edits.end(),
                    [](const ResolvedFixEdit &a, const ResolvedFixEdit &b) {
                      return a.Begin == b.Begin && a.End == b.End &&
                             a.Replacement == b.Replacement;
                    }),
        edits.end());
    result.DeduplicatedCount += beforeDedupe - edits.size();

    bool hasOverlap = false;
    for (std::size_t i = 1; i < edits.size(); ++i) {
      if (edits[i - 1].End > edits[i].Begin) {
        hasOverlap = true;
        break;
      }
    }
    if (hasOverlap) {
      llvm::errs() << "warning: skipping overlapping fixes in file '" << path
                   << "'\n";
      result.FailedCount += edits.size();
      continue;
    }

    std::size_t fileAppliedCount{0};
    for (auto it = edits.rbegin(); it != edits.rend(); ++it) {
      if (it->End > content.size() || it->Begin > it->End) {
        llvm::errs() << "warning: skipping out-of-range fix in file '" << path
                     << "'\n";
        ++result.FailedCount;
        continue;
      }
      content.replace(it->Begin, it->End - it->Begin, it->Replacement);
      ++fileAppliedCount;
    }

    std::error_code ec;
    llvm::raw_fd_ostream out(path, ec, llvm::sys::fs::OF_None);
    if (ec) {
      llvm::errs() << "error: failed to write fixes to '" << path
                   << "': " << ec.message() << "\n";
      result.FailedCount += fileAppliedCount;
      continue;
    }
    out << content;
    result.AppliedCount += fileAppliedCount;
  }

  return result;
}

int runFlangTidy(const FlangTidyOptions &options) {
  auto flang = std::make_unique<Fortran::frontend::CompilerInstance>();

  // create diagnostics engine
  flang->createDiagnostics();
  if (!flang->hasDiagnostics()) {
    llvm::errs() << "Failed to create diagnostics engine\n";
    return 1;
  }

  // create diagnostics buffer
  auto diagsPrinter =
      std::make_unique<Fortran::frontend::TextDiagnosticPrinter>(
          llvm::outs(), flang->getDiagnostics().getDiagnosticOptions());

  flang->getDiagnostics().setClient(diagsPrinter.get(), false);

  // convert the options to a format that can be passed to the compiler
  // invocation
  std::vector<const char *> cstrArgs;

  // add input files
  for (const auto &sourcePath : options.sourcePaths) {
    cstrArgs.push_back(sourcePath.c_str());
  }

  // add extra args before (if present)
  if (options.ExtraArgsBefore) {
    for (const auto &arg : *options.ExtraArgsBefore) {
      cstrArgs.push_back(arg.c_str());
    }
  }

  // add extra args (if present)
  if (options.ExtraArgs) {
    for (const auto &arg : *options.ExtraArgs) {
      cstrArgs.push_back(arg.c_str());
    }
  }

  llvm::ArrayRef<const char *> argv(cstrArgs);
  flang->getFrontendOpts().programAction = frontend::ParseSyntaxOnly;

  // parse arguments
  bool success = Fortran::frontend::CompilerInvocation::createFromArgs(
      flang->getInvocation(), argv, flang->getDiagnostics(), options.argv0);

  // initialize targets
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();

  // emit diagnostics
  if (!success) {
    llvm::errs() << "Failed to parse arguments\n";
    return 1;
  }

  // run the compiler instance
  if (!Fortran::frontend::executeCompilerInvocation(flang.get())) {
    return 1;
  }

  // handle help and version flags
  if (flang->getFrontendOpts().showHelp ||
      flang->getFrontendOpts().showVersion) {
    return 0;
  }

  // get the parse tree
  auto &parsing = flang->getParsing();
  auto &parseTree = parsing.parseTree();
  if (!parseTree) {
    return 1;
  }

  auto &semantics = flang->getSemantics();
  auto &semanticsContext = semantics.context();

  FlangTidyContext context{options, &semanticsContext};

  MultiplexVisitorFactory visitorFactory{};
  MultiplexVisitor visitor{semanticsContext};
  auto checks = visitorFactory.CheckFactories->createChecks(&context);

  for (auto &&check : checks) {
    visitor.AddChecker(std::move(check));
  }

  visitor.Walk(*parseTree);

  auto &messages = context.Context->messages();
  filterMessagesWithNoLint(messages, &context);

  bool hasFatalError = messages.AnyFatalError();

  semantics.EmitMessages(llvm::outs());

  for (const auto &fix : context.getFixIts()) {
    llvm::outs() << "fix-it[" << fix.CheckName << "]";
    if (!fix.Location.empty()) {
      llvm::outs() << " at '" << fix.Location.ToString() << "'";
    }
    if (!fix.Hint.CodeToInsert.empty()) {
      llvm::outs() << ": insert \"" << fix.Hint.CodeToInsert << "\"";
    } else if (!fix.Hint.InsertFromRange.empty()) {
      llvm::outs() << ": insert-from-range '"
                   << fix.Hint.InsertFromRange.ToString() << "'";
    } else if (!fix.Hint.RemoveRange.empty()) {
      llvm::outs() << ": remove '" << fix.Hint.RemoveRange.ToString() << "'";
    }
    llvm::outs() << "\n";
    if (auto fixedLine = renderFixedLine(fix, semanticsContext)) {
      llvm::outs() << "  fixed: " << *fixedLine << "\n";
    }
  }

  if (options.Fix && !context.getFixIts().empty()) {
    const auto applyResult =
        applyFixesInPlace(context.getFixIts(), semanticsContext);
    llvm::outs() << "applied " << applyResult.AppliedCount << " fix-it(s)\n";
    if (applyResult.FailedCount != 0) {
      llvm::errs() << "warning: some fixes could not be applied\n";
    }
  }

  return hasFatalError ? 1 : 0;
}

} // namespace Fortran::tidy
