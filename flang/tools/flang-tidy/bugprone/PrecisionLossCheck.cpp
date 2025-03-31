#include "PrecisionLossCheck.h"
#include "flang/Common/Fortran.h"
#include "flang/Evaluate/expression.h"
#include "flang/Evaluate/type.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"
#include <clang/Basic/SourceLocation.h>
#include <variant>

namespace Fortran::tidy::bugprone {

PrecisionLossCheck::PrecisionLossCheck(llvm::StringRef name,
                                       FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {}

static bool IsLossOfPrecision(const semantics::SomeExpr *lhs,
                              const semantics::SomeExpr *rhs,
                              bool hasExplicitKind, bool isConstantReal) {

  const auto &lhsType{lhs->GetType()};
  const auto &rhsType{rhs->GetType()};

  if (!lhsType || !rhsType)
    return false;

  auto lhsCat = lhsType->category();
  auto rhsCat = lhsType->category();

  // ignore derived types
  if (lhsCat == common::TypeCategory::Derived ||
      rhsCat == common::TypeCategory::Derived)
    return false;

  // this will fail if we call this on a derived type
  int lhsKind = lhsType->kind();
  int rhsKind = rhsType->kind();

  // integer -> integer, real, complex
  // real -> integer, real, complex
  // complex -> integer, real, complex
  //
  if (lhsCat == rhsCat && lhsKind < rhsKind)
    return true;

  // is the rhs is a literal and lhs has larger kind than the rhs
  if (isConstantReal && lhsKind > rhsKind && !hasExplicitKind) {
    return true;
  }

  // complex = real are basically real = real
  if (lhsCat == common::TypeCategory::Complex &&
      rhsCat == common::TypeCategory::Real) {
    if (lhsKind < rhsKind)
      return true;
    return false;
  }

  // real = complex loses precision
  if ((lhsCat == common::TypeCategory::Real ||
       lhsCat == common::TypeCategory::Integer) &&
      rhsCat == common::TypeCategory::Complex) {
    return true;
  }

  // integer = real/complex conversions always lose precision
  if (lhsCat == common::TypeCategory::Integer &&
      (rhsCat == common::TypeCategory::Real ||
       rhsCat == common::TypeCategory::Complex)) {
    return true;
  }

  // real/complex = integer conversions always lose precision
  if ((lhsCat == common::TypeCategory::Real ||
       lhsCat == common::TypeCategory::Complex) &&
      rhsCat == common::TypeCategory::Integer && lhsKind <= rhsKind) {
    return true;
  }

  return false;
}

bool shouldSuppressWarning(const parser::CharBlock &source,
                           llvm::StringRef checkName,
                           FlangTidyContext *context) {
  if (source.empty()) {
    return false;
  }

  // Get the provenance range for the source location
  const auto &cookedSources = context->getSemanticsContext().allCookedSources();
  const auto &allSources = cookedSources.allSources();

  auto provenanceRange = cookedSources.GetProvenanceRange(source);
  if (!provenanceRange) {
    return false;
  }

  // Get the source position
  auto srcPosition = allSources.GetSourcePosition(provenanceRange->start());
  if (!srcPosition) {
    return false;
  }

  int lineNum = srcPosition->line;

  // Get the source file and line
  std::size_t offset;
  const auto *sourceFile =
      allSources.GetSourceFile(provenanceRange->start(), &offset);
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
      if (comment.contains("(" + checkName.str() + ")") ||
          comment.contains("(bugprone-uninitialized-var)")) {
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

using namespace parser::literals;
void PrecisionLossCheck::Enter(const parser::AssignmentStmt &assignment) {
  const auto &var{std::get<parser::Variable>(assignment.t)};
  const auto &expr{std::get<parser::Expr>(assignment.t)};
  const auto *lhs{semantics::GetExpr(context_->getSemanticsContext(), var)};
  const auto *rhs{semantics::GetExpr(context_->getSemanticsContext(), expr)};

  if (!lhs || !rhs)
    return;

  bool hasExplicitKind = false;
  bool isConstantReal = false;
  // if the expr is LiteralConstant, we can get the value
  if (std::holds_alternative<parser::LiteralConstant>(expr.u)) {
    const auto &literal = std::get<parser::LiteralConstant>(expr.u);
    // if it holds a RealLiteralConstant, we can get the value
    if (std::holds_alternative<parser::RealLiteralConstant>(literal.u)) {
      const auto &realLiteral =
          std::get<parser::RealLiteralConstant>(literal.u);
      hasExplicitKind = realLiteral.kind.has_value();
      isConstantReal = true;
    }
  }

  if (IsLossOfPrecision(lhs, rhs, hasExplicitKind, isConstantReal)) {
    context_->getSemanticsContext().Say(
        context_->getSemanticsContext().location().value(),
        "Possible loss of precision in implicit conversion (%s to %s) "_warn_en_US,
        rhs->GetType()->AsFortran(), lhs->GetType()->AsFortran());
  }
}

} // namespace Fortran::tidy::bugprone
