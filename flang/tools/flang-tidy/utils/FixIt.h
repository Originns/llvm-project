//===-- FlangFixIt.h - FixIt Hint for Flang ----------------------*- C++
//-*-===//
//
// Defines a lightweight, Flang-native analogue of Clang's FixItHint.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_UTILS_FIXIT_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_UTILS_FIXIT_H

#include "flang/Parser/char-block.h"
#include "flang/Parser/provenance.h"
#include <string>

namespace Fortran::tidy {

/// Represents a code edit (insert, remove, replace) associated with a
/// diagnostic.
class FixItHint {
public:
  /// The range of code to remove or replace.
  parser::CharBlock RemoveRange;

  /// The source range whose text should be inserted (if any).
  parser::CharBlock InsertFromRange;

  /// Literal text to insert at the insertion location.
  std::string CodeToInsert;

  /// Whether insertion should occur before previous ones at the same location.
  bool BeforePreviousInsertions{false};

  FixItHint() = default;

  bool isNull() const {
    return RemoveRange.empty() && CodeToInsert.empty() &&
           InsertFromRange.empty();
  }

  //===------------------------------------------------------------------===//
  // Factory methods
  //===------------------------------------------------------------------===//

  static FixItHint CreateInsertion(parser::CharBlock InsertionLoc,
                                   llvm::StringRef Code,
                                   bool BeforePreviousInsertions = false) {
    FixItHint Hint;
    // A zero-length block at the insertion point
    Hint.RemoveRange = parser::CharBlock(InsertionLoc.begin(), 0ul);
    Hint.CodeToInsert = Code.str();
    Hint.BeforePreviousInsertions = BeforePreviousInsertions;
    return Hint;
  }

  static FixItHint
  CreateInsertionFromRange(parser::CharBlock InsertionLoc,
                           parser::CharBlock FromRange,
                           bool BeforePreviousInsertions = false) {
    FixItHint Hint;
    Hint.RemoveRange = parser::CharBlock{InsertionLoc.begin(), 0ul};
    Hint.InsertFromRange = FromRange;
    Hint.BeforePreviousInsertions = BeforePreviousInsertions;
    return Hint;
  }

  static FixItHint CreateRemoval(parser::CharBlock RemoveRange) {
    FixItHint Hint;
    Hint.RemoveRange = RemoveRange;
    return Hint;
  }

  static FixItHint CreateReplacement(parser::CharBlock RemoveRange,
                                     llvm::StringRef Code) {
    FixItHint Hint;
    Hint.RemoveRange = RemoveRange;
    Hint.CodeToInsert = Code.str();
    return Hint;
  }

  static FixItHint CreateReplacement(parser::CharBlock RemoveRange,
                                     parser::CharBlock FromRange) {
    FixItHint Hint;
    Hint.RemoveRange = RemoveRange;
    Hint.InsertFromRange = FromRange;
    return Hint;
  }
};

} // namespace Fortran::tidy

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_UTILS_FIXIT_H
