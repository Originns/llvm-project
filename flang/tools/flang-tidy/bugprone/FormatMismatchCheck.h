//===--- FormatMismatchCheck.h - flang-tidy ---------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_FORMATMISMATCHCHECK_H
#define LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_FORMATMISMATCHCHECK_H

#include "FlangTidyCheck.h"
#include "flang/Parser/format-specification.h"
#include <cstdint>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace Fortran::tidy::bugprone {

/// The class of list item a data edit descriptor may be applied to.
enum class DescriptorClass {
  Integer,   // I
  Bits,      // B, O, Z -- accept any intrinsic type
  RealLike,  // F, E, EN, ES, EX, D
  Any,       // G
  Logical,   // L
  Character, // A
  Derived,   // DT
};

struct FormatDescriptor {
  DescriptorClass cls;
  const char *spelling;
};

/// A format specification reduced to the sequence of data edit descriptors it
/// applies to an I/O list, plus the point at which format reversion restarts.
struct FormatShape {
  std::vector<FormatDescriptor> descriptors;
  /// Index into \c descriptors at which format control reverts once the
  /// format is exhausted while I/O list items remain (F2018 13.4).
  std::size_t reversionPoint{0};
  /// A `*(...)` unlimited format item is present, so the descriptor sequence
  /// is effectively inexhaustible and counts cannot be compared.
  bool unlimited{false};
};

/// This check compares the data edit descriptors of a format specification
/// against the types and the number of items in the corresponding I/O list.
///
/// For the user-facing documentation see:
/// https://flang.llvm.org/@PLACEHOLDER@/format-mismatch.html
class FormatMismatchCheck : public virtual FlangTidyCheck {
public:
  using FlangTidyCheck::FlangTidyCheck;
  virtual ~FormatMismatchCheck() = default;

  void
  Enter(const parser::Statement<common::Indirection<parser::FormatStmt>> &)
      override;
  void Leave(const parser::WriteStmt &) override;
  void Leave(const parser::PrintStmt &) override;
  void Leave(const parser::ReadStmt &) override;
  void Leave(const parser::ProgramUnit &) override;

private:
  /// One I/O list item, resolved to what the format has to satisfy.
  struct IoItem {
    common::TypeCategory category;
    std::string typeName;
    parser::CharBlock at;
    /// Number of array elements; std::nullopt when the count is not known.
    std::optional<std::int64_t> elements;
  };

  /// An I/O statement whose format is a statement label.  Diagnosis is
  /// deferred until the enclosing program unit has been traversed, because a
  /// FORMAT statement may follow the statement that references it.
  struct PendingIoStmt {
    parser::Label label;
    std::vector<IoItem> items;
    parser::CharBlock at;
    const char *itemWord;
  };

  template <typename ITEMS>
  void Handle(const std::optional<parser::Format> &format,
              const std::list<parser::IoControlSpec> &controls,
              const ITEMS &items, const char *itemWord);

  template <typename ITEMS>
  void HandleFormat(const parser::Format &format, const ITEMS &items,
                    const char *itemWord);

  template <typename ITEMS>
  bool CollectItems(const ITEMS &items, std::vector<IoItem> &out,
                    bool insideImpliedDo);

  bool CollectOne(const semantics::SomeExpr *expr, parser::CharBlock at,
                  std::vector<IoItem> &out, bool insideImpliedDo);

  void Diagnose(const FormatShape &shape, const std::vector<IoItem> &items,
                parser::CharBlock at, const char *itemWord);

  std::map<parser::Label, const format::FormatSpecification *> labeledFormats_;
  std::set<parser::Label> ambiguousLabels_;
  std::vector<PendingIoStmt> pending_;
};

} // namespace Fortran::tidy::bugprone

#endif // LLVM_FLANG_TOOLS_FLANG_TIDY_BUGPRONE_FORMATMISMATCHCHECK_H
