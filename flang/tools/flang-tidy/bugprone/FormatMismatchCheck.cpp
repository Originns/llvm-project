//===--- FormatMismatchCheck.cpp - flang-tidy -----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "FormatMismatchCheck.h"
#include "flang/Common/idioms.h"
#include "flang/Common/visit.h"
#include "flang/Evaluate/shape.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/tools.h"
#include <cctype>

namespace Fortran::tidy::bugprone {

namespace {

/// Formats with absurd repeat counts are not worth expanding; such a format is
/// simply not analysed.
constexpr std::size_t kMaxDescriptors{4096};

bool Accepts(DescriptorClass cls, common::TypeCategory cat) {
  switch (cls) {
  case DescriptorClass::Integer:
    return cat == common::TypeCategory::Integer;
  case DescriptorClass::Bits:
    // F2018 13.7.2.1: B, O and Z accept any intrinsic type.
    return true;
  case DescriptorClass::RealLike:
    return cat == common::TypeCategory::Real ||
           cat == common::TypeCategory::Complex;
  case DescriptorClass::Any:
    return true;
  case DescriptorClass::Logical:
    return cat == common::TypeCategory::Logical;
  case DescriptorClass::Character:
    return cat == common::TypeCategory::Character;
  case DescriptorClass::Derived:
    return cat == common::TypeCategory::Derived;
  }
  return true;
}

/// Append \p repeat - 1 further copies of descriptors[first..end).
bool RepeatRange(FormatShape &shape, std::size_t first, std::uint64_t repeat) {
  std::size_t count{shape.descriptors.size() - first};
  if (count == 0 || repeat <= 1)
    return true;
  if (count * repeat > kMaxDescriptors)
    return false;
  shape.descriptors.reserve(first + count * repeat);
  for (std::uint64_t i{1}; i < repeat; ++i)
    for (std::size_t j{0}; j < count; ++j)
      shape.descriptors.push_back(shape.descriptors[first + j]);
  return true;
}

//===----------------------------------------------------------------------===//
// Scanning a format given as a character string
//===----------------------------------------------------------------------===//

/// Reduces the text of a format specification to its data edit descriptors.
/// Anything malformed makes the scan fail, in which case the statement is left
/// alone -- Flang's own FormatValidator already reports invalid formats.
class FormatTextScanner {
public:
  FormatTextScanner(const std::string &text, FormatShape &shape)
      : t_{text}, shape_{shape} {}

  bool Scan() {
    SkipBlanks();
    if (Peek() != '(')
      return false;
    ++p_;
    return ScanItemList(/*topLevel=*/true);
  }

private:
  bool AtEnd() const { return p_ >= t_.size(); }
  char Peek() const { return AtEnd() ? '\0' : t_[p_]; }
  char PeekAt(std::size_t off) const {
    return p_ + off >= t_.size() ? '\0' : t_[p_ + off];
  }
  static char Upper(char c) {
    return static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
  }
  static bool IsDigit(char c) { return c >= '0' && c <= '9'; }

  void SkipBlanks() {
    while (!AtEnd() && (t_[p_] == ' ' || t_[p_] == '\t'))
      ++p_;
  }
  void SkipSeparators() {
    while (!AtEnd() && (t_[p_] == ' ' || t_[p_] == '\t' || t_[p_] == ','))
      ++p_;
  }
  void SkipDigits() {
    while (!AtEnd() && IsDigit(t_[p_]))
      ++p_;
  }

  bool ReadInt(std::uint64_t &value) {
    std::uint64_t v{0};
    bool any{false};
    while (!AtEnd() && IsDigit(t_[p_])) {
      if (v > (kMaxDescriptors + 1))
        return false; // absurd repeat count
      v = v * 10 + static_cast<std::uint64_t>(t_[p_] - '0');
      ++p_;
      any = true;
    }
    value = v;
    return any;
  }

  /// Skips a char-string-edit-desc, honouring doubled quotes.
  bool SkipCharString() {
    char quote{Peek()};
    ++p_;
    while (!AtEnd()) {
      if (t_[p_] == quote) {
        if (PeekAt(1) == quote) {
          p_ += 2;
          continue;
        }
        ++p_;
        return true;
      }
      ++p_;
    }
    return false;
  }

  /// Skips `w[.d][Ee]` following a data edit descriptor.
  void SkipFieldSpec() {
    SkipBlanks();
    SkipDigits();
    SkipBlanks();
    if (Peek() == '.') {
      ++p_;
      SkipBlanks();
      SkipDigits();
    }
    SkipBlanks();
    if (Upper(Peek()) == 'E') {
      // Only an exponent width, never the start of the next descriptor.
      std::size_t save{p_};
      ++p_;
      SkipBlanks();
      if (IsDigit(Peek()))
        SkipDigits();
      else
        p_ = save;
    }
  }

  bool Push(DescriptorClass cls, const char *spelling, std::uint64_t repeat) {
    if (shape_.descriptors.size() + repeat > kMaxDescriptors)
      return false;
    for (std::uint64_t i{0}; i < repeat; ++i)
      shape_.descriptors.push_back({cls, spelling});
    return true;
  }

  bool ScanEditDescriptor(std::uint64_t repeat) {
    char c{Upper(Peek())};
    char n{Upper(PeekAt(1))};

    // Two-letter control edit descriptors, which consume no list item.
    if ((c == 'D' && (n == 'C' || n == 'P')) ||
        (c == 'B' && (n == 'N' || n == 'Z')) ||
        (c == 'S' && (n == 'S' || n == 'P')) ||
        (c == 'R' &&
            (n == 'U' || n == 'D' || n == 'Z' || n == 'N' || n == 'C' ||
                n == 'P'))) {
      p_ += 2;
      return true;
    }
    if (c == 'T' && (n == 'L' || n == 'R')) {
      p_ += 2;
      SkipBlanks();
      SkipDigits();
      return true;
    }

    // DT [char-literal] [(v-list)]
    if (c == 'D' && n == 'T') {
      p_ += 2;
      SkipBlanks();
      if ((Peek() == '\'' || Peek() == '"') && !SkipCharString())
        return false;
      SkipBlanks();
      if (Peek() == '(') {
        while (!AtEnd() && t_[p_] != ')')
          ++p_;
        if (AtEnd())
          return false;
        ++p_;
      }
      return Push(DescriptorClass::Derived, "DT", repeat);
    }

    // F2023 AT: like A, but takes no field width.  Must be recognised before
    // a bare A, or it lexes as A followed by a T control descriptor.
    if (c == 'A' && n == 'T') {
      p_ += 2;
      return Push(DescriptorClass::Character, "AT", repeat);
    }

    // EN, ES and EX must be recognised before a bare E.
    if (c == 'E' && (n == 'N' || n == 'S' || n == 'X')) {
      const char *spelling{n == 'N' ? "EN" : n == 'S' ? "ES" : "EX"};
      p_ += 2;
      SkipFieldSpec();
      return Push(DescriptorClass::RealLike, spelling, repeat);
    }

    switch (c) {
    case 'I':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::Integer, "I", repeat);
    case 'B':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::Bits, "B", repeat);
    case 'O':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::Bits, "O", repeat);
    case 'Z':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::Bits, "Z", repeat);
    case 'F':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::RealLike, "F", repeat);
    case 'E':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::RealLike, "E", repeat);
    case 'D':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::RealLike, "D", repeat);
    case 'G':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::Any, "G", repeat);
    case 'L':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::Logical, "L", repeat);
    case 'A':
      ++p_;
      SkipFieldSpec();
      return Push(DescriptorClass::Character, "A", repeat);
    // Control edit descriptors: no list item is consumed.
    case 'T':
      ++p_;
      SkipBlanks();
      SkipDigits();
      return true;
    case 'X':
    case 'P':
    case 'S':
    case '/':
    case ':':
    case '$':
    case '\\':
      ++p_;
      return true;
    default:
      return false;
    }
  }

  bool ScanItemList(bool topLevel) {
    while (true) {
      SkipSeparators();
      if (AtEnd())
        return false; // unterminated format
      if (Peek() == ')') {
        ++p_;
        return true;
      }

      std::size_t itemStart{shape_.descriptors.size()};

      if (Peek() == '*') { // unlimited-format-item
        ++p_;
        SkipBlanks();
        if (Peek() != '(')
          return false;
        ++p_;
        shape_.unlimited = true;
        if (topLevel)
          shape_.reversionPoint = itemStart;
        if (!ScanItemList(/*topLevel=*/false))
          return false;
        continue;
      }

      std::uint64_t repeat{1};
      bool hasRepeat{false};
      if (IsDigit(Peek())) {
        if (!ReadInt(repeat))
          return false;
        hasRepeat = true;
      }
      SkipBlanks();
      if (AtEnd())
        return false;

      // Hollerith: the digits just read are its length, not a repeat count.
      if (Upper(Peek()) == 'H') {
        if (!hasRepeat)
          return false;
        ++p_;
        if (p_ + repeat > t_.size())
          return false;
        p_ += repeat;
        continue;
      }
      if (Peek() == '\'' || Peek() == '"') {
        if (!SkipCharString())
          return false;
        continue;
      }
      if (Peek() == '(') {
        ++p_;
        if (topLevel)
          shape_.reversionPoint = itemStart;
        if (!ScanItemList(/*topLevel=*/false))
          return false;
        if (!RepeatRange(shape_, itemStart, repeat))
          return false;
        continue;
      }
      if (!ScanEditDescriptor(repeat))
        return false;
    }
  }

  const std::string &t_;
  FormatShape &shape_;
  std::size_t p_{0};
};

bool ScanFormatText(const std::string &text, FormatShape &shape) {
  FormatTextScanner scanner{text, shape};
  return scanner.Scan();
}

//===----------------------------------------------------------------------===//
// Reducing an already-parsed FORMAT statement
//===----------------------------------------------------------------------===//

bool FlattenItems(const std::list<format::FormatItem> &items, FormatShape &shape,
                  bool topLevel);

bool FlattenOne(const format::FormatItem &item, FormatShape &shape,
                bool topLevel) {
  std::uint64_t repeat{item.repeatCount.value_or(1)};
  std::size_t itemStart{shape.descriptors.size()};
  bool ok{true};
  auto push = [&](DescriptorClass cls, const char *spelling) {
    if (shape.descriptors.size() + repeat > kMaxDescriptors) {
      ok = false;
      return;
    }
    for (std::uint64_t i{0}; i < repeat; ++i)
      shape.descriptors.push_back({cls, spelling});
  };

  common::visit(
      common::visitors{
          [&](const format::IntrinsicTypeDataEditDesc &d) {
            using Kind = format::IntrinsicTypeDataEditDesc::Kind;
            switch (d.kind) {
            case Kind::I:
              push(DescriptorClass::Integer, "I");
              break;
            case Kind::B:
              push(DescriptorClass::Bits, "B");
              break;
            case Kind::O:
              push(DescriptorClass::Bits, "O");
              break;
            case Kind::Z:
              push(DescriptorClass::Bits, "Z");
              break;
            case Kind::F:
              push(DescriptorClass::RealLike, "F");
              break;
            case Kind::E:
              push(DescriptorClass::RealLike, "E");
              break;
            case Kind::EN:
              push(DescriptorClass::RealLike, "EN");
              break;
            case Kind::ES:
              push(DescriptorClass::RealLike, "ES");
              break;
            case Kind::EX:
              push(DescriptorClass::RealLike, "EX");
              break;
            case Kind::D:
              push(DescriptorClass::RealLike, "D");
              break;
            case Kind::G:
              push(DescriptorClass::Any, "G");
              break;
            case Kind::L:
              push(DescriptorClass::Logical, "L");
              break;
            case Kind::A:
              push(DescriptorClass::Character, "A");
              break;
            case Kind::AT:
              // F2023: character output with trailing blanks trimmed.
              push(DescriptorClass::Character, "AT");
              break;
            }
          },
          [&](const format::DerivedTypeDataEditDesc &) {
            push(DescriptorClass::Derived, "DT");
          },
          [&](const format::ControlEditDesc &) {},
          [&](const std::string &) {},
          [&](const std::list<format::FormatItem> &nested) {
            if (topLevel)
              shape.reversionPoint = itemStart;
            ok = FlattenItems(nested, shape, /*topLevel=*/false) &&
                 RepeatRange(shape, itemStart, repeat);
          },
      },
      item.u);
  return ok;
}

bool FlattenItems(const std::list<format::FormatItem> &items, FormatShape &shape,
                  bool topLevel) {
  for (const auto &item : items)
    if (!FlattenOne(item, shape, topLevel))
      return false;
  return true;
}

bool FlattenFormatSpec(const format::FormatSpecification &spec,
                       FormatShape &shape) {
  if (!FlattenItems(spec.items, shape, /*topLevel=*/true))
    return false;
  if (!spec.unlimitedItems.empty()) {
    shape.unlimited = true;
    shape.reversionPoint = shape.descriptors.size();
    if (!FlattenItems(spec.unlimitedItems, shape, /*topLevel=*/false))
      return false;
  }
  return true;
}

} // namespace

//===----------------------------------------------------------------------===//
// Collecting the I/O list
//===----------------------------------------------------------------------===//

bool FormatMismatchCheck::CollectOne(const semantics::SomeExpr *expr,
                                     parser::CharBlock at,
                                     std::vector<IoItem> &out,
                                     bool insideImpliedDo) {
  if (!expr)
    return false;
  auto type{expr->GetType()};
  if (!type)
    return false;
  // Derived-type items are transferred through defined I/O, whose descriptor
  // requirements are not modelled here.
  if (type->category() == common::TypeCategory::Derived)
    return false;

  IoItem item;
  item.category = type->category();
  item.typeName = type->AsFortran();
  item.at = at;
  if (insideImpliedDo) {
    item.elements = std::nullopt;
  } else if (expr->Rank() == 0) {
    item.elements = 1;
  } else {
    auto &foldingContext{context()->getSemanticsContext().foldingContext()};
    if (auto shape{evaluate::GetShape(foldingContext, *expr)}) {
      if (auto extents{evaluate::AsConstantExtents(foldingContext, *shape)}) {
        std::int64_t n{1};
        for (auto extent : *extents)
          n *= extent;
        item.elements = n;
      }
    }
  }
  out.push_back(std::move(item));
  return true;
}

template <typename ITEMS>
bool FormatMismatchCheck::CollectItems(const ITEMS &items,
                                       std::vector<IoItem> &out,
                                       bool insideImpliedDo) {
  auto &semanticsContext{context()->getSemanticsContext()};
  for (const auto &item : items) {
    bool ok{true};
    common::visit(
        [&](const auto &x) {
          using T = std::decay_t<decltype(x)>;
          if constexpr (std::is_same_v<T, parser::Expr>) {
            ok = CollectOne(semantics::GetExpr(semanticsContext, x), x.source,
                out, insideImpliedDo);
          } else if constexpr (std::is_same_v<T, parser::Variable>) {
            ok = CollectOne(semantics::GetExpr(semanticsContext, x),
                x.GetSource(), out, insideImpliedDo);
          } else {
            // An implied-DO contributes an unknown number of elements, which
            // downgrades the analysis to a type-only check.
            ok = CollectItems(
                std::get<0>(x.value().t), out, /*insideImpliedDo=*/true);
          }
        },
        item.u);
    if (!ok)
      return false;
  }
  return true;
}

//===----------------------------------------------------------------------===//
// Diagnosis
//===----------------------------------------------------------------------===//

using namespace parser::literals;

void FormatMismatchCheck::Diagnose(const FormatShape &shape,
                                   const std::vector<IoItem> &items,
                                   parser::CharBlock at,
                                   const char *itemWord) {
  const auto &descriptors{shape.descriptors};

  if (items.empty()) {
    if (!descriptors.empty() && !shape.unlimited)
      Say(at,
          "Format specifies %d data edit descriptor(s) but no %s item is supplied"_warn_en_US,
          static_cast<int>(descriptors.size()), itemWord);
    return;
  }

  if (descriptors.empty()) {
    Say(at,
        "Format contains no data edit descriptor but %s items are supplied"_warn_en_US,
        itemWord);
    return;
  }

  bool countsKnown{!shape.unlimited};
  std::int64_t totalSlots{0};
  for (const auto &item : items) {
    if (!item.elements) {
      countsKnown = false;
      break;
    }
    totalSlots += *item.elements *
        (item.category == common::TypeCategory::Complex ? 2 : 1);
  }

  if (!countsKnown) {
    // The positions cannot be paired up, so report only an item whose type no
    // descriptor in the format accepts.
    for (const auto &item : items) {
      bool anyAccepts{false};
      for (const auto &descriptor : descriptors) {
        if (Accepts(descriptor.cls, item.category)) {
          anyAccepts = true;
          break;
        }
      }
      if (!anyAccepts)
        Say(item.at,
            "%s item of type %s is not accepted by any data edit descriptor in the format"_warn_en_US,
            itemWord, item.typeName);
    }
    return;
  }

  // Walk the list items against the descriptors, reverting as the runtime
  // would once the format is exhausted.
  std::size_t next{0};
  bool reverted{false};
  for (const auto &item : items) {
    std::int64_t slots{*item.elements *
        (item.category == common::TypeCategory::Complex ? 2 : 1)};
    bool reported{false};
    for (std::int64_t slot{0}; slot < slots; ++slot) {
      if (next >= descriptors.size()) {
        next = shape.reversionPoint < descriptors.size() ? shape.reversionPoint
                                                         : 0;
        reverted = true;
      }
      const auto &descriptor{descriptors[next++]};
      if (!reported && !Accepts(descriptor.cls, item.category)) {
        Say(item.at,
            "%s item of type %s is transferred with the '%s' edit descriptor"_warn_en_US,
            itemWord, item.typeName, descriptor.spelling);
        reported = true;
      }
    }
  }

  if (reverted) {
    Say(at,
        "Format specifies %d data edit descriptor(s) for %d effective %s item(s); format control reverts and reuses the format"_warn_en_US,
        static_cast<int>(descriptors.size()), static_cast<int>(totalSlots),
        itemWord);
  } else if (next < descriptors.size()) {
    Say(at,
        "Format specifies %d data edit descriptor(s) but only %d effective %s item(s) are supplied"_warn_en_US,
        static_cast<int>(descriptors.size()), static_cast<int>(totalSlots),
        itemWord);
  }
}

//===----------------------------------------------------------------------===//
// Traversal
//===----------------------------------------------------------------------===//

template <typename ITEMS>
void FormatMismatchCheck::HandleFormat(const parser::Format &format,
                                       const ITEMS &items,
                                       const char *itemWord) {
  std::vector<IoItem> resolved;
  if (!CollectItems(items, resolved, /*insideImpliedDo=*/false))
    return;

  auto &semanticsContext{context()->getSemanticsContext()};
  auto location{semanticsContext.location()};

  common::visit(
      common::visitors{
          [&](const parser::Expr &expr) {
            auto text{
                semantics::GetConstExpr<std::string>(semanticsContext, expr)};
            if (!text)
              return; // format is only known at run time
            FormatShape shape;
            if (!ScanFormatText(*text, shape))
              return; // malformed; Flang diagnoses this itself
            Diagnose(shape, resolved, expr.source, itemWord);
          },
          [&](const parser::Label &label) {
            if (!location)
              return;
            pending_.push_back(
                {label, std::move(resolved), *location, itemWord});
          },
          [&](const parser::Star &) {}, // list-directed
      },
      format.u);
}

template <typename ITEMS>
void FormatMismatchCheck::Handle(const std::optional<parser::Format> &format,
                                 const std::list<parser::IoControlSpec> &controls,
                                 const ITEMS &items, const char *itemWord) {
  const parser::Format *resolvedFormat{format ? &*format : nullptr};
  if (!resolvedFormat) {
    for (const auto &control : controls) {
      if (const auto *f{std::get_if<parser::Format>(&control.u)}) {
        resolvedFormat = f;
        break;
      }
    }
  }
  if (!resolvedFormat)
    return; // list-directed or namelist-directed
  HandleFormat(*resolvedFormat, items, itemWord);
}

void FormatMismatchCheck::Enter(
    const parser::Statement<common::Indirection<parser::FormatStmt>> &stmt) {
  if (!stmt.label)
    return;
  const auto &spec{stmt.statement.value().v};
  if (!labeledFormats_.emplace(*stmt.label, &spec).second) {
    // The same label carries more than one format in this program unit, so it
    // cannot be attributed reliably.
    ambiguousLabels_.insert(*stmt.label);
  }
}

void FormatMismatchCheck::Leave(const parser::WriteStmt &stmt) {
  Handle(stmt.format, stmt.controls, stmt.items, "Output");
}

void FormatMismatchCheck::Leave(const parser::PrintStmt &stmt) {
  const auto &format{std::get<parser::Format>(stmt.t)};
  const auto &items{std::get<std::list<parser::OutputItem>>(stmt.t)};
  HandleFormat(format, items, "Output");
}

void FormatMismatchCheck::Leave(const parser::ReadStmt &stmt) {
  Handle(stmt.format, stmt.controls, stmt.items, "Input");
}

void FormatMismatchCheck::Leave(const parser::ProgramUnit &) {
  for (const auto &io : pending_) {
    if (ambiguousLabels_.find(io.label) != ambiguousLabels_.end())
      continue;
    auto it{labeledFormats_.find(io.label)};
    if (it == labeledFormats_.end())
      continue;
    FormatShape shape;
    if (!FlattenFormatSpec(*it->second, shape))
      continue;
    Diagnose(shape, io.items, io.at, io.itemWord);
  }
  pending_.clear();
  labeledFormats_.clear();
  ambiguousLabels_.clear();
}

} // namespace Fortran::tidy::bugprone
