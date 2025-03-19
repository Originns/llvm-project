#include "MissingActionCheck.h"
#include "flang/Evaluate/check-expression.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"

#include <algorithm>

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

MissingActionCheck::MissingActionCheck(llvm::StringRef name,
                                       FlangTidyContext *context)
    : FlangTidyCheck{name}, context_{context} {}

/*
// R1201 io-unit -> file-unit-number | * | internal-file-variable
// R1203 internal-file-variable -> char-variable
// R905 char-variable -> variable
// When Variable appears as an IoUnit, it must be character of a default,
// ASCII, or Unicode kind; this constraint is not automatically checked.
// The parse is ambiguous and is repaired if necessary once the types of
// symbols are known.
struct IoUnit {
  UNION_CLASS_BOILERPLATE(IoUnit);
  std::variant<Variable, FileUnitNumber, Star> u;
};

// R1206 file-name-expr -> scalar-default-char-expr
using FileNameExpr = ScalarDefaultCharExpr;

// R1205 connect-spec ->
//         [UNIT =] file-unit-number | ACCESS = scalar-default-char-expr |
//         ACTION = scalar-default-char-expr |
//         ASYNCHRONOUS = scalar-default-char-expr |
//         BLANK = scalar-default-char-expr |
//         DECIMAL = scalar-default-char-expr |
//         DELIM = scalar-default-char-expr |
//         ENCODING = scalar-default-char-expr | ERR = label |
//         FILE = file-name-expr | FORM = scalar-default-char-expr |
//         IOMSG = iomsg-variable | IOSTAT = scalar-int-variable |
//         NEWUNIT = scalar-int-variable | PAD = scalar-default-char-expr |
//         POSITION = scalar-default-char-expr | RECL = scalar-int-expr |
//         ROUND = scalar-default-char-expr | SIGN = scalar-default-char-expr |
//         STATUS = scalar-default-char-expr
//         @ | CARRIAGECONTROL = scalar-default-char-variable
//           | CONVERT = scalar-default-char-variable
//           | DISPOSE = scalar-default-char-variable
WRAPPER_CLASS(StatusExpr, ScalarDefaultCharExpr);
WRAPPER_CLASS(ErrLabel, Label);

struct ConnectSpec {
  UNION_CLASS_BOILERPLATE(ConnectSpec);
  struct CharExpr {
    ENUM_CLASS(Kind, Access, Action, Asynchronous, Blank, Decimal, Delim,
        Encoding, Form, Pad, Position, Round, Sign,
         extensions:  Carriagecontrol, Convert, Dispose)
    TUPLE_CLASS_BOILERPLATE(CharExpr);
    std::tuple<Kind, ScalarDefaultCharExpr> t;
  };
  WRAPPER_CLASS(Recl, ScalarIntExpr);
  WRAPPER_CLASS(Newunit, ScalarIntVariable);
  std::variant<FileUnitNumber, FileNameExpr, CharExpr, MsgVariable,
      StatVariable, Recl, Newunit, ErrLabel, StatusExpr>
      u;
};

*/
// TODO: magic check for all I/O specifiers

void MissingActionCheck::Leave(const parser::OpenStmt &openStmt) {
  const auto &source = context_->getSemanticsContext().location().value();

  const auto &connectSpec = openStmt.v;

  // first, check if the file unit number is a constant literal
  const auto &variant = std::find_if(
      connectSpec.begin(), connectSpec.end(), [](const auto &spec) {
        return std::holds_alternative<parser::FileUnitNumber>(spec.u);
      });

  if (variant != connectSpec.end()) {
    const auto &fileUnitNumber = std::get<parser::FileUnitNumber>(variant->u);

    const auto *expr =
        semantics::GetExpr(context_->getSemanticsContext(), fileUnitNumber.v);

    if (expr && evaluate::IsConstantExpr(*expr)) {
      // warn about it
      context_->getSemanticsContext().Say(
          source, "File unit number is a constant literal"_warn_en_US);
    }
  }

  // get all char expr from the connect spec
  std::vector<const parser::ConnectSpec::CharExpr *> charExprs;
  for (const auto &spec : connectSpec) {
    if (std::holds_alternative<parser::ConnectSpec::CharExpr>(spec.u)) {
      const auto &charExpr = std::get<parser::ConnectSpec::CharExpr>(spec.u);
      charExprs.push_back(&charExpr);
    }
  }

  // check if any of the char exprs are ACTION
  if (std::any_of(charExprs.begin(), charExprs.end(), [](const auto &expr) {
        return std::get<parser::ConnectSpec::CharExpr::Kind>(expr->t) ==
               parser::ConnectSpec::CharExpr::Kind::Action;
      })) {
    return;
  }

  context_->getSemanticsContext().Say(source,
                                      "ACTION specifier is missing"_warn_en_US);
}

} // namespace Fortran::tidy::bugprone
