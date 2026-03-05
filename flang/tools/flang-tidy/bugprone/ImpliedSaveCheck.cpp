//===--- ImpliedSaveCheck.cpp - flang-tidy --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ImpliedSaveCheck.h"
#include "flang/Evaluate/tools.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/attr.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"
#include "flang/Semantics/type.h"
#include "utils/FixIt.h"
#include "utils/SourceEditUtils.h"
#include <vector>

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

void ImpliedSaveCheck::Enter(const parser::TypeDeclarationStmt &typeDecl) {
  const auto &entities = std::get<std::list<parser::EntityDecl>>(typeDecl.t);
  std::vector<const semantics::Symbol *> symbolsNeedingSaveFix;
  bool canEmitDeclAttrFix{!entities.empty()};

  for (const auto &entityDecl : entities) {
    const auto &objectName = std::get<parser::ObjectName>(entityDecl.t);
    const auto *symbol = objectName.symbol;
    if (!symbol) {
      canEmitDeclAttrFix = false;
      continue;
    }

    const auto &scope{symbol->owner()};
    const bool needsSave = semantics::IsSaved(*symbol) &&
                           scope.kind() != semantics::Scope::Kind::Module &&
                           !symbol->attrs().test(semantics::Attr::SAVE);
    const bool canFixHere = needsSave && !scope.hasSAVE();

    if (canFixHere) {
      symbolsNeedingSaveFix.push_back(symbol);
    } else {
      canEmitDeclAttrFix = false;
    }

    if (!needsSave) {
      continue;
    }

    if (semantics::IsSaved(*symbol) &&
        scope.kind() != semantics::Scope::Kind::Module &&
        !symbol->attrs().test(semantics::Attr::SAVE)) {
      // std::get<2>(newTypeDeclStmt.t).push_back(entityDecl);

      if (scope.hasSAVE()) {
        const auto name = scope.GetName();
        if (name) {
          Say(symbol->name(),
              "Symbol '%s' is implicitly saved in scope '%s', but does not "
              "have "
              "the SAVE attribute"_warn_en_US,
              symbol->name(), name->ToString());
        } else {
          Say(symbol->name(),
              "Symbol '%s' is implicitly saved in enclosing scope, but does "
              "not "
              "have the SAVE attribute"_warn_en_US,
              symbol->name());
        }
      } else {
        Say(symbol->name(), "Implicit SAVE on symbol '%s'"_warn_en_US,
            symbol->name());
      }
    }
  }

  if (symbolsNeedingSaveFix.empty()) {
    return;
  }

  if (const auto source{context()->getSemanticsContext().location()}) {
    const std::string sourceText{source->ToString()};
    const std::string saveKeyword{
        utils::inferKeywordSpelling(sourceText, "SAVE")};
    const char *insertPos{nullptr};
    std::string fixText;

    if (canEmitDeclAttrFix) {
      if (auto pos =
              utils::findDeclAttrInsertionPoint(sourceText, source->begin())) {
        insertPos = *pos;
        fixText = ", " + saveKeyword;
      }
    } else {
      // Mixed declarations: add a dedicated SAVE statement for only the
      // implicitly-saved subset.
      std::string indent{utils::getLeadingWhitespaceFromSourceLine(
          context()->getSemanticsContext(), *source)};
      if (indent.empty()) {
        indent = utils::getLeadingWhitespace(sourceText);
      }
      std::string separator{utils::getDoubleColonSeparator(sourceText)};

      std::string names;
      for (std::size_t i{0}; i < symbolsNeedingSaveFix.size(); ++i) {
        if (i > 0) {
          names += ", ";
        }
        names += symbolsNeedingSaveFix[i]->name().ToString();
      }

      insertPos = source->end();
      fixText = "\n" + indent + saveKeyword + separator + names;
    }

    if (insertPos && !fixText.empty()) {
      Fortran::tidy::FixItHint fixItHint =
          Fortran::tidy::FixItHint::CreateInsertion(
              parser::CharBlock{insertPos, insertPos}, fixText);
      context()->addFixIt(name(), symbolsNeedingSaveFix.front()->name(),
                          fixItHint);
    }
  }
}

} // namespace Fortran::tidy::bugprone
