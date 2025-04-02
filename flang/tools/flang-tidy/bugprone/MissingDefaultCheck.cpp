#include "MissingDefaultCheck.h"
#include "flang/Parser/parse-tree.h"

#include <algorithm>

namespace Fortran::tidy::bugprone {

using namespace parser::literals;

// MissingDefaultCheck::MissingDefaultCheck(llvm::StringRef name,
//                                          FlangTidyContext *context)
//     : FlangTidyCheck{name}, context(){context} {}

void MissingDefaultCheck::Enter(const parser::CaseConstruct &caseConstruct) {
  const auto &source =
      std::get<parser::Statement<parser::SelectCaseStmt>>(caseConstruct.t)
          .source;

  const auto &cases =
      std::get<std::list<parser::CaseConstruct::Case>>(caseConstruct.t);

  bool hasDefault = std::any_of(cases.begin(), cases.end(), [](const auto &c) {
    const auto &caseSelector = std::get<parser::CaseSelector>(
        std::get<parser::Statement<parser::CaseStmt>>(c.t).statement.t);

    return std::holds_alternative<parser::Default>(caseSelector.u);
  });

  if (!hasDefault) {
    Say(source, "SELECT CASE construct has no DEFAULT case"_warn_en_US);
  }
}

} // namespace Fortran::tidy::bugprone
