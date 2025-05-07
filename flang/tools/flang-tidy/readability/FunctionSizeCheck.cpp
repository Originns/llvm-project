#include "FunctionSizeCheck.h"
#include "flang/Parser/parse-tree.h"

namespace Fortran::tidy::readability {

using namespace parser::literals;

// TODO: make this configurable
constexpr int LineCountThreshold = 0;
constexpr int NestingThreshold = 5;
constexpr int DummyArgThreshold = 5;

void FunctionSizeCheck::Enter(const parser::SubroutineSubprogram &program) {
  currentNestingLevel_ = -1; // start at -1 to account for the subroutine itself
  maxNestingLevel_ = 0;
  inProcedure_ = true;

  currentProcLoc_ =
      std::get<parser::Statement<parser::SubroutineStmt>>(program.t).source;

  if (DummyArgThreshold) {
    const auto &subroutineStmt =
        std::get<parser::Statement<parser::SubroutineStmt>>(program.t)
            .statement;
    const auto &dummyArgs =
        std::get<std::list<parser::DummyArg>>(subroutineStmt.t);
    if (dummyArgs.size() > DummyArgThreshold) {
      Say(currentProcLoc_,
          "%s has %d dummy arguments, which exceeds the threshold of %d"_warn_en_US,
          currentProcLoc_, dummyArgs.size(), DummyArgThreshold);
    }
  }

  if (LineCountThreshold) {
    // get the end of the subroutine
    const auto &endLoc =
        std::get<parser::Statement<parser::EndSubroutineStmt>>(program.t)
            .source;

    const auto &cookedSources =
        context()->getSemanticsContext().allCookedSources();

    // get the source position of the end location
    auto endProvenanceRange = cookedSources.GetProvenanceRange(endLoc);
    auto startProvenanceRange =
        cookedSources.GetProvenanceRange(currentProcLoc_);

    if (!endProvenanceRange || !startProvenanceRange) {
      return;
    }

    // get the source position of the end location
    auto endSourcePosition = cookedSources.allSources().GetSourcePosition(
        endProvenanceRange->start());
    auto startSourcePosition = cookedSources.allSources().GetSourcePosition(
        startProvenanceRange->start());

    if (!endSourcePosition || !startSourcePosition) {
      return;
    }

    auto lineCount = endSourcePosition->line - startSourcePosition->line;
    if (lineCount > LineCountThreshold) {
      Say(currentProcLoc_,
          "%s has a line count of %d, which exceeds the threshold of %d"_warn_en_US,
          currentProcLoc_, lineCount, LineCountThreshold);
    }
  }
}

void FunctionSizeCheck::Leave(const parser::SubroutineSubprogram &) {
  CheckNestingThreshold();
  inProcedure_ = false;
}

void FunctionSizeCheck::Enter(
    const parser::FunctionSubprogram &functionSubprogram) {
  currentNestingLevel_ = -1;
  maxNestingLevel_ = 0;
  inProcedure_ = true;

  currentProcLoc_ =
      std::get<parser::Statement<parser::FunctionStmt>>(functionSubprogram.t)
          .source;

  if (DummyArgThreshold) {
    const auto &functionStmt =
        std::get<parser::Statement<parser::FunctionStmt>>(functionSubprogram.t)
            .statement;
    const auto &args = std::get<std::list<parser::Name>>(functionStmt.t);
    if (args.size() > DummyArgThreshold) {
      Say(currentProcLoc_,
          "%s has %d dummy arguments, which exceeds the threshold of %d"_warn_en_US,
          currentProcLoc_, args.size(), DummyArgThreshold);
    }
  }

  if (LineCountThreshold) {
    // get the end of the subroutine
    const auto &endLoc = std::get<parser::Statement<parser::EndFunctionStmt>>(
                             functionSubprogram.t)
                             .source;

    const auto &cookedSources =
        context()->getSemanticsContext().allCookedSources();

    // get the source position of the end location
    auto endProvenanceRange = cookedSources.GetProvenanceRange(endLoc);
    auto startProvenanceRange =
        cookedSources.GetProvenanceRange(currentProcLoc_);

    if (!endProvenanceRange || !startProvenanceRange) {
      return;
    }

    // get the source position of the end location
    auto endSourcePosition = cookedSources.allSources().GetSourcePosition(
        endProvenanceRange->start());
    auto startSourcePosition = cookedSources.allSources().GetSourcePosition(
        startProvenanceRange->start());

    if (!endSourcePosition || !startSourcePosition) {
      return;
    }

    auto lineCount = endSourcePosition->line - startSourcePosition->line;
    if (lineCount > LineCountThreshold) {
      Say(currentProcLoc_,
          "%s has a line count of %d, which exceeds the threshold of %d"_warn_en_US,
          currentProcLoc_, lineCount, LineCountThreshold);
    }
  }
}

void FunctionSizeCheck::Leave(const parser::FunctionSubprogram &) {
  CheckNestingThreshold();
  inProcedure_ = false;
}

void FunctionSizeCheck::Enter(const parser::Block &) {
  if (inProcedure_) {
    currentNestingLevel_++;
    UpdateMaxNestingLevel();
  }
}

void FunctionSizeCheck::Leave(const parser::Block &) {
  if (inProcedure_ && currentNestingLevel_ > 0) {
    currentNestingLevel_--;
  }
}

void FunctionSizeCheck::UpdateMaxNestingLevel() {
  maxNestingLevel_ = std::max(maxNestingLevel_, currentNestingLevel_);
}

void FunctionSizeCheck::CheckNestingThreshold() {
  if (maxNestingLevel_ > NestingThreshold) {
    Say(currentProcLoc_,
        "%s has a nesting level of %d, which exceeds the threshold of %d"_warn_en_US,
        currentProcLoc_, maxNestingLevel_, NestingThreshold);
  }
}

} // namespace Fortran::tidy::readability
