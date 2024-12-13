//===--- tools/extra/clang-tidy/ClangTidyModule.cpp - Clang tidy tool -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
///  \file Implements classes required to build clang-tidy modules.
///
//===----------------------------------------------------------------------===//

#include "FlangTidyModule.h"
#include "FlangTidyCheck.h"
#include "FlangTidyContext.h"
#include <cstdint>

namespace Fortran::tidy {

void FlangTidyCheckFactories::registerCheckFactory(llvm::StringRef Name,
                                                   CheckFactory Factory) {
  Factories.insert_or_assign(Name, std::move(Factory));
}

std::vector<std::unique_ptr<FlangTidyCheck>>
FlangTidyCheckFactories::createChecks(FlangTidyContext *Context) const {
  std::vector<std::unique_ptr<FlangTidyCheck>> Checks;
  llvm::outs() << "Registered " << Factories.size() << " checks\n";
  for (const auto &Factory : Factories) {
    llvm::outs() << "Comparing check: " << Factory.getKey() << "\n";
    if (Context->isCheckEnabled(Factory.getKey())) {
      llvm::outs() << "Adding check: " << Factory.getKey() << "\n";
      Checks.emplace_back(Factory.getValue()(Factory.getKey(), Context));
    }
  }
  return Checks;
}

std::vector<std::unique_ptr<FlangTidyCheck>>
FlangTidyCheckFactories::createChecksForLanguage(
    FlangTidyContext *Context) const {
  std::vector<std::unique_ptr<FlangTidyCheck>> Checks;
  // const LangOptions &LO = Context->getLangOpts();
  for (const auto &Factory : Factories) {
    if (!Context->isCheckEnabled(Factory.getKey()))
      continue;
    std::unique_ptr<FlangTidyCheck> Check =
        Factory.getValue()(Factory.getKey(), Context);
    // if (Check->isLanguageVersionSupported(LO))
    //   Checks.push_back(std::move(Check));
  }
  return Checks;
}

FlangTidyOptions FlangTidyModule::getModuleOptions() { return {}; }

} // namespace Fortran::tidy
