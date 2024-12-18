#ifndef FORTRAN_TIDY_FLANGTIDYMODULE_H
#define FORTRAN_TIDY_FLANGTIDYMODULE_H

#include "FlangTidyOptions.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include <functional>
#include <memory>

#include "llvm/Support/raw_ostream.h"

namespace Fortran::tidy {

class FlangTidyCheck;
class FlangTidyContext;

/// A collection of \c TidyCheckFactory instances.
///
/// All clang-tidy modules register their check factories with an instance of
/// this object.
class FlangTidyCheckFactories {
public:
  using CheckFactory = std::function<std::unique_ptr<FlangTidyCheck>(
      llvm::StringRef Name, FlangTidyContext *Context)>;

  /// Registers check \p Factory with name \p Name.
  ///
  /// For all checks that have default constructors, use \c registerCheck.
  void registerCheckFactory(llvm::StringRef Name, CheckFactory Factory);

  /// Registers the \c CheckType with the name \p Name.
  ///
  /// This method should be used for all \c ClangTidyChecks that don't require
  /// constructor parameters.
  ///
  /// For example, if have a clang-tidy check like:
  /// \code
  /// class MyTidyCheck : public ClangTidyCheck {
  ///   void registerMatchers(ast_matchers::MatchFinder *Finder) override {
  ///     ..
  ///   }
  /// };
  /// \endcode
  /// you can register it with:
  /// \code
  /// class MyModule : public ClangTidyModule {
  ///   void addCheckFactories(ClangTidyCheckFactories &Factories) override {
  ///     Factories.registerCheck<MyTidyCheck>("myproject-my-check");
  ///   }
  /// };
  /// \endcode
  template <typename CheckType>
  void registerCheck(llvm::StringRef CheckName) {
    registerCheckFactory(CheckName,
                         [](llvm::StringRef Name, FlangTidyContext *Context) {
                           return std::make_unique<CheckType>(Name, Context);
                         });
  }

  /// Create instances of checks that are enabled.
  std::vector<std::unique_ptr<FlangTidyCheck>>
  createChecks(FlangTidyContext *Context) const;

  /// Create instances of checks that are enabled for the current Language.
  std::vector<std::unique_ptr<FlangTidyCheck>>
  createChecksForLanguage(FlangTidyContext *Context) const;

  using FactoryMap = llvm::StringMap<CheckFactory>;
  FactoryMap::const_iterator begin() const { return Factories.begin(); }
  FactoryMap::const_iterator end() const { return Factories.end(); }
  bool empty() const { return Factories.empty(); }

private:
  FactoryMap Factories;
};

/// A clang-tidy module groups a number of \c ClangTidyChecks and gives
/// them a prefixed name.
class FlangTidyModule {
public:
  virtual ~FlangTidyModule() {}

  /// Implement this function in order to register all \c CheckFactories
  /// belonging to this module.
  virtual void addCheckFactories(FlangTidyCheckFactories &CheckFactories) = 0;

  /// Gets default options for checks defined in this module.
  virtual FlangTidyOptions getModuleOptions();
};

} // namespace Fortran::tidy

#endif // FORTRAN_TIDY_FLANGTIDYMODULE_H
