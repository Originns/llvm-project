#ifndef FORTRAN_TIDY_SEMANTICS_VISITOR
#define FORTRAN_TIDY_SEMANTICS_VISITOR

#include "flang/Evaluate/expression.h"
#include "flang/Semantics/semantics.h"
#include "flang/Semantics/symbol.h"
#include "flang/Semantics/tools.h"

namespace Fortran::tidy::utils {

template <typename... C>
class SemanticsVisitor : public virtual semantics::BaseChecker,
                         public virtual C... {
public:
  using semantics::BaseChecker::Enter;
  using semantics::BaseChecker::Leave;
  using C::Enter...;
  using C::Leave...;
  SemanticsVisitor(semantics::SemanticsContext &context)
      : C{context}..., context_{context} {}

  template <typename N>
  bool Pre(const N &node) {
    if constexpr (common::HasMember<const N *, semantics::ConstructNode>) {
      context_.PushConstruct(node);
    }
    Enter(node);
    return true;
  }
  template <typename N>
  void Post(const N &node) {
    Leave(node);
    if constexpr (common::HasMember<const N *, semantics::ConstructNode>) {
      context_.PopConstruct();
    }
  }

  template <typename T>
  bool Pre(const parser::Statement<T> &node) {
    context_.set_location(node.source);
    Enter(node);
    return true;
  }
  template <typename T>
  bool Pre(const parser::UnlabeledStatement<T> &node) {
    context_.set_location(node.source);
    Enter(node);
    return true;
  }
  template <typename T>
  void Post(const parser::Statement<T> &node) {
    Leave(node);
    context_.set_location(std::nullopt);
  }
  template <typename T>
  void Post(const parser::UnlabeledStatement<T> &node) {
    Leave(node);
    context_.set_location(std::nullopt);
  }

  bool Walk(const parser::Program &program) {
    parser::Walk(program, *this);
    return !context_.AnyFatalError();
  }

private:
  semantics::SemanticsContext &context_;
};

} // namespace Fortran::tidy::utils
#endif // FORTRAN_TIDY_SEMANTICS_VISITOR