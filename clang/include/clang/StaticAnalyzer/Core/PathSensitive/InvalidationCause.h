#ifndef LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_INVALIDATION_CAUSE_H
#define LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_INVALIDATION_CAUSE_H

#include "clang/StaticAnalyzer/Core/PathSensitive/SymExpr.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SymbolManager.h"

namespace clang {
namespace ento {

class InvalidationCause : public llvm::FoldingSetNode {
  virtual void anchor();
public:
  virtual ~InvalidationCause() = default;

  enum Kind {
#define INVALIDATION_CAUSE(Id, Parent) Id##Kind,
#define ABSTRACT_INVALIDATION_CAUSE(Id, Parent) Id##Kind,
#include "clang/StaticAnalyzer/Core/PathSensitive/InvalidationCause.def"
  };
    
    Kind getKind() const { return K; }
    virtual void dump() const;
    virtual void dump(raw_ostream &OS) const;

  protected:
    explicit InvalidationCause(Kind K) : K(K) {}

  private:
    Kind K;
};

inline raw_ostream &operator<<(raw_ostream &OS,
                               const InvalidationCause *Invalidation) {
  Invalidation->dump(OS);
  return OS;
}


} // namespace ento
} // namespace clang

#endif // LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_INVALIDATION_CAUSE_H
