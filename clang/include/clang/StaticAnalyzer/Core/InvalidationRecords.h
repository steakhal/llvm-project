#ifndef FOOBARBBBBBBBAA
#define FOOBARBBBBBBBAA

#include "llvm/Support/Casting.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/ImmutableSet.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/MemRegion.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramStateTrait.h"

namespace clang {
  namespace ento {

class InvalidationEvent : llvm::FoldingSetNode {
public:
  using RegionSet = llvm::ImmutableSet<const MemRegion *>;
  using iterator = RegionSet::iterator;

  InvalidationEvent(RegionSet::Factory &F,
                    ArrayRef<const MemRegion *> ExplicitRegions,
                    ArrayRef<const MemRegion *> Regions) {
    Direct = F.getEmptySet();
    Indirect = F.getEmptySet();

    for (const MemRegion *R : ExplicitRegions)
      Direct = F.add(Direct, R);

    for (const MemRegion *R : Regions)
      if (isa<SubRegion>(R) && !llvm::is_contained(ExplicitRegions, R))
        Indirect = F.add(Indirect, R);
  }

  iterator direct_begin() const { return Direct.begin(); }
  iterator direct_end() const { return Direct.end(); }
  iterator indirect_begin() const { return Indirect.begin(); }
  iterator indirect_end() const { return Indirect.end(); }

  void dump() const {
    if (!Direct.isEmpty()) {
      llvm::errs() << "Directly invalidated regions:\n";
      for (const MemRegion *R : Direct) {
        llvm::errs() << "  " << R << "\n";
      }
    }
    if (!Indirect.isEmpty()) {
      llvm::errs() << "Indirectly invalidated regions:\n";
      for (const MemRegion *R : Indirect) {
        llvm::errs() << "  " << R << "\n";
      }
    }
  }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    for (const MemRegion *R : Direct) {
      ID.AddPointer(R);
    }
    for (const MemRegion *R : Indirect) {
      ID.AddPointer(R);
    }
  }

  bool operator==(const InvalidationEvent &X) const {
    return Direct == X.Direct && Indirect == X.Indirect;
  }

  bool operator<(const InvalidationEvent &X) const {
    auto lhs = Direct.getHeight();
    auto rhs = X.Direct.getHeight();
    if (lhs < rhs)
      return true;
    if (lhs > rhs)
      return false;

    lhs = Indirect.getHeight();
    rhs = X.Indirect.getHeight();
    if (lhs < rhs)
      return true;
    if (lhs > rhs)
      return false;
    {
      auto it1 = Direct.begin();
      auto it2 = X.Direct.begin();
      auto it1End = Direct.end();
      for (; it1 != it1End; ++it1, ++it2) {
        if (*it1 < *it2)
          return true;
        if (*it1 > *it2)
          return false;
      }
    }
    {
      auto it1 = Indirect.begin();
      auto it2 = X.Indirect.begin();
      auto it1End = Indirect.end();
      for (; it1 != it1End; ++it1, ++it2) {
        if (*it1 < *it2)
          return true;
        if (*it1 > *it2)
          return false;
      }
    }
    return false;
  }

private:
  RegionSet Direct = RegionSet{nullptr};
  RegionSet Indirect = RegionSet{nullptr};
};
  }
}

REGISTER_SET_FACTORY_WITH_PROGRAMSTATE(InvalidationRecords, clang::ento::InvalidationEvent)
REGISTER_FACTORY_WITH_PROGRAMSTATE(clang::ento::InvalidationEvent::RegionSet)


#endif
