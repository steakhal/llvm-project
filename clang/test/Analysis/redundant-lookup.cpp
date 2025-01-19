// RUN: %clang_analyze_cc1 -std=c++17 -verify %s \
// RUN:   -analyzer-checker=core,alpha.cplusplus.RedundantLookupChecker \
// RUN:   -analyzer-output text

#include "Inputs/system-header-simulator-cxx.h"

template <class T> void escape(T);
void opaque_call();

namespace c {
template <class T, class U> class MyFancyMap {
public:
  bool contains(T) const;
  int count(T) const;
  U &operator[](T);
  const U &operator[](T) const;

  void likelyMutate();
  void nonMutating() const;
  void nonMutating();

private:
  T* keys;
  U* values;
};

template <class T> class MyFancySet {
public:
  bool contains(T) const;
  int count(T) const;

  void likelyMutate();
  void nonMutating() const;
  void nonMutating();

private:
  T* keys;
};

} // namespace c

void containerNameSpellsSet(c::MyFancySet<int> &s, int key) {
  (void)s.contains(key); // expected-note {{Previous lookup with the same value was here}}
  (void)s.contains(key);
  // expected-warning@-1 {{Redundant lookup with the same value}}
  // expected-note@-2    {{Redundant lookup with the same value}}
}

void containerNameSpellsMap(c::MyFancyMap<int, int> &m, int key) {
  (void)m.contains(key); // expected-note {{Previous lookup with the same value was here}}
  (void)m.contains(key);
  // expected-warning@-1 {{Redundant lookup with the same value}}
  // expected-note@-2    {{Redundant lookup with the same value}}
}

void stdSetAlsoWorks(std::set<int> &m, int key) {
  (void)m.count(key); // expected-note {{Previous lookup with the same value was here}}
  (void)m.count(key);
  // expected-warning@-1 {{Redundant lookup with the same value}}
  // expected-note@-2    {{Redundant lookup with the same value}}
}

// FIXME: Add std::map to the "Inputs/system-header-simulator-cxx.h" and test that too.

void differentLookupKeys(c::MyFancyMap<int, int> &m, int key, int val) {
  (void)m.contains(key);
  m[val] = key; // no-warning: The second lookup uses a different key.
}

void differentContainers(c::MyFancyMap<int, int> &m, c::MyFancyMap<int, int> &n, int key, int val) {
  (void)m.contains(key);
  n[key] = val; // no-warning: The second lookup is on a different container.
}

void countThenContains(c::MyFancyMap<int, int> &m, int key, int val) {
  (void)m.count(key); // expected-note {{Previous lookup with the same value was here}}
  (void)m.contains(key);
  // expected-warning@-1 {{Redundant lookup with the same value}}
  // expected-note@-2    {{Redundant lookup with the same value}}
}

void containsThanContains(c::MyFancyMap<int, int> &m, int key, int val) {
  (void)m.contains(key); // expected-note {{Previous lookup with the same value was here}}
  (void)m.count(key);
  // expected-warning@-1 {{Redundant lookup with the same value}}
  // expected-note@-2    {{Redundant lookup with the same value}}
}

void subscriptThenContains(c::MyFancyMap<int, int> &m, int key, int val) {
  (void)m[key];
  (void)m.contains(key);// FIXME: We should catch this.
}

void subscriptThenContainsOnConst(c::MyFancyMap<int, int> &m, int key, int val) {
  const auto &cref = m;
  (void)cref[key]; // expected-note {{Previous lookup with the same value was here}}
  (void)m.contains(key);
  // expected-warning@-1 {{Redundant lookup with the same value}}
  // expected-note@-2    {{Redundant lookup with the same value}}
}

void doubleLookupWhileHavingIgnorableMutationsInbetween(c::MyFancyMap<int, int> &m, int key, int val) {
  (void)m.contains(key); // expected-note {{Previous lookup with the same value was here}}
  m.nonMutating(); // This unlikely that actually mutates the container.
  m[key] = val;
  // expected-warning@-1 {{Redundant lookup with the same value}}
  // expected-note@-2    {{Redundant lookup with the same value}}
}

void potentiallyMutateInbetween(c::MyFancyMap<int, int> &m, int key, int val) {
  (void)m.contains(key);
  m.likelyMutate();
  m[key] = val; // no-warning: "potentiallyMutating" may have mutated the container inbetween the two lookups.
}

void directEscapeBetweenLookups(c::MyFancyMap<int, int> &m, int key, int val) {
  (void)m.contains(key);
  escape(&m);
  m[key] = val; // no-warning: "escape" may have mutated the container inbetween the two lookups.
}

template <class T> struct PtrWrapper {
  PtrWrapper(T *p) : p(p) {}
  T *p;
};

void inirectEscapeBetweenLookups(c::MyFancyMap<int, int> &m, int key, int val) {
  (void)m.contains(key);
  escape(PtrWrapper(&m));
  m[key] = val; // no-warning: "escape" may have mutated the container inbetween the two lookups by chasing the pointer.
}

void escapeToGlobalAndOpaqueCallAfterLookup(c::MyFancyMap<int, int> &m, int key, int val) {
  (void)m.contains(key);

  extern void *global;
  global = &m;
  opaque_call(); // This should invalidate everything we know of "m".
  m[key] = val; // no-warning: "opaque_call" may have mutated the container inbetween the two lookups.
}

void escapeToGlobalBeforeLookup(c::MyFancyMap<int, int> &m, int key, int val) {
  extern void *global;
  global = &m;
  // We don't escape "m" just yet. That only truly escapes when we lose track of the globals,
  // aka. only when some conservative eval call happens.

  (void)m.contains(key); // expected-note {{Previous lookup with the same value was here}}
  // First, the "contains" call wipes the globals and what is reachable from those, due to the conservative eval call.
  // This means that "glob" will no longer refer to "m".
  // After this, we set the last lookup location on the "m" container to this one.

  opaque_call();
  // We should know here that "glob" could/should have referred to "m", thus also consider "m" escaped in this call.

  m[key] = val;
  // expected-warning@-1 {{Redundant lookup with the same value}}
  // expected-note@-2    {{Redundant lookup with the same value}}
}

void escapeByGlobalBeforeLookup(c::MyFancyMap<int, int> &m, int key, int val) {
  // If a container ever escapes by an opaque call other than the member function of the container, then
  // opaque calls will become poison for the escaped container.
  // This should mean that all opaque calls that happen inbetween the two subsequent "lookups",
  // we should conservatively assume sideffects.

  extern void *global;
  global = &m;
  opaque_call(); // "m" escaped here.

  (void)m.contains(key); // expected-note {{Previous lookup with the same value was here}}
  // No opaque calls between the "contains" and the "[key]" calls.
  m[key] = val;
  // expected-warning@-1 {{Redundant lookup with the same value}}
  // expected-note@-2    {{Redundant lookup with the same value}}
}
