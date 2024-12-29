// RUN: %clang_analyze_cc1 -analyzer-checker=core,optin.taint,debug.ExprInspection,debug.TaintTest -verify %s

extern "C" int scanf(const char *format, ...);

template <class T> T conjure();
void clang_analyzer_isTainted(int);

struct Base {
  virtual ~Base() = default;
  virtual void OnRecvCancel(int port) = 0;
};

struct Handler final : Base {
  void OnRecvCancel(int port) override {
    clang_analyzer_isTainted(port);
    // expected-warning@-1 {{YES}}
    // expected-warning@-2 {{tainted}}
  }
};

struct AlternativeHandler final : Base {
  void OnRecvCancel(int port) override {
    clang_analyzer_isTainted(port);
    // expected-warning@-1 {{YES}}
    // expected-warning@-2 {{tainted}}
  }
};

struct PParent {
  bool OnMessageReceived();
};

struct Actor {
  explicit Actor(Base* aRequest) : m(aRequest) {}
  Base* m;
};

struct Parent : Actor, PParent {
  explicit Parent(Base *aRequest) : Actor(aRequest) {}
  bool RecvCancel(int port) {
    // FIRST CALL to check taint
    clang_analyzer_isTainted(port);
    // expected-warning@-1 {{YES}}
    // expected-warning@-2 {{tainted}}

    m->OnRecvCancel(port); // expected-warning {{tainted}}
    return true;
  }
};

// Entry point, calling `Parent::RecvCancel()` which is then calling `Handler::OnRecvCancel()`.
// Taint should propagate through the `port` variables.
auto PParent::OnMessageReceived() -> bool {
    int port;
    scanf("%i", &port);
    clang_analyzer_isTainted(port);
    // expected-warning@-1 {{YES}}
    // expected-warning@-2 {{tainted}}

    Parent* foo = conjure<Parent *>();
    return foo->RecvCancel(port); // expected-warning {{tainted}}
}
