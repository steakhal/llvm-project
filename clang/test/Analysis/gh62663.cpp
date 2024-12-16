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
    // SECOND CALL to check taint
    clang_analyzer_isTainted(port);
  }
};

struct AlternativeHandler final : Base {
  void OnRecvCancel(int port) override {
    // SECOND CALL to check taint
    clang_analyzer_isTainted(port);
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

    // Statement 2A: With this statement (and 1A) things behave as we expect.  We report that
    //               port is tainted on both the first and second calls.
    // Handler* foo = (Handler*)m;

    // Statement 2B: With this statement (and 1A) we only report that port is tainted on the first call
    auto foo = m;

    foo->OnRecvCancel(port);
    return true;
  }
};

// Entry point, calling `Parent::RecvCancel()` which is then calling `Handler::OnRecvCancel()`.
// Taint should propagate through the `port` variables.
auto PParent::OnMessageReceived() -> bool {
    int port;
    scanf("%i", &port);

    // Not needed, but yes it is Tainted.
    //clang_analyzer_isTainted(port);

    // Statement 1A: With this value (and statement 2A) we correctly report that port is tainted on
    //               the first and second calls
    Parent* foo = conjure<Parent *>();

    // Statement 1B: With this value (and statement 2A) we report that port is tainted only on the
    //               first call.  Parent inherits from PParent.
    //Parent* foo = static_cast<Parent*>(this);

    // Statement 1C: With this value (and statement 2A) we do not report that port is tainted at all
    //               I presume this is do to optimization as we're causing Undefined Behavior
    //Parent* foo = nullptr;

    return foo->RecvCancel(port);
}
