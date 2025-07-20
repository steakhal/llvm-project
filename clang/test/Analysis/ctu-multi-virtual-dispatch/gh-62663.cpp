// RUN: rm -rf %t && mkdir %t
// RUN: split-file %s %t

// RUN: %analyzer-emit-ast -o "%t/ParentRecvCancel.ast" "%t/ParentRecvCancel.cpp"
// RUN: %analyzer-emit-ast -o "%t/Handler.ast" "%t/Handler.cpp"

// RUN: cd "%t" && %clang_extdef_map "%t/ParentRecvCancel.ast" >> externalDefMap.txt
// RUN: cd "%t" && %clang_extdef_map "%t/Handler.ast" >> externalDefMap.txt
// RUN: %direct-overriders-merger.py %t %t/direct-overriders.txt

// RUN: %clang_analyze_cc1 -I %t \
// RUN:   -analyzer-checker=core,optin.taint,debug.ExprInspection \
// RUN:   -analyzer-config experimental-enable-naive-ctu-analysis=true \
// RUN:   -analyzer-config ctu-dir=%t \
// RUN:   -analyzer-config display-ctu-progress=true \
// RUN:   -analyzer-config direct-overriders-file=%t/direct-overriders.txt \
// RUN:   -verify %t/Parent.cpp

/*
  Class Hierarchy:

        Actor◄───┐
                 │---Parent
        PParent◄─┘

        Base ◄── Handler

  Control Flow:

    start: PParent::OnMessageReceived
           Parent::RecvCancel
           Handler::OnRecvCancel
*/

//--- CSA.h
#ifndef CSA_H
#define CSA_H
template <class T> void clang_analyzer_isTainted(T) {}
#endif // CSA_H

//--- Handler.h
#ifndef HANDLER_H
#define HANDLER_H
class Base {
public:
  virtual ~Base() = default;
  virtual void OnRecvCancel(int port) = 0;
};
class Handler final : public Base {
 public:
  void OnRecvCancel(int port) override;
};
#endif // HANDLER_H

//--- Handler.cpp
#include "Handler.h"
#include "CSA.h"

void Handler::OnRecvCancel(int port) {
  clang_analyzer_isTainted(port); // Handler::OnRecvCancel is tainted(port) at the beginning
}

//--- Parent.h
#ifndef PARENT_H
#define PARENT_H
class Base;
class PParent {
public:
  bool OnMessageReceived() ;
};
class Actor {
public:
  explicit Actor(Base* aRequest) : m(aRequest) {}

protected:
  Base* m;
};
class Parent : public Actor, public PParent {
public:
  explicit Parent(Base* aRequest);

  bool RecvCancel(int port);
};
#endif // PARENT_H

//--- ParentRecvCancel.cpp
#include "Parent.h"
#include "Handler.h"
#include "CSA.h"

Parent::Parent(Base* aRequest) : Actor(aRequest) {}

bool Parent::RecvCancel(int port) {
  clang_analyzer_isTainted(port); // Paren::RecvCancel is tainted(port) at the beginning

  ((Handler*)m)->OnRecvCancel(port);
  return true;
}

//--- Parent.cpp
#include "Parent.h"
#include "CSA.h"

int scanf(const char *format, ...);

Parent *getParent();

bool PParent::OnMessageReceived() {
  int port;
  scanf("%i", &port);

  // Not needed, but yes it is Tainted.
  clang_analyzer_isTainted(port); // expected-warning {{YES}}

  return static_cast<Parent*>(this)->RecvCancel(port);
  // expected-warning@ParentRecvCancel.cpp:8 {{YES}} aka. Paren::RecvCancel is tainted(port) at the beginning
  // expected-warning@Handler.cpp:5 {{YES}} aka. Handler::OnRecvCancel is tainted(port) at the beginning
}
