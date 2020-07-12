// Using taint analysis WITH Z3 crosscheck:
//
// REQUIRES: z3
// RUN: %clang_cc1 %S/bstring.c \
// RUN:   -verify=expected,without-taint \
// RUN:   -analyze -analyzer-constraints=range -setup-static-analyzer \
// RUN:   -analyzer-checker=core \
// RUN:   -analyzer-checker=unix.cstring \
// RUN:   -analyzer-checker=alpha.unix.cstring \
// RUN:   -analyzer-checker=debug.ExprInspection \
// RUN:   -analyzer-config eagerly-assume=false \
// RUN:   -analyzer-config crosscheck-with-z3=true

