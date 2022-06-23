// RUN: %check_clang_tidy -std=c++17-or-later %s cert-dcl58-cpp %t -- -- -I %clang_tidy_headers

#include "system-header-simulation.h"

namespace A {
  namespace B {
    int b;
  }
}

namespace A {
  namespace B {
    int c;
  }
}

namespace posix {
// CHECK-MESSAGES: :[[@LINE+2]]:11: warning: modification of 'posix' namespace can result in undefined behavior [cert-dcl58-cpp]
// CHECK-MESSAGES: :[[@LINE-2]]:11: note: 'posix' namespace opened here
namespace vmi {
int foobar;
}
}

namespace std {
// CHECK-MESSAGES: :[[@LINE+2]]:5: warning: modification of 'std' namespace
// CHECK-MESSAGES: :[[@LINE-2]]:11: note: 'std' namespace opened here
int stdInt;
}

namespace foobar {
  namespace std {
    int bar;
  }
}

namespace posix::a {
// CHECK-MESSAGES: :[[@LINE-1]]:18: warning: modification of 'posix' namespace
// CHECK-MESSAGES: :[[@LINE-2]]:11: note: 'posix' namespace opened here
}

enum class MyError {
  ErrorA,
  ErrorB
};

namespace std {
// no-warning: Class template specialized by a program-defined type.
template <>
struct is_error_code_enum<MyError> : std::true_type {};

// no-warning: Function template specialized by a program-defined type.
template<>
void swap<MyError>(MyError &a, MyError &b);
}

using namespace std;
int x;

namespace std {
// Forbid specializations over `std::` types.
// CHECK-MESSAGES: :[[@LINE+3]]:8: warning: modification of 'std' namespace
// CHECK-MESSAGES: :[[@LINE-3]]:11: note: 'std' namespace opened here
template <>
struct is_error_code_enum<std::io_errc> : std::true_type {};
} // namespace std

namespace std {
// Forbid specializations over builtin types.
// CHECK-MESSAGES: :[[@LINE+3]]:8: warning: modification of 'std' namespace
// CHECK-MESSAGES: :[[@LINE-3]]:11: note: 'std' namespace opened here
template <>
struct is_error_code_enum<int> : std::true_type {};
} // namespace std

// Test parameter packs.
namespace std {
// Forbid variadic specializations over only `std::` or builtin types.
// CHECK-MESSAGES: :[[@LINE+3]]:7: warning: modification of 'std' namespace
// CHECK-MESSAGES: :[[@LINE-3]]:11: note: 'std' namespace opened here
template <>
class tuple<int, std::io_errc, float> {};
} // namespace std

namespace std {
// no-warning: The specialization refers to at least one program-defined type.
template <>
class tuple<int, std::io_errc, float, MyError> {};
} // namespace std

namespace std {
// no-warning: empty
} // namespace std

namespace std {
// Warn for non-NamedDecls as well.
// CHECK-MESSAGES: :[[@LINE+2]]:1: warning: modification of 'std' namespace
// CHECK-MESSAGES: :[[@LINE-3]]:11: note: 'std' namespace opened here
static_assert(1 == 1, "non-NamedDecl");
} // namespace std

// Test member template specializations.
namespace std {
// CHECK-MESSAGES: :[[@LINE+3]]:18: warning: modification of 'std' namespace
// CHECK-MESSAGES: :[[@LINE-2]]:11: note: 'std' namespace opened here
template <>
bool less<void>::operator()<int &&, float &&>(int &&, float &&) const {
  return true;
}
} // namespace std

// We did not open the 'std' namespace, but still specialized the member
// function of 'std::less'.
// CHECK-MESSAGES: :[[@LINE+3]]:23: warning: modification of 'std' namespace
// no-note: There is no opening of 'std' namespace, hence no note emitted.
template <>
bool std::less<void>::operator()<int &&, int &&>(int &&, int &&) const {
  return true;
}
namespace SpaceA {
namespace SpaceB {
class MapKey {
  int Type = 0;

public:
  MapKey() = default;
  int getType() const { return Type; }
};
} // namespace SpaceB
} // namespace SpaceA

// no-warning: Specializing for 'std::hash' for a program-defined type.
template <>
struct std::hash<::SpaceA::SpaceB::MapKey> {
  // no-warning
  unsigned long operator()(const ::SpaceA::SpaceB::MapKey &K) const {
    return K.getType();
  }
  // no-warning
  bool operator()(const ::SpaceA::SpaceB::MapKey &K1,
                  const ::SpaceA::SpaceB::MapKey &K2) const {
    return K1.getType() < K2.getType();
  }
};

using myint = int;

// FIXME: We should have a single warning for the struct specialization,
// proving that the check can look through type aliases.
template <>
struct std::hash<myint> {
  // no-warning: The warning was already reported for the struct itself.
  unsigned long operator()(const myint &K) const {
    return K;
  }
  // no-warning: The warning was already reported for the struct itself.
  bool operator()(const myint &K1,
                  const myint &K2) const {
    return K1 < K2;
  }
};

// FIXME: Expected a warning for this.
// Note the global namespace specifier (::).
template <>
struct ::std::hash<long> {
  unsigned long operator()(const long &K) const {
    return K;
  }
};

namespace ranges {
namespace detail {
struct diffmax_t {};
} // namespace detail
} // namespace ranges

namespace std {
// FIXME: We should have a single warning for the struct specialization.
template <>
struct numeric_limits<::ranges::detail::diffmax_t> {
  static constexpr bool is_signed = true;
  static constexpr bool is_integer = true;
  static constexpr ::ranges::detail::diffmax_t max() noexcept {
    return {};
  }
};
// FIXME: We should not have a warning for this two lines.
inline constexpr bool numeric_limits<::ranges::detail::diffmax_t>::is_signed;
inline constexpr bool numeric_limits<::ranges::detail::diffmax_t>::is_integer;
} // namespace std
