#pragma clang system_header

namespace std {

template<class T, T v>
struct integral_constant {
    static constexpr T value = v;
    typedef T value_type;
    typedef integral_constant type;
    constexpr operator value_type() const noexcept { return value; }
};

template <bool B>
using bool_constant = integral_constant<bool, B>;
using true_type = bool_constant<true>;
using false_type = bool_constant<false>;

template<class T>
struct is_error_code_enum : false_type {};

template <class T>
void swap(T &a, T &b);

enum class io_errc {
  stream = 1,
};

template <class... Types>
class tuple;

template <typename T = void>
class less;

template <>
class less<void> {
public:
  template <typename T, typename U>
  bool operator()(T &&Lhs, U &&Rhs) const {
    return static_cast<T &&>(Lhs) < static_cast<U &&>(Rhs);
  }
};

template <class Key>
struct hash;

template <class T>
class numeric_limits;

} // namespace std
