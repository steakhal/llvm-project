.. title:: clang-tidy - bugprone-strict-aliasing

bugprone-strict-aliasing
========================

The check finds usages of bitcast (reinterpret cast) expressions which are most
likely errors.

The ``reinterpret_cast`` turns a reference or pointer to the given type without
any conversions. This could break the C/C++ aliasing rules which is undefined
behavior and could result in crashing the application due to breaking the
alignment requirements or mis-optimized code.

As a rule of thumb you can cast to char-like types, to the signed/unsigned
counterpart and to the dynamic type of the object. In C you can cast to the
struct's first direct member. In C++ it is required to have standard layout for
structs and classes to be able to cast to it's first direct non-static data
member. If the struct/class is empty, than you can cast to the first non-empty
standard layout base's first non-static data member.

Casting between unrelated fundamental types: '(int*)&myfloat'
-------------------------------

A fundamental type like `float` can only be casted to char-like type.
Eg can not use a `float` as an `unsigned int`.

.. code-block:: c++

  float fl = 3.14;
  unsigned int fl_int = *(unsigned int)&fl;

For this use `std::memcpy` for type-punning.

.. code-block:: c++

  float fl = 3.14;
  unsigned int fl_int;
  static_assert(sizeof(float) == sizeof(int), "for punning, sizes must equal");
  std::memcpy(&fl_int, &fl, sizeof(float));

Options
-------

.. option:: WarnOnlyIfDereferenced

   When non-zero, the check will warn on a bad bitcast expression even if the
   resulting pointer is not dereferenced right after the cast. Default is `1`.
   Otherwise it will warn on each bad bitcast expression.
