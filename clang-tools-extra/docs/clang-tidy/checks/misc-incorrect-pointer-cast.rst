.. title:: clang-tidy - misc-incorrect-pointer-cast

misc-incorrect-pointer-cast
===========================

Warns for cases when pointer is cast and the pointed to type is wider than the
allocated type.
For example `char` vs `int`, `long` vs `char` etc.
Also warns for cases when the pointed to type layout is different from the
allocated type layout, like different structs, `int` vs `float`/`double`,
different signedness.

Allows pointer casts if the pointed to struct type is "part" of the allocated
type.
Which means the allocated type contains the pointed to type member by member.

Options
-------

..  option:: WarnForDifferentSignedness

  This option can be configured to warn when the pointed to type signedness
  is different from the allocated type.
  Disabled by default because this option might be noisy on some code bases.

..  option:: IgnoreReinterpretCast

  This option can be configured to do not warn  when reinterpter cast is used.
  Disabled by default but this option might be useful on code bases where
  `reinterpret_cast` is used carefully.

Examles
-------

Cast char pointer to integer pointer.
Check will warn because of cast to a wider type.

.. code-block:: c++

    char c = 'a';
    int *i = (int *)&c;

Cast between structs.
Check will allow to cast to a narrower struct if it is part of the source struct
member by member.

.. code-block:: c++

    struct S1 {
      int a;
    };

    struct S2 {
      int a;
      double b;
    };

    struct S3 {
      double y;
      long x;
    };

    struct S2 s2;
    struct S1 *s1 = (struct S1 *)&s2; // Won't warn. Struct "S2" contains struct
                                      // "S2" member by member.
    struct S3 *s3 = (struct S3 *)&s2; // Warning because of different type
                                      // layout.

Cast with `reinterpret_cast`.
If the `IgnoreReinterpretCast` option is `0`, check will warn for these
kind of casts.

.. code-block:: c++

    char c = 'x';
    int *i = reinterpret_cast<int *>(&c);

Cast between different signedness types.
If the `WarnForDifferentSignedness` option is `1`, check will warn for these
kind of casts.

.. code-block:: c++

    unsigned int u;
    int i = (int *)&u;
