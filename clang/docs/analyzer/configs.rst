=======================
Analyzer config options
=======================

Overview
--------

The behavior of the analyzer engine can be fine-tuned by setting configuration
options. Setting these options might reduce the number of certain false-positives
or have an affect on the analysis by other means such as duration.
These config options are organized into the following categories:

- `Released <released_config>`_:
  These options are intended for a regular end-user.
  If one has unexpected checker reports, one might need to check these options
  to see if any of these configs could resolve the observable behavior.

  For example: <<cross-ref>>
    Setting the ``assume-controlled-environment=true`` would prevent e.g.
    ``getenv()`` ever returning ``NULL``.
    But it would also have effects on `taint-analysis` as well.

- `Alpha <alpha_config>`_:
  These config options are not meant for a regular end-user.
  These options are considered for opt-in experimental features.
  These options might have surprising effects on the analyzer engine.
  One should use these with caution.

- `Debug <debug_config>`_:
  These config options are intended for analyzer developers or tool vendors
  providing frontends for the Clang Static Analyzer such as
  `scan-build`_ or `CodeChecker`_.

Enabling config options
-----------------------

One can set these config options by passing the option to the analyzer frontend by:

.. code-block:: bash

  clang -cc1 -analyzer-config <option>=<value>

  clang -cc1 -analyzer-config ignore-bison-generated-files=false \
             -analyzer-config assume-controlled-environment=true \
             my-source.cpp

Alternatively, one can prepend `-Xanalyzer` to each token in the invocation:
``clang ... -Xanalyzer -analyzer-config -Xanalyzer <option>=<value>``.
However, it's highly recommended to not specify these directly,
rather by using some dedicated frontends such as
`scan-build`_ or `CodeChecker`_.


.. contents:: Table of Contents

------------


Available options
-----------------

.. _released_config:

Released config options
^^^^^^^^^^^^^^^^^^^^^^^

.. _mode:

``mode``
""""""""

  Controls the high-level analyzer mode, which influences the default settings for some of the lower-level config options (such as IPAMode_).

  Options:
    - ``deep``: TODO...
    - ``shallow``: TODO...

  Default value: ``deep``

  Related config options:
    - IPAMode_, TODO... <<cross-refs>>

.. _IPAMode:

``IPAMode``
"""""""""""

  Controls the mode of inter-procedural analysis.

  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras sed risus elit. Donec posuere venenatis arcu nec facilisis. Aliquam ut augue laoreet, viverra mauris nec, tristique dolor. Pellentesque pulvinar cursus ultrices. Ut sit amet sem eros. Ut varius consectetur lacus quis hendrerit. Fusce ullamcorper, enim quis ultrices vestibulum, leo dolor fermentum ligula, sed tincidunt mauris massa in sapien. Nulla facilisi. Proin sed eros ac erat dignissim accumsan et ut velit. Proin eu neque tempus, cursus felis id, dapibus neque. Praesent auctor velit non urna consectetur imperdiet. Nullam eleifend diam dui, mollis sagittis metus luctus et. Nam vel elit magna. Proin ac arcu ut neque fermentum feugiat. Nam porttitor iaculis est quis porta. Integer fringilla nec turpis sit amet egestas.

  Options:
    - ``none``: TODO...
    - ``basic-inlining``. TODO...
    - ``inlining``: TODO...
    - ``dynamic``: TODO...
    - ``dynamic-bifurcate``: TODO...

  Default value: ``inlining`` *(in shallow mode)*, ``dynamic-bifurcate`` *(in deep mode)*

  Related config options:
    - mode_


.. _scan-build: https://clang-analyzer.llvm.org/scan-build.html
.. _CodeChecker: https://github.com/Ericsson/CodeChecker
