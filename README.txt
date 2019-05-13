There are 3 folders. These are based on the official LLVM github mirror repository.
https://github.com/llvm/llvm-project.git

Based on commit: c0806e0d24add3895768ac1638a75b898433a03e
Author: Jonathan Metzman <metzman@chromium.org>
Date:   Tue Apr 30 23:46:52 2019 +0000


The folders already contain my changes. Which are:
  modified: clang-tools-extra/clang-tidy/bugprone/BugproneTidyModule.cpp
  modified: clang-tools-extra/clang-tidy/bugprone/CMakeLists.txt
  added: clang-tools-extra/clang-tidy/bugprone/StrictAliasingCheck.cpp
  added: clang-tools-extra/clang-tidy/bugprone/StrictAliasingCheck.h
  added: clang-tools-extra/docs/clang-tidy/checks/bugprone-strict-aliasing.rst
  modified: clang-tools-extra/docs/clang-tidy/checks/list.rst
  added: clang-tools-extra/test/clang-tidy/bugprone-strict-aliasing.cpp

But I also include the diff patch to be applied on top of the mentioned commit.
It is called 'add-strict-aliasing.patch'.

You can simply apply it on top the c0806e0d24add3895768ac1638a75b898433a03e commit of the github repo (after you have checked out) with:
git apply add-strict-aliasing.patch

