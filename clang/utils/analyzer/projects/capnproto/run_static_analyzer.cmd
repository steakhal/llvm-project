# Skip the test targets (BUILD_TESTING gates kj-*-tests etc.): kj's C++20
# coroutine-allocator test code (async-test.c++) doesn't compile under GCC 15
# (canConvert on an incomplete coroutine-lambda type), and capnproto targets
# clang. We only need the kj/capnp libraries analyzed, which still build.
cmake . -DCMAKE_BUILD_TYPE=Debug -Bbuild -GNinja -DBUILD_TESTING=OFF
cmake --build build
