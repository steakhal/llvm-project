# libsoundio's CMakeLists declares cmake_minimum_required(VERSION 2.8.5), which
# modern CMake rejects (compatibility with < 3.5 was removed). Pin the policy
# floor to 3.5 so configure proceeds under the analyzer image's CMake.
cmake . -DCMAKE_BUILD_TYPE=Debug -Bbuild -GNinja -DCMAKE_POLICY_VERSION_MINIMUM=3.5
cmake --build build
