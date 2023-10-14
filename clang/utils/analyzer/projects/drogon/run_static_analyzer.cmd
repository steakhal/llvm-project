git submodule update --init

cmake . -DCMAKE_BUILD_TYPE=Debug -Bbuild -GNinja
cmake --build build
