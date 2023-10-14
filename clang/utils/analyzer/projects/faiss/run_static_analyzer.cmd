cmake . -DCMAKE_BUILD_TYPE=Debug -Bbuild -GNinja -DFAISS_ENABLE_GPU=OFF -DFAISS_ENABLE_PYTHON=OFF
cmake --build build
