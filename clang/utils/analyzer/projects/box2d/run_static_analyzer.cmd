# BOX2D_SAMPLES defaults ON and FetchContent-pulls GLFW, which needs the host
# tool wayland-scanner just to configure (absent in the analyzer image). We only
# want to analyze box2d's own library, so skip the samples subdir entirely.
cmake . -DCMAKE_BUILD_TYPE=Debug -Bbuild -GNinja -DBOX2D_SAMPLES=OFF
cmake --build build
