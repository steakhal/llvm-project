# Build only the static library target: it compiles (and thus analyzes) all the
# library TUs while skipping the final `duckdb` executable link. That link is a
# huge Debug static binary (jemalloc + extensions + duckdb_static) whose .text
# overflows AArch64's +/-128 MiB branch range (R_AARCH64_CALL26). We only need
# the sources analyzed, not a working binary.
cmake . -DCMAKE_BUILD_TYPE=Debug -Bbuild -GNinja
cmake --build build --target duckdb_static
