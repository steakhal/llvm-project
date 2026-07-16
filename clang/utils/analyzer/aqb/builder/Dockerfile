# Builder image for AQB Clang Volumes. Given the AQB_* env contract (see
# aqb/volume.py), it fetches a commit, builds clang via a CMake preset, and
# installs it into the mounted clang volume (/opt/aqb/clang), caching objects in
# /ccache.
#
# Build once:  docker build -t aqb-clang-builder:latest clang/utils/analyzer/aqb/builder
#
# ubuntu:24.04 ships cmake 3.28, which understands LLVM's CMakePresets.json
# (schema version 6, requires cmake >= 3.25); ubuntu:22.04's cmake 3.22 is too
# old and fails with "Unrecognized version field".
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates git cmake ninja-build clang lld ccache python3 \
    && rm -rf /var/lib/apt/lists/*

COPY build.sh /usr/local/bin/aqb-build-clang
RUN chmod +x /usr/local/bin/aqb-build-clang

ENTRYPOINT ["/usr/local/bin/aqb-build-clang"]
