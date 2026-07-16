# Builder image for AQB Clang Volumes. Given the AQB_* env contract (see
# aqb/volume.py), it fetches a commit, builds clang via a CMake preset, and
# installs it into the mounted clang volume (/opt/aqb/clang), caching objects in
# /ccache.
#
# Build once:  <runtime> build -t aqb-clang-builder:latest clang/utils/analyzer/aqb/builder
#
# Base image, cmake, and toolchain mirror clang/utils/analyzer/Dockerfile (the
# SATest image): the Apple registry so it pulls in-env, Kitware cmake (LLVM's
# CMakePresets.json is schema v6, needs cmake >= 3.25), a pinned clang/lld host
# toolchain, and the corpus's system build dependencies so this image can also
# drive the analyze step.
FROM docker-upstream.apple.com/ubuntu:26.04
RUN apt-get update && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    gnupg \
    software-properties-common \
    wget

# Newer CMake is required by LLVM (and to read its v6 CMakePresets.json).
RUN wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor - | tee /etc/apt/trusted.gpg.d/kitware.gpg >/dev/null
RUN apt-add-repository -y 'deb https://apt.kitware.com/ubuntu/ resolute main'

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get install -y \
    clang=1:21.1.6-71 \
    lld=1:21.1.6-71

# Build toolchain.
RUN apt-get install -y \
    git=1:2.53.0-1ubuntu1 \
    gettext=0.23.2-1 \
    python3=3.14.3-0ubuntu2 \
    python3-pip=25.1.1+dfsg-1ubuntu2 \
    cmake=4.3.4-0kitware3ubuntu26.04.1 \
    cmake-data=4.3.4-0kitware3ubuntu26.04.1 \
    ninja-build=1.13.2-1

# box2d dependencies
RUN apt-get install -y \
    libx11-dev=2:1.8.13-1 \
    libxrandr-dev=2:1.5.4-1build1 \
    libxinerama-dev=2:1.1.4-3build2 \
    libxcursor-dev=1:1.2.3-1build1 \
    libxi-dev=2:1.8.2-2

# symengine dependencies
RUN apt-get install -y \
    libgmp10=2:6.3.0+dfsg-5ubuntu2 \
    libgmp-dev=2:6.3.0+dfsg-5ubuntu2

# simbody dependencies
RUN apt-get install -y \
    liblapack-dev=3.12.1-7ubuntu1

# drogon dependencies
RUN apt-get install -y \
    libjsonrpccpp-dev=0.7.0-2.2build4 \
    uuid-dev=2.41.3-3ubuntu2

# tmux dependencies
RUN apt-get install -y \
    autotools-dev=20240727.1build1 \
    automake=1:1.18.1-3build1 \
    libncurses-dev=6.6+20251231-1 \
    libevent-dev=2.1.12-stable-10build2 \
    pkg-config=2.5.1-4 \
    flex=2.6.4-8.2build2 \
    bison=2:3.8.2+dfsg-1build4

RUN apt-get install -y \
    libjpeg-dev

RUN apt-get install -y \
    ccache=4.12.3-1 \
    libz3-dev=4.13.3-1build1

RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 1

# The ccache volume is mounted here (contract constant, also in build.sh).
ENV CCACHE_DIR=/ccache
ENV CCACHE_MAXSIZE=30G

COPY build.sh /usr/local/bin/aqb-build-clang
RUN chmod +x /usr/local/bin/aqb-build-clang

ENTRYPOINT ["/usr/local/bin/aqb-build-clang"]
