FROM resolute-20260707@sha256:3131b4cc82a783df6c9df078f86e01819a13594b865c2cad47bd1bca2b7063bb
RUN apt-get update && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    gnupg \
    software-properties-common \
    wget

# newer CMake is required by LLVM
RUN wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor - | tee /etc/apt/trusted.gpg.d/kitware.gpg >/dev/null
RUN apt-add-repository -y 'deb https://apt.kitware.com/ubuntu/ resolute main'

#ENV TZ=Etc/UTC
ARG DEBIAN_FRONTEND=noninteractive 
RUN apt-get install -y \
    clang=1:21.1.6-71 \
    lld=1:21.1.6-71

# test system dependencies
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

VOLUME /ccache
ENV CCACHE_DIR=/ccache
ENV CCACHE_MAXSIZE=30G

ENV PATH="/analyzer/bin:${PATH}"

ADD entrypoint.py /entrypoint.py

ADD requirements.txt /requirements.txt
RUN pip3 install -r /requirements.txt --break-system-packages

ENTRYPOINT ["python", "/entrypoint.py"]
