FROM ubuntu:jammy

RUN apt-get update && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    gnupg \
    software-properties-common \
    wget

# newer CMake is required by LLVM
RUN wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor - | tee /usr/share/keyrings/kitware-archive-keyring.gpg >/dev/null && \
    echo 'deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ jammy main' | tee /etc/apt/sources.list.d/kitware.list >/dev/null && \
    apt-get update && \
    rm /usr/share/keyrings/kitware-archive-keyring.gpg && \
    apt-get install kitware-archive-keyring

# test system dependencies
RUN apt-get update && apt-get install -y \
    git=1:2.34.1* \
    gettext=0.21-4* \
    python3=3.10.6-1~22.04 \
    python3-pip=22.0.2* \
    cmake=3.27.6* \
    cmake-data=3.27.6* \
    ninja-build=1.10.1-1

# box2d dependencies
RUN apt-get install -y \
    libx11-dev=2:1.7.5-1* \
    libxrandr-dev=2:1.5.2-1* \
    libxinerama-dev=2:1.1.4-3 \
    libxcursor-dev=1:1.2.0-2* \
    libxi-dev=2:1.8-1*

# symengine dependencies
RUN apt-get install -y \
    libgmp10=2:6.2.1* \
    libgmp-dev=2:6.2.1*

# simbody dependencies
RUN apt-get install -y \
    liblapack-dev=3.10.0-2*

# drogon dependencies
RUN apt-get install -y \
    libjsonrpccpp-dev=0.7.0-1.1* \
    uuid-dev=2.37.2-4*

# tmux dependencies
RUN apt-get install -y \
    autotools-dev=20220109.1 \
    automake=1:1.16.5-1.3 \
    libncurses5-dev=6.3-2* \
    libevent-dev=2.1.12* \
    pkg-config=0.29.2-1* \
    flex=2.6.4-8* \
    bison=2:3.8.2*

# re2 dependencies
RUN apt-get install -y \
    libabsl-dev=0~20210324.2-2

RUN apt-get install -y \
    libjpeg-dev=8c-2*

RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 1

VOLUME /analyzer
VOLUME /projects
VOLUME /llvm-project
VOLUME /build
VOLUME /scripts

ENV PATH="/analyzer/bin:${PATH}"

ADD requirements.txt /requirements.txt
RUN pip3 install -r /requirements.txt

ADD entrypoint.py /entrypoint.py

ENTRYPOINT ["python", "/entrypoint.py"]
