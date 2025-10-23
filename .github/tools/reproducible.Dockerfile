# syntax=docker/dockerfile:1
# Use Debian Bullseye (oldoldstable) for good compatibility while still being accessible
# debian:bullseye-20240722-slim
FROM debian:bullseye-20240722-slim AS builder

ENV SOURCE_DATE_EPOCH=1721606400

RUN apt-get update && apt-get -y upgrade && DEBIAN_FRONTEND=noninteractive apt-get -y install \
    build-essential \
    file \
    git \
    libfreetype6-dev \
    libx11-dev \
    x11-common \
    ninja-build \
    python3 \
    zlib1g-dev \
    ca-certificates \
    curl \
    xz-utils \
    lsb-release \
    wget \
    software-properties-common \
    gnupg \
    ;

# Install LLVM 18 from apt to bootstrap the build of LLVM 21
RUN wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 18 && \
    rm llvm.sh

ARG _GF_CLANG_VERSION=21.1.4
ARG _GF_CMAKE_VERSION=3.27.6

# Download CMake and LLVM sources
RUN curl -fsSL -o /cmake.tar.gz \
    https://github.com/Kitware/CMake/releases/download/v${_GF_CMAKE_VERSION}/cmake-${_GF_CMAKE_VERSION}.tar.gz && \
    curl -fsSL -o /llvm-project.tar.xz \
    https://github.com/llvm/llvm-project/releases/download/llvmorg-${_GF_CLANG_VERSION}/llvm-project-${_GF_CLANG_VERSION}.src.tar.xz

# Build and install CMake
RUN tar xf /cmake.tar.gz && \
    cd cmake-${_GF_CMAKE_VERSION} && \
    echo 'set(CMAKE_USE_OPENSSL OFF CACHE BOOL "" FORCE)' > gf-init.cmake && \
    ./bootstrap --parallel=$(nproc) --init=gf-init.cmake --generator=Ninja && \
    ninja install && \
    cd / && rm -rf /cmake* cmake-*

# Build and install Clang/LLVM toolchain using LLVM 18 as bootstrap compiler
RUN tar xf /llvm-project.tar.xz && \
    cmake -S llvm-project-${_GF_CLANG_VERSION}.src/llvm -B build-toolchain -GNinja \
        -DCMAKE_C_COMPILER=clang-18 \
        -DCMAKE_CXX_COMPILER=clang++-18 \
        -DCMAKE_C_FLAGS="-march=x86-64" \
        -DCMAKE_CXX_FLAGS="-march=x86-64" \
        -DLLVM_INCLUDE_DOCS=Off \
        -DLLVM_TARGETS_TO_BUILD=X86 \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX=/pinnedtoolchain \
        -DCOMPILER_RT_BUILD_SANITIZERS=Off \
        -DLIBCXX_HARDENING_MODE=fast \
        -DLLVM_ENABLE_PROJECTS='lld;clang;clang-tools-extra' \
        -DLLVM_ENABLE_RUNTIMES='compiler-rt;libcxx;libcxxabi;libunwind' && \
    cmake --build build-toolchain -t install && \
    cd / && rm -rf /build* /llvm*

# Create CMake toolchain file for reproducible builds
COPY <<-"EOF" /pinnedtoolchain/pinnedtoolchain.cmake
   set(CMAKE_C_COMPILER ${CMAKE_CURRENT_LIST_DIR}/bin/clang)
   set(CMAKE_CXX_COMPILER ${CMAKE_CURRENT_LIST_DIR}/bin/clang++)

   set(CMAKE_CXX_STANDARD_INCLUDE_DIRECTORIES ${CMAKE_CURRENT_LIST_DIR}/include/c++/v1 ${CMAKE_CURRENT_LIST_DIR}/include/x86_64-unknown-linux-gnu/c++/v1 /usr/local/include /usr/include)

   set(CMAKE_C_FLAGS_INIT "-march=x86-64 -D_FORTIFY_SOURCE=2 -fstack-protector-strong -fpie -pthread")
   set(CMAKE_CXX_FLAGS_INIT "-march=x86-64 -nostdinc++ -D_FORTIFY_SOURCE=2 -fstack-protector-strong -fpie -pthread")

   set(CMAKE_EXE_LINKER_FLAGS_INIT "-stdlib=libc++ -nostdlib++ -pie -pthread -Wl,-z,relro,-z,now")
   set(CMAKE_SHARED_LINKER_FLAGS_INIT "-stdlib=libc++ -nostdlib++")
   set(CMAKE_MODULE_LINKER_FLAGS_INIT "-stdlib=libc++ -nostdlib++")

   set(CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CURRENT_LIST_DIR}/lib/x86_64-unknown-linux-gnu/libc++.a ${CMAKE_CURRENT_LIST_DIR}/lib/x86_64-unknown-linux-gnu/libc++abi.a")
EOF

ENV CMAKE_TOOLCHAIN_FILE=/pinnedtoolchain/pinnedtoolchain.cmake

FROM builder AS build

ARG GF_BUILD_JOBS

COPY / /gf

RUN cmake -S /gf -B build \
        -DCMAKE_BUILD_TYPE=Release \
        -GNinja && \
    cmake --build build ${GF_BUILD_JOBS:+-j$GF_BUILD_JOBS}

FROM scratch AS exporter
COPY --from=build /build/gf /gf
