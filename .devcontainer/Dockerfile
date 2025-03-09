# Use Ubuntu 20.04 LTS as the base image
FROM ubuntu:24.04

# Avoid prompts from apt
ENV DEBIAN_FRONTEND=noninteractive

# Update and install dependencies
RUN apt-get update && apt-get install -y \
    git \
    cmake \
    ninja-build \
    llvm-17 \
    llvm-17-dev \
    clang-17 \
    lld-17 \
    lldb-17 \
    libmlir-17-dev \
    mlir-17-tools \
    zlib1g-dev \
    libzstd-dev \
    libedit-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Set LLVM and Clang as the default compiler
ENV CC=/usr/bin/clang-17
ENV CXX=/usr/bin/clang++-17

# Set the LLVM_DIR environment variable
ENV LLVM_DIR=/usr/lib/llvm-17/lib/cmake/llvm/
ENV MLIR_DIR=/usr/lib/llvm-17/lib/cmake/mlir/

# Set the default shell to bash instead of sh
SHELL ["/bin/bash", "-c"]
