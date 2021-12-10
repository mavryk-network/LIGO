#!/bin/bash

git clone --recursive https://github.com/WebAssembly/wabt
cd wabt
git checkout 1.0.24
git submodule update --init

mkdir build
cd build

if ! command -v cmake &> /dev/null
then 
    echo "cmake is not present on this machine. Please install. "
    exit 1
else    
    cmake ..
    cmake --build .
    mv wasm-validate ../../wasm-validate
fi