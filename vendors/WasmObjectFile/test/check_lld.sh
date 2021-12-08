#!/bin/bash

if ! command -v lld &> /dev/null
then 
    echo "The LLVM LLD linker is not present on this system. Please install."
    exit 1
else    
    if lld -flavor wasm --version | grep -q "13.0.0" 
    then 
        exit 0
    else 
        lld -flavor wasm --version
        echo "An LLVM LLD linker was found, but with an incorrect version. We expect version 13.0.*. Please install."
        exit 2
    fi
fi