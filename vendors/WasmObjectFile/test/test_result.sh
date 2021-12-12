#!/bin/bash

OUTPUT=$(./wasm-interp test.wasm --run-all-exports)

if [ "$OUTPUT" = "_start() => i32:10" ]; then
    exit 0
else
    echo "Running of test.wasm did not result in the expected end value"
    exit 1
fi
