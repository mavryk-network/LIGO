export WASI_VERSION=14
export WASI_VERSION_FULL=${WASI_VERSION}.0

if [[ "$OSTYPE" == "linux-"* ]]; then
    tar xvf wasi-sdk-${WASI_VERSION_FULL}-linux.tar.gz
elif [[ "$OSTYPE" == "darwin"* ]]; then
    tar xvf wasi-sdk-${WASI_VERSION_FULL}-macos.tar.gz
fi

cp wasi-sdk-${WASI_VERSION_FULL}/lib/clang/13.0.0/lib/wasi/libclang_rt.builtins-wasm32.a libclang_rt.builtins-wasm32.a
rm -rf wasm32-wasi/
mkdir wasm32-wasi/
cp wasi-sdk-${WASI_VERSION_FULL}/share/wasi-sysroot/lib/wasm32-wasi/* ./
