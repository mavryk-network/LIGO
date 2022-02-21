export WASI_VERSION=14
export WASI_VERSION_FULL=${WASI_VERSION}.0

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    wget https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_VERSION}/wasi-sdk-${WASI_VERSION_FULL}-linux.tar.gz
    tar xvf wasi-sdk-${WASI_VERSION_FULL}-linux.tar.gz
elif [[ "$OSTYPE" == "darwin"* ]]; then
    wget https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_VERSION}/wasi-sdk-${WASI_VERSION_FULL}-macos.tar.gz
    tar xvf wasi-sdk-${WASI_VERSION_FULL}-macos.tar.gz
fi

