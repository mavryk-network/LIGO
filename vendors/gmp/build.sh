rm -rf gmp-6.2.1
rm -rf wasi
git clone https://github.com/torquem-ch/gmp-wasm gmp-6.2.1

export WASI_VERSION=14
export WASI_VERSION_FULL=${WASI_VERSION}.0

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    wget https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_VERSION}/wasi-sdk-${WASI_VERSION_FULL}-linux.tar.gz
    tar xvf wasi-sdk-${WASI_VERSION_FULL}-linux.tar.gz
elif [[ "$OSTYPE" == "darwin"* ]]; then
    wget https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_VERSION}/wasi-sdk-${WASI_VERSION_FULL}-macos.tar.gz
    tar xvf wasi-sdk-${WASI_VERSION_FULL}-macos.tar.gz
fi

WASI_SDK_PATH=wasi/wasi-sdk-14.0

cd gmp-6.2.1
make clean
rm -rf dist
mkdir dist
./configure --prefix ${PWD}/dist CC=${PWD}/../wasi-sdk-14.0/bin/clang RANLIB=${PWD}/../wasi-sdk-14.0/bin/llvm-ranlib AR=${PWD}/../wasi-sdk-14.0/bin/llvm-ar --disable-assembly --host none
make 
make check
make install

cp dist/lib/libgmp.a ../
cd ..
ls -l libgmp.a