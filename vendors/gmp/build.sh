rm -rf gmp-6.2.1
rm -rf emsdk
wget -c https://gmplib.org/download/gmp/gmp-6.2.1.tar.lz
tar -xf gmp-6.2.1.tar.lz

git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
git pull
./emsdk install 3.1.1
./emsdk activate 3.1.1
source ./emsdk_env.sh

cd ../gmp-6.2.1
mkdir dist
CC_FOR_BUILD=gcc ABI=standard emconfigure ./configure --disable-assembly --build wasm32-unknown-wasi --prefix=${PWD}/dist
emmake make
emmake make install
cp dist/lib/libgmp.a ../
cd ..
ls -l libgmp.a