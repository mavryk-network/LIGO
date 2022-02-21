rm -rf gmp-6.2.1
wget -c https://gmplib.org/download/gmp/gmp-6.2.1.tar.lz
tar -xf gmp-6.2.1.tar.lz

# must be sure that emscripten is installed before
../emscripten/emsdk/emsdk install 3.1.1
../emscripten/emsdk/emsdk activate 3.1.1
source ../emscripten/emsdk/emsdk_env.sh

cd ../gmp-6.2.1
mkdir dist
CC_FOR_BUILD=cc ABI=standard emconfigure ./configure --disable-assembly --build wasm32-unknown-wasi --prefix=${PWD}/dist
emmake make
emmake make install
cp dist/lib/libgmp.a ../
cd ..
ls -l libgmp.a