rm -rf emsdk
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
git pull
./emsdk install 3.1.1
./emsdk activate 3.1.1
source ./emsdk_env.sh
