export WABT_VERSION_FULL=1.0.30

if [[ "$OSTYPE" == "linux-"* ]]; then
    tar xvf wabt-${WABT_VERSION_FULL}-ubuntu.tar
elif [[ "$OSTYPE" == "darwin"* ]]; then
    tar xvf wabt-${WABT_VERSION_FULL}-macos-12.tar
fi

cp wabt-${WABT_VERSION_FULL}/bin/wat2wasm .