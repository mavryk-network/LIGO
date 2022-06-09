compile:
	cargo wasi build --lib
	cp target/wasm32-wasi/debug/libligo_runtime.a libligo_runtime.debug.a
	# cargo clean
	RUSTFLAGS=-Zprint-type-sizes cargo +nightly wasi build --lib --release &> ligo-runtime-output.txt
	# cargo wasi build --lib --release
	# cbindgen --config cbindgen.toml --crate ligo-runtime --output ligo-runtime.h
	cp target/wasm32-wasi/release/libligo_runtime.a libligo_runtime.a 