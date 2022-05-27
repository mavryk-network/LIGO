compile:
	cargo wasi build --lib
	cp target/wasm32-wasi/debug/libligo_runtime.a libligo_runtime.debug.a
	# RUSTFLAGS=-Zprint-type-sizes cargo +nightly wasi build --lib --release &> ligo-runtime-output.txt
	cargo wasi build --lib --release
	cp target/wasm32-wasi/release/libligo_runtime.a libligo_runtime.a