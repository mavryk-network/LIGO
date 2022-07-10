compile:
	# rm -f ../../src/passes/14-wasm/interop.ml
	# rm -f ../../src/passes/14-wasm/mem_helpers.ml
	# cargo clean
	cargo wasi build --lib 
	cargo +nightly fmt
	cargo doc --target-dir docs --no-deps --lib --document-private-items
	cp target/wasm32-wasi/debug/libligo_runtime.a libligo_runtime.debug.a
	# RUSTFLAGS=-Zprint-type-sizes cargo +nightly wasi build --lib --release &> ligo-runtime-output.txt
	cargo wasi build --lib --release
	# cbindgen --config cbindgen.toml --crate ligo-runtime --output ligo-runtime.h
	cp target/wasm32-wasi/release/libligo_runtime.a libligo_runtime.a
