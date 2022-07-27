compile:
	cd src && cargo clean
	# cd src && cargo wasi build --lib --release #for testing purposes... (println works here)
	cd src && cargo build --lib --target wasm32-unknown-unknown --release
	# cp src/target/wasm32-unknown-unknown/release/libligo_runtime.a libligo_runtime.a