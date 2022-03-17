TODO: automate this.

- install wasi: vendors/wasi/build.sh
- install gmp:  vendors/gmp/build.sh
- dune exec src/bin/runligo.exe -- compile contract --backend wasm <PATH_TO_CONTRACT>


- run:       `wasmtime out.wasm`
- debugging: `RUST_LOG=trace wasmtime out.wasm`
- text format of wasm: 
  - install wabt
  - `wabt/bin/wasm2wat out.wasm -o out.wat`