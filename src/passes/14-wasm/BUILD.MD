TODO: automate this.

- requirements: LLVM 13 with wasm support.
- install wasi: vendors/wasi/build.sh
- install gmp: vendors/gmp/build.sh
- dune exec src/bin/runligo.exe -- compile contract --backend wasm <PATH_TO_CONTRACT>
