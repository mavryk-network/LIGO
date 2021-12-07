Important: use the right LLD version!!! Version 13. How to enforce?

To help with debugging the tests, the following can be used:

https://github.com/WebAssembly/wabt

Local command:
dune exec test/test.exe && /usr/local/Cellar/llvm/13.0.0_1/bin/lld -flavor wasm  testml.wasm  -o foo.wasm && ../../../wabt/bin/wasm-validate foo.wasm