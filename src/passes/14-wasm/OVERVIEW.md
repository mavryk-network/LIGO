LIGO Wasm Backend
===

Background
---
The wasm backend takes mini-c types as input, but requires a bit of extra 
massage-ing to make it possible to translate to WebAssembly: 
- nested functions need to be moved (done)
- function handling will need to work with memory to make it possible to do 
  curried calls. (todo)

The LIGO wasm backend targets a minimal set of WebAssembly as described by the 
[Tezos L1 Wasm spec](todo proper naming + link). This also includes wasi which 
is basically libc for WebAssembly. 

The expectations of the VM are different from the Michelson VM. Where 
parameters, storage and operations are handled for you with Michelson - with 
WebAssembly this will need to be done _manually_. 

For this reason there's a LIGO runtime, which handles the loading and storing 
of data-structures. For maintenance reasons this has been done in Rust - as 
doing this manually is tedious, and doing it in C might introduce memory 
issues. There is one slight problem with this approach and that's the 
translation of data structures (TODO: describe proper way to manage this).

LIGO generates a [wasm object file](todo link) which is linked via LLVM's LLD 
with the LIGO runtime. This becomes a wasm file which can be executed in 
wasmtime.

To combine everything together LLVM's LLD is used for linking.

Overview
====




Used tooling:
- Rust for the ligo-runtime
- LLVM for linking
- wabt for debugging.
- wasmtime
- wasi