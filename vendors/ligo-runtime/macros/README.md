Macros
===
This crates exposes macros that make it easier to interop between generated 
wasm code of Rust and LIGO. Note that not _all_ sorts of functions are supported. 

For those who are not familiar with Rust macros, think of it as Rust version of OCaml's PPX. 

expose_macro
===
Creates necessary symbols and imports for functions in Rust. The generated code is in: src/passes/14-wasm/interop.ml.

expose
---
Add this to functions that need to be exported. 

produce_file
---
A rather silly way to produce the src/passes/14-wasm/interop.ml file. Should ideally be removed.

datatype_helper
===
Generates objects to make it easier to work with the datatypes in Rust from LIGO. The objects hide
the wasm specific code to work with the datatypes. The generated code is in: src/passes/14-wasm/mem_helpers.ml

produce_datatype_file
---
A rather silly way to produce the src/passes/14-wasm/mem_helpers.ml file. Should ideally be removed.