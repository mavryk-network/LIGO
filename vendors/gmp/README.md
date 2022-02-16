A version of GMP compiled to WebAssembly. 

The generated libgmp.a archive can be used by LLVM's LLD tool.

To see what's inside the archive: llvm-ar x libgmp.a 
(lots of files will be produced by this in the current folder).