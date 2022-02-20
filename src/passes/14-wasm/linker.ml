let link files _output = 
  (* TODO: get this installed somehow? *)
  (* not sure why the main is going wrong, try to change _start to clang like `main` *)
  (* "/Users/Sander/Projects/ligo/./wasi/wasi-sdk-14.0/bin/wasm-ld" -m wasm32 -L/Users/Sander/Projects/ligo/wasi/wasi-sdk-14.0/bin/../share/wasi-sysroot/lib/wasm32-wasi /Users/Sander/Projects/ligo/wasi/wasi-sdk-14.0/bin/../share/wasi-sysroot/lib/wasm32-wasi/crt1-command.o work_in_progress.wasm vendors/gmp/libgmp.a -lc /Users/Sander/Projects/ligo/wasi/wasi-sdk-14.0/lib/clang/13.0.0/lib/wasi/libclang_rt.builtins-wasm32.a -o out.wasm *)
  let command = sprintf "./wasi/wasi-sdk-14.0/bin/lld -flavor wasm -m wasm32 -L/Users/Sander/Projects/ligo/wasi/wasi-sdk-14.0/bin/../share/wasi-sysroot/lib/wasm32-wasi %s -lc /Users/Sander/Projects/ligo/wasi/wasi-sdk-14.0/lib/clang/13.0.0/lib/wasi/libclang_rt.builtins-wasm32.a -o %s" (String.concat ?sep:(Some " ") files) ("out.wasm") in
  let _ = Sys.command command in
  ()