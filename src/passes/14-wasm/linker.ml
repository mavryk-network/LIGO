let link files output =
  ignore files;
  ignore output;
  (* TODO: get this installed somehow? *)
  (* not sure why the main is going wrong, try to change _start to clang like `main` *)
  (* "/Users/Sander/Projects/ligo/./wasi/wasi-sdk-14.0/bin/wasm-ld" -m wasm32 -L/Users/Sander/Projects/ligo/wasi/wasi-sdk-14.0/bin/../share/wasi-sysroot/lib/wasm32-wasi /Users/Sander/Projects/ligo/wasi/wasi-sdk-14.0/bin/../share/wasi-sysroot/lib/wasm32-wasi/crt1-command.o work_in_progress.wasm vendors/gmp/libgmp.a -lc /Users/Sander/Projects/ligo/wasi/wasi-sdk-14.0/lib/clang/13.0.0/lib/wasi/libclang_rt.builtins-wasm32.a -o out.wasm *)
  (* -lGL -lal -lhtml5 -lstubs-debug -lc-debug -lcompiler_rt -lc++-noexcept -lc++abi-noexcept -ldlmalloc -lstandalonewasm -lc_rt -lsockets -mllvm -combiner-global-alias-analysis=false -mllvm -enable-emscripten-sjlj -mllvm -disable-lsr --import-undefined --export-if-defined=__start_em_asm --export-if-defined=__stop_em_asm --export=emscripten_stack_get_end --export=emscripten_stack_get_free --export=emscripten_stack_init --export=stackSave --export=stackRestore --export=stackAlloc --export=__errno_location --export-table -z stack-size=5242880 --initial-memory=16777216 --max-memory=16777216 --global-base=1024 *)
  (* --lto-O3 -s --gc-sections --merge-data-segments *)
  let command =
    sprintf
      "./vendors/wasi/wasi-sdk-14.0/bin/wasm-ld -m wasm32 \
       -L${PWD}/vendors/wasi/wasi-sdk-14.0/share/wasi-sysroot/lib/wasm32-wasi \
       %s -lc \
       ${PWD}/vendors/wasi/wasi-sdk-14.0/lib/clang/13.0.0/lib/wasi/libclang_rt.builtins-wasm32.a \
       -lc ${PWD}/vendors/ligo-runtime/libligo_runtime.debug.a  --stack-first \
       --fatal-warnings -z stack-size=8388608 -o %s"
      (String.concat ?sep:(Some " ") files)
      "out.wasm"
  in
  (* let command = sprintf "./vendors/wasi/wasi-sdk-14.0/bin/wasm-ld -m wasm32 --lto-O3 -s --gc-sections --merge-data-segments -L${PWD}/vendors/wasi/wasi-sdk-14.0/share/wasi-sysroot/lib/wasm32-wasi %s -lc ${PWD}/vendors/wasi/wasi-sdk-14.0/lib/clang/13.0.0/lib/wasi/libclang_rt.builtins-wasm32.a -lc ${PWD}/vendors/ligo-runtime/libligo_runtime.a  --stack-first --fatal-warnings -z stack-size=8388608 -o %s" (String.concat ?sep:(Some " ") files) ("out.wasm") in *)
  (* let command = sprintf "./vendors/emscripten/emsdk/upstream/bin/wasm-ld -o %s -L. %s -L./vendors/emscripten/emsdk/upstream/emscripten/cache/sysroot/lib/wasm32-emscripten ./vendors/emscripten/emsdk/upstream/emscripten/cache/sysroot/lib/wasm32-emscripten/crt1.o -lGL -lal -lhtml5 -lstubs-debug -lc-debug -lcompiler_rt -lc++-noexcept -lc++abi-noexcept -ldlmalloc -lstandalonewasm -lc_rt -lsockets -mllvm -combiner-global-alias-analysis=false -mllvm -enable-emscripten-sjlj -mllvm -disable-lsr --import-undefined --export-if-defined=__start_em_asm --export-if-defined=__stop_em_asm --export=emscripten_stack_get_end --export=emscripten_stack_get_free --export=emscripten_stack_init --export=stackSave --export=stackRestore --export=stackAlloc --export=__errno_location --export-table -z stack-size=5242880 --initial-memory=16777216 --max-memory=16777216 --global-base=1024" "out.wasm" (String.concat ?sep:(Some " ") files) in *)
  let _ = Sys.command command in
  ()
