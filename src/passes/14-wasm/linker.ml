let link files output =
  let command =
    sprintf
      "./vendors/wasi/wasi-sdk-14.0/bin/wasm-ld -m wasm32 \
       -L${PWD}/vendors/wasi/wasi-sdk-14.0/share/wasi-sysroot/lib/wasm32-wasi \
       %s -lc \
       ${PWD}/vendors/wasi/wasi-sdk-14.0/lib/clang/13.0.0/lib/wasi/libclang_rt.builtins-wasm32.a \
       -lc ${PWD}/vendors/ligo-runtime/libligo_runtime.a  --stack-first \
       --fatal-warnings -z stack-size=8388608 -o %s"
      (String.concat ?sep:(Some " ") files)
      output
  in
  let _ = Sys.command command in
  ()
