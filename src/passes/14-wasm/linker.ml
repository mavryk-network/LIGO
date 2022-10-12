    
(**
 * Find a file in the given locations.
 *) 
let find_file name locations = 
  let rec aux filename = function
  | [] -> failwith "Linker could not find: " ^ name ^ ". Most likely it was not installed yet. "
  | dir::dirs ->
    let filename' = Filename.concat dir filename in
    match Sys_unix.file_exists filename' with 
    | `No ->  aux filename dirs
    | `Unknown -> aux filename dirs
    | `Yes -> filename'
  in aux name locations

(**
 * Link the generated WebAssembly object file with:
 * - generated ligo-runtime (see: ./runtime)
 * - wasi (see ./wasi) 
 *
 * To have these files available at runtime, Dune's Sites feature is used::
 *  https://dune.readthedocs.io/en/stable/sites.html.
 *
 * Annoying Dune issue: for some reason Dune wants to use the archives, while 
 * it should only copy them. This results in the following warning when doing a clean `dune build` : 
 * > ld: warning: ignoring file src/passes/14-wasm/wasi/libc.a, building for macOS-arm64 
 * >              but attempting to link with file built for unknown-unsupported file format 
 * >              ( 0x21 0x3C 0x61 0x72 0x63 0x68 0x3E 0x0A 0x2F 0x20 0x20 0x20 0x20 0x20 0x20 0x20 )
 *)
let link files output =
  let libligo_wasi_share_dir = find_file "lib" Ligo_runtime.Sites.ligo_wasi in
  let libligo_wasi = find_file "libclang_rt.builtins-wasm32.a" Ligo_runtime.Sites.ligo_wasi in
  let libligo_runtime_location = find_file "libligo_runtime.a" Ligo_runtime.Sites.ligo_runtime in
  let command =
    sprintf
      "wasm-ld -m wasm32 --import-undefined -L%s -lc %s -lc %s %s --stack-first --fatal-warnings \
       -z stack-size=8388608 -o %s --export=entrypoint"
      libligo_wasi_share_dir
      libligo_wasi
      libligo_runtime_location
      (String.concat ?sep:(Some " ") files)
      output
  in
  Sys_unix.command_exn command
