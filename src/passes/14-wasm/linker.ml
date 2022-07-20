
let find_file name locations = 
  let rec aux filename = function
  | [] -> failwith "Linker could not find: " ^ name ^ ". Most likely it was not installed yet. "
  | dir::dirs ->
    let filename' = Filename.concat dir filename in
    match Sys.file_exists filename' with 
    | `No ->  aux filename dirs
    | `Unknown -> aux filename dirs
    | `Yes -> filename'
  in aux name locations

let link files output =
  let libligo_wasi_share_dir = find_file "lib" Ligo_runtime.Sites.ligo_wasi in
  let libligo_runtime_location = find_file "libligo_runtime.a" Ligo_runtime.Sites.ligo_runtime in
  let libligo_wasi_1 = find_file "libclang_rt.builtins-wasm32.a" Ligo_runtime.Sites.ligo_wasi in
  let command =
    sprintf
      "wasm-ld -m wasm32 \
      -L%s \
      -lc %s -lc %s %s --stack-first \
       --fatal-warnings -z stack-size=8388608 -o %s"
      libligo_wasi_share_dir
      libligo_wasi_1
      libligo_runtime_location
      (String.concat ?sep:(Some " ") files)
      output
  in
  print_endline command;
  let _ = Sys.command command in
  ()
