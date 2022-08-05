let get_lib : Environment.Protocols.t -> Syntax_types.t -> backend:string -> test_enabled:bool -> string = fun protocol stx ~backend ~test_enabled ->
  let def str = "#define " ^ str ^ "\n" in
  let test_module = if test_enabled then def "TEST_LIB" else "" in
  let func_type =
    match stx with
    | ( PascaLIGO | ReasonLIGO | JsLIGO) -> def "UNCURRY"
    | CameLIGO                             -> def "CURRY"
  in
  let backend = def (String.uppercase backend) in
  let std = match protocol with
    | Environment.Protocols.Jakarta -> def "JAKARTA"
    | Environment.Protocols.Kathmandu -> def "KATHMANDU"
  in
  let lib = Ligo_lib.get () in
  test_module ^ backend ^ func_type ^ std ^ lib

let stdlib ~options syntax =
  let backend = match options.Compiler_options.backend.backend with 
    `Michelson -> "MICHELSON"
  | `Wasm -> "WASM"
  in
  let lib = get_lib Compiler_options.(options.middle_end.protocol_version) syntax ~backend ~test_enabled:Compiler_options.(options.middle_end.test) in
  match Simple_utils.Trace.to_stdlib_result @@
          Ligo_compile.Utils.type_program_string ~options CameLIGO lib with
  | Ok (s,_w) -> s
  | Error (e,_w) ->
     let error_msg = Format.asprintf "%a" (Main_errors.Formatter.error_ppformat ~display_format:Human_readable) e in
     failwith ("Error compiling the stdlib: " ^ error_msg)


let typed ~options (syntax : Syntax_types.t) =
  let open Helpers in
  let k = build_key ~options syntax in
  internalize_typed @@
    match LanguageMap.find_opt k @@ ! std_lib_cache with
    | None ->
       let typed, core = stdlib ~options syntax in
       std_lib_cache := LanguageMap.add k (typed, core) @@ !std_lib_cache;
       typed
    | Some (typed, _) -> typed

let core ~options (syntax : Syntax_types.t) =
  let open Helpers in
  let k = build_key ~options syntax in
  internalize_core @@
    match LanguageMap.find_opt k @@ ! std_lib_cache with
    | None ->
       let typed, core = stdlib ~options syntax in
       std_lib_cache := LanguageMap.add k (typed, core) @@ ! std_lib_cache;
       core
    | Some (_, core) -> core
