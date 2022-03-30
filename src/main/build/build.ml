
open Simple_utils
open Trace
open Main_errors

let lib (s : Syntax_types.t) =
  match s with
  | PascaLIGO _ | ReasonLIGO | JsLIGO ->"
module String = struct
   [@inline] let length (s : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] s
   [@inline] let size (s : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] s
   [@inline] let sub (sli : nat * nat * string) : string = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] sli
   [@inline] let slice (sli : nat * nat * string) : string = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] sli
   [@inline] let concat (p : string * string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] p
end
module Crypto = struct
   [@inline] let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
   [@inline] let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
   [@inline] let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
   [@inline] let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
   [@inline] let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
   [@inline] let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k
   [@inline] let check ((k, s, b) : key * signature * bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
end
module Bytes = struct
   [@inline] let concat (p : bytes * bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] p
   [@inline] let sub (sli : nat * nat * bytes) : bytes = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] sli
   [@inline] let pack (type a) (x : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] x
   [@inline] let length (b : bytes) : nat = [%Michelson ({| { SIZE } |} : bytes -> nat)] b
   (* let unpack (type a) (b : bytes) : a option = [%Michelson ({| { UNPACK } |} : bytes -> a option)] b *)
end
module List = struct
   [@inline] let length (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)] xs
   [@inline] let size (type a) (xs : a list) : nat = length xs
   [@inline] let head_opt (type a) (xs : a list) : a option =
     match xs with
     | [] -> None
     | x :: _ -> Some x
   [@inline] let tail_opt (type a) (xs : a list) : (a list) option =
     match xs with
     | [] -> None
     | _ :: xs -> Some xs
end
"
  | CameLIGO -> "
module String = struct
   [@inline] let length (s : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] s
   [@inline] let size (s : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] s
   [@inline] let sub (sli : nat * nat * string) : string = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] sli
   [@inline] let slice (sli : nat * nat * string) : string = [%Michelson ({| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] sli
   [@inline] let sub (start : nat) (length : nat) (input : string) : string = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] (start, length, input)
   [@inline] let concat (b : string) (c : string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] (b, c)
end
module Crypto = struct
   [@inline] let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
   [@inline] let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
   [@inline] let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
   [@inline] let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
   [@inline] let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
   [@inline] let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k
   [@inline] let check (k : key) (s : signature) (b : bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
end
module Bytes = struct
   [@inline] let concat (b : bytes) (c : bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] (b, c)
   [@inline] let sub (start : nat) (length : nat) (input : bytes) : bytes = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] (start, length, input)
   [@inline] let pack (type a) (x : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] x
   [@inline] let length (b : bytes) : nat = [%Michelson ({| { SIZE } |} : bytes -> nat)] b
   (* let unpack (type a) (b : bytes) : a option = [%Michelson ({| { UNPACK } |} : bytes -> a option)] b *)
end
module List = struct
   [@inline] let length (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)] xs
   [@inline] let size (type a) (xs : a list) : nat = length xs
   [@inline] let head_opt (type a) (xs : a list) : a option =
     match xs with
     | [] -> None
     | x :: _ -> Some x
   [@inline] let tail_opt (type a) (xs : a list) : (a list) option =
     match xs with
     | [] -> None
     | _ :: xs -> Some xs
end
"

module type Params = sig
  val raise : all raise
  val add_warning : Main_warnings.all -> unit
  val options : Compiler_options.t
end

module M (Params : Params) =
  struct
    let raise = Params.raise
    let add_warning = Params.add_warning
    let options = Params.options
    type file_name = string
    type module_name = string
    type compilation_unit = Buffer.t
    type meta_data = Ligo_compile.Helpers.meta
    let preprocess : file_name -> compilation_unit * meta_data * (file_name * module_name) list =
      fun file_name ->
      let meta = Ligo_compile.Of_source.extract_meta ~raise "auto" file_name in
      let c_unit, deps = Ligo_compile.Helpers.preprocess_file ~raise ~meta ~options:options.frontend file_name in
      c_unit,meta,deps
    module AST = struct
      type declaration = Ast_typed.declaration
      type t = declaration list
      type environment = Environment.t
      let add_ast_to_env : t -> environment -> environment = fun ast env ->
        Environment.append ast env
      let add_module_to_env : module_name -> environment -> environment -> environment =
        fun module_name ast_typed_env env ->
          let module_name = Ast_typed.ModuleVar.of_input_var module_name in
          Environment.add_module ~public:() module_name (Environment.to_program ast_typed_env) env
      let init_env : environment = options.middle_end.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        let module_ = Location.wrap (Ast_typed.M_struct ast_typed) in
        let module_binder = Ast_typed.ModuleVar.of_input_var module_binder in
        Location.wrap Ast_typed.(Declaration_module {module_binder;module_;module_attr={public=true}})
      let make_module_alias : module_name -> file_name -> declaration =
        fun module_name file_name ->
        let module_binder = Ast_typed.ModuleVar.of_input_var module_name in
        let file_name   = Ast_typed.ModuleVar.of_input_var file_name in
        let module_ = Location.wrap (Ast_typed.M_variable file_name) in
        Location.wrap Ast_typed.(Declaration_module {module_binder;module_;module_attr={public=true}})
    end
    let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
      fun env file_name meta c_unit ->
      let options = Compiler_options.set_init_env options env in
      let pre, _ = Ligo_compile.Utils.type_contract_string ~raise ~add_warning:(fun _ -> ()) ~options CameLIGO (lib meta.syntax) in
      let options = Compiler_options.set_init_env options (Environment.append pre env) in
      let ast_core = Ligo_compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit file_name in
      let ast_typed = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Ligo_compile.Of_core.Env ast_core in
      ast_typed

  end

module Infer (Params : Params) = struct
  include M(Params)
  module AST = struct
    include AST
    type declaration = Ast_core.declaration
    type t = declaration list
      type environment = Environment.core
      let add_ast_to_env : t -> environment -> environment = fun ast env ->
        Environment.append_core ast env
      let add_module_to_env : module_name -> environment -> environment -> environment =
        fun module_name ast_typed_env env ->
          let module_name = Ast_core.ModuleVar.of_input_var module_name in
          Environment.add_core_module ~public:() module_name (Environment.to_core_program ast_typed_env) env
      let init_env : environment = Environment.init_core @@ Checking.untype_program @@ Environment.to_program @@ options.middle_end.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        let module_ = Location.wrap (Ast_core.M_struct ast_typed) in
        let module_binder = Ast_core.ModuleVar.of_input_var module_binder in
        Location.wrap Ast_core.(Declaration_module {module_binder;module_;module_attr={public=true}})
      let make_module_alias : module_name -> file_name -> declaration =
        fun module_name file_name ->
        let module_binder = Ast_core.ModuleVar.of_input_var module_name in
        let file_name   = Ast_core.ModuleVar.of_input_var file_name in
        let module_ = Location.wrap (Ast_core.M_variable file_name) in
        Location.wrap Ast_core.(Declaration_module {module_binder;module_;module_attr={public=true}})
  end

  let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
    fun _ file_name meta c_unit ->
    let _, pre = Ligo_compile.Utils.type_contract_string ~raise ~add_warning:(fun _ -> ()) ~options CameLIGO (lib meta.syntax) in
    let module_ = Ligo_compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit file_name in
    pre @ module_

end

module Build(Params : Params) = BuildSystem.Make(M(Params))

type file_name = string

let dependency_graph ~raise ~add_warning : options:Compiler_options.t -> Ligo_compile.Of_core.form -> file_name -> _ =
  fun ~options _form file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    dependency_graph file_name

let infer_contract ~raise ~add_warning : options:Compiler_options.t -> file_name -> Ast_core.module_ =
  fun ~options main_file_name ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end)) in
    trace ~raise build_error_tracer @@ from_result (compile_separate main_file_name)

let type_contract ~raise ~add_warning : options:Compiler_options.t -> file_name -> _ =
  fun ~options file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    trace ~raise build_error_tracer @@ from_result (compile_separate file_name)

let build_context ~raise ~add_warning : options:Compiler_options.t -> file_name -> Ast_typed.program =
  fun ~options file_name ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end)) in
    let contract = trace ~raise build_error_tracer @@ from_result (compile_combined file_name) in
    let contract = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Env contract in
    contract

let build_typed ~raise ~add_warning :
  options:Compiler_options.t -> Ligo_compile.Of_core.form -> file_name -> Ast_typed.program * Ast_typed.program =
    fun ~options entry_point file_name ->
      let open Build(struct
        let raise = raise
        let add_warning = add_warning
        let options = options
      end) in
      let contract = build_context ~raise ~add_warning ~options file_name in
      let applied =
        match entry_point with
        | Ligo_compile.Of_core.Contract entrypoint ->
          trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_contract entrypoint contract
        | View (view_name,main_name) ->
          trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_view view_name main_name contract
        | Env -> contract
      in
      applied, contract

let build_expression ~raise ~add_warning : options:Compiler_options.t -> string -> string -> file_name option -> _ =
  fun ~options syntax expression file_name ->
    let contract, aggregated_prg =
      match file_name with
      | Some init_file ->
         let module_ = build_context ~raise ~add_warning ~options init_file in
         let contract = Ligo_compile.Of_typed.compile_program ~raise module_ in
         (module_, contract)
      | None ->
         let syntax   = Syntax.of_string_opt ~raise (Syntax_name syntax) None in
         let module_, _ = Ligo_compile.Utils.type_contract_string ~raise ~add_warning:(fun _ -> ()) ~options CameLIGO (lib syntax) in
         let contract = Ligo_compile.Of_typed.compile_program ~raise module_ in
         (module_, contract)
    in
    let typed_exp       = Ligo_compile.Utils.type_expression ~raise ~options file_name syntax expression contract in
    let aggregated      = Ligo_compile.Of_typed.compile_expression_in_context ~raise typed_exp aggregated_prg in
    let mini_c_exp      = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    (mini_c_exp ,aggregated)

(* TODO: this function could be called build_michelson_code since it does not really reflect a "contract" (no views, parameter/storage types) *)
let build_contract ~raise ~add_warning : options:Compiler_options.t -> string -> file_name -> Stacking.compiled_expression * Ast_typed.program =
  fun ~options entry_point file_name ->
    let entry_point = Ast_typed.ValueVar.of_input_var entry_point in
    let typed_prg, contract = build_typed ~raise ~add_warning ~options (Ligo_compile.Of_core.Contract entry_point) file_name in
    let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise typed_prg entry_point in
    let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    let michelson  = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
    michelson, contract

let build_views ~raise ~add_warning :
  options:Compiler_options.t -> string -> string list * Ast_typed.program -> file_name -> (Ast_typed.ValueVar.t * Stacking.compiled_expression) list =
  fun ~options main_name (declared_views,program) source_file ->
    let main_name = Ast_typed.ValueVar.of_input_var main_name in
    let views =
      let annotated_views = Ligo_compile.Of_typed.get_views @@ program in
      match declared_views with
      | [] -> List.map annotated_views ~f:fst
      | _ -> (
        (* detects whether a declared view (passed with --views command line option) overwrites an annotated view ([@view] let ..)*)
        let () = List.iter annotated_views
          ~f:(fun (x,loc) ->
            if Option.is_none (List.find ~f:(fun s -> Ast_typed.ValueVar.is_name x s) declared_views) then
              add_warning (`Main_view_ignored loc)
          )
        in
        List.map ~f:Ast_typed.ValueVar.of_input_var declared_views
      )
    in
    match views with
    | [] -> []
    | _ ->
    let _, contract  = build_typed ~raise ~add_warning:(fun _ -> ()) ~options (Ligo_compile.Of_core.View (views,main_name)) source_file in
    let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_view ~raise contract views in
    let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    let mini_c = Self_mini_c.all_expression ~raise mini_c in
    let mini_c_tys = trace_option ~raise (`Self_mini_c_tracer (Self_mini_c.Errors.corner_case "Error reconstructing type of views")) @@
                       Mini_c.get_t_tuple mini_c.type_expression in
    let aux i view =
      let idx_ty = trace_option ~raise (`Self_mini_c_tracer (Self_mini_c.Errors.corner_case "Error reconstructing type of view")) @@
                     List.nth mini_c_tys i in
      let idx = Mini_c.e_proj mini_c idx_ty i (List.length views) in
      let idx = Self_mini_c.all_expression ~raise idx in
      (view, idx) in
    let views = List.mapi ~f:aux views in
    let aux (vn, mini_c) = (vn, Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c) in
    let michelsons = List.map ~f:aux views in
    michelsons
