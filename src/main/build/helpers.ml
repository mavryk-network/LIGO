(* This pass turns 'sap_' prefixed outer type quantification into Singleton kind *)
let sap_for_all (b : (Ast_core.type_expression option) Ligo_prim.Binder.t) =
  let open Ast_core in
  let f t =
    match t.type_content with
    | T_for_all { ty_binder ; type_ ; kind = _ } when not (Ligo_prim.Type_var.is_generated ty_binder) ->
       let name = Ligo_prim.Type_var.to_name_exn ty_binder in
       if String.is_prefix name ~prefix:"sap_" then
         let type_content = T_for_all { ty_binder ; type_ ; kind = Ligo_prim.Kind.Singleton } in
         { t with type_content }
       else
         t
    | _ -> t in
  let b = Ligo_prim.Binder.map (Option.map ~f) b in
  b

(* This pass removes '@' prefix from binders.
   E.g., `let @or = ...` becomes `let or = ...` *)
let at_prefix (b : (Ast_core.type_expression option) Ligo_prim.Binder.t) =
  if not (Ligo_prim.Value_var.is_generated @@ Ligo_prim.Binder.get_var b) then
    let name = Ligo_prim.Value_var.to_name_exn @@ Ligo_prim.Binder.get_var b in
    match String.chop_prefix name ~prefix:"@" with
    | Some name -> Ligo_prim.Binder.set_var b @@ Ligo_prim.Value_var.of_input_var name
    | None -> b
  else
    b

let internalize_core ?(inner = false) (ds : Ast_core.program) : Ast_core.program =
  let open Ast_core in
  let rec f ~inner (d : declaration_content) : declaration_content = match d with
    | D_module { module_binder ; module_ ; module_attr = _ } ->
      let module_attr = TypeOrModuleAttr.{ public = inner ; hidden = true } in
      let module_ = match module_ with
        | { wrap_content = M_struct x ; _ } ->
          { module_ with wrap_content = Ligo_prim.Module_expr.M_struct (module' ~inner:true x)}
        | _ -> module_
      in
      D_module { module_binder ; module_ ; module_attr }
    | D_type { type_binder ; type_expr ; type_attr = _ } ->
      let type_attr = TypeOrModuleAttr.{ public = false ; hidden = true } in
      D_type { type_binder ; type_expr ; type_attr }
    | D_value x ->
      let binder = sap_for_all x.binder in
      let binder = at_prefix binder in
      let attr : ValueAttr.t = { x.attr with inline = true ; hidden = true } in
      D_value { x with attr ; binder }
  and declaration ~inner (d : declaration) = Simple_utils.Location.map (f ~inner) d
  and decl ~inner d = declaration ~inner d
  and module' ~inner = List.map ~f:(decl ~inner) in

  List.map ~f:(declaration ~inner) ds

(* [get_aliases_prelude] [var] [typed_program] returns aliases for all non-private module and value declarations
   in [typed_program] accessed through [var].
  
  e.g. with [var] == Curry
    ```
    module A = <..>
    module B = struct
      module C = <..>
    end
    [@private] module D = <..>
    let foo = <..>
    ```
  we return
    ```
    module A = Curry.A
    module B = Curry.B
    let foo = Curry.foo
    ```
*)
let get_aliases_prelude : Ast_typed.module_variable -> Ast_typed.program -> Ast_core.program =
  fun mod_binder prg ->
    let get_mod_bindings acc d =
      let open Ast_typed in
      match Location.unwrap d with
      | D_module { module_binder ; module_attr = _ ; _ } (* when TypeOrModuleAttr.(module_attr.public) *) -> module_binder::acc
      | _ -> acc in
    let get_top_bindings acc d =
      let open Ast_typed in
      match Location.unwrap d with
      | D_value { binder ; attr = _ ; _ } (* when ValueAttr.(attr.public) *) -> binder::acc | _ -> acc in
    let module_attr = Ast_core.TypeOrModuleAttr.{ public = true ; hidden = true } in
    let attr = Ast_core.ValueAttr.
      { inline = false ; no_mutation = true ; view = false ;
        public = true ; hidden = true ; thunk = false }
    in
    let mod_bindings = List.(rev (fold prg ~init:[] ~f:get_mod_bindings)) in
    let top_level_bindings = List.(rev (fold prg ~init:[] ~f:get_top_bindings)) in
    let prelude_mod : Ast_core.program =
      let open Ast_core in
      let prelude = List.map mod_bindings ~f:(fun module_binder ->
        let module_ = Location.wrap @@ Ligo_prim.Module_expr.M_module_path (mod_binder,[module_binder]) in
        Location.wrap @@ D_module {module_binder ; module_attr ; module_ })
      in
      prelude
    in
    let prelude_top : Ast_core.program =
      let open Ast_core in
      let prelude = List.map top_level_bindings ~f:(fun binder ->
        let expr = make_e @@ E_module_accessor {module_path = [mod_binder] ; element = Ligo_prim.Binder.get_var binder } in
        Location.wrap @@ D_value { binder = Ligo_prim.Binder.set_ascr binder None ; attr ; expr })
      in
      prelude
    in
    prelude_mod @ prelude_top

(* [inject_declaration] [syntax] [module_] fetch expression argument passed through CLI options and inject a declaration `let cli_arg = [options.cli_expr_inj]`
         on top of an existing core program
*)
let inject_declaration ~options ~raise : Syntax_types.t -> Ast_core.program -> Ast_core.program = fun syntax prg ->
  let inject_arg_declaration arg =
    let open Ast_core in
    let expr = Ligo_compile.Utils.core_expression_string ~raise syntax arg in
    let attr = ValueAttr.{ inline = false ; no_mutation = true ; view = false ; public = false ; hidden = false ; thunk = false } in
    let d = Location.wrap @@ D_value { binder = make_binder (Ligo_prim.Value_var.of_input_var "cli_arg") ; expr ; attr } in
    d::prg
  in
  (Option.value_map Compiler_options.(options.test_framework.cli_expr_inj) ~default:prg ~f:inject_arg_declaration)

