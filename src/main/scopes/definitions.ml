open Ligo_prim
open Simple_utils
module AST = Ast_core
module VVar = Value_var
module TVar = Type_var
module MVar = Module_var
module LSet = Types.LSet

type t = Types.def list

let get_location_of_module_path : Module_var.t list -> Location.t =
 fun mvs ->
  List.fold mvs ~init:Location.dummy ~f:(fun loc m ->
      Location.cover loc (Module_var.get_location m))


(**
    Add a variable definition to the provided list of definitions `t`,
    using the information provided in the given `vvar`.

    @param body The expression bound to the given variable.
    @return The provided list of definitions augmented with the given variable.
    *)
let add_vvar ~(body : AST.expression) : VVar.t -> t -> t =
 fun vvar acc ->
  if VVar.is_generated vvar
  then acc
  else
    let open Types in
    let name = get_binder_name vvar in
    let vdef : vdef =
      let name : string = name in
      let uid : string = Types.make_def_id name (VVar.get_location vvar) in
      let range : Location.t = VVar.get_location vvar in
      let body_range : Location.t =
        match body.expression_content with
        (* For [E_recursive], we have to dig into [r.lambda.result] to get the real body range
             because otehrwise [body.location] will just return the "rec" keyword's range,
             for some reason *)
        | E_recursive r -> r.lambda.result.location
        | _ -> body.location
      in
      let t : type_case = Unresolved (* Filled in a later pass *) in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      let def_type : def_type = Local in
      { name; uid; range; body_range; t; references; def_type }
    in
    Variable vdef :: acc


(**
    Add a variable definition to the provided list of definitions `t`,
    using the information provided in the given `binder`.

    It's a wrapper over {!add_vvar}, calling it with the binder's extracted vvar.

    @param body The expression bound to the given variable.
    @return The provided list of definitions augmented with the given binder.
    *)
let add_binder ~(body : AST.expression) : _ Binder.t -> t -> t =
 fun binder acc -> add_vvar ~body (Binder.get_var binder) acc


(**
    Add a type variable definition to the provided list of definitions `t`,
    using the information provided in the given `tvar`.

    @param bindee The type expression bound to the given type variable.
    @return The provided list of definitions augmented with the given type variable.
    *)
let add_tvar ~(bindee : Ast_core.type_expression) : TVar.t -> t -> t =
 fun tvar acc ->
  if TVar.is_generated tvar
  then acc
  else
    let open Types in
    let name = get_type_binder_name tvar in
    let tdef : tdef =
      let name : string = name in
      let uid : string = Types.make_def_id name (TVar.get_location tvar) in
      let range : Location.t = TVar.get_location tvar in
      let body_range : Location.t = bindee.location (* How to get this ? *) in
      let content : Ast_core.type_expression = bindee in
      let def_type : def_type = Local in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      { name; uid; range; body_range; content; def_type; references }
    in
    Type tdef :: acc


(**
    Add a module variable definition to the provided list of definitions `t`,
    using the information provided in the given `mvar`.

    @param bindee The module expression bound to the given module variable.
    @return The provided list of definitions augmented with the given module variable.
    *)
let add_mvar ~(bindee : Ast_core.module_expr) ~(mod_case : Types.mod_case)
    : MVar.t -> t -> t
  =
 fun mvar acc ->
  if MVar.is_generated mvar
  then acc
  else
    let open Types in
    let name = get_mod_binder_name mvar in
    let mdef : mdef =
      let name : string = name in
      let uid : string = Types.make_def_id name (MVar.get_location mvar) in
      let range : Location.t = MVar.get_location mvar in
      let body_range : Location.t =
        match Location.unwrap bindee with
        | M_struct _ -> Location.get_location bindee
        | M_variable mvar -> MVar.get_location mvar
        | M_module_path mpath -> get_location_of_module_path @@ List.Ne.to_list mpath
      in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      let mod_case : mod_case = mod_case in
      let def_type : def_type = Local in
      { name; uid; range; body_range; references; mod_case; def_type }
    in
    Module mdef :: acc


let mod_case_of_mod_expr
    : defs_of_decls:(AST.declaration list -> t -> t) -> AST.module_expr -> Types.mod_case
  =
 fun ~defs_of_decls mod_expr ->
  let alias_of_mvars : Module_var.t list -> Types.mod_case =
   fun mvars ->
    let path = List.map ~f:(fun mvar -> Format.asprintf "%a" MVar.pp mvar) mvars in
    Types.Alias path
  in
  match Location.unwrap mod_expr with
  | M_struct decls -> Def (defs_of_decls decls [])
  | M_variable mod_var -> alias_of_mvars [ mod_var ]
  | M_module_path mod_path -> alias_of_mvars @@ List.Ne.to_list mod_path


(**
    This module contains the functions traversing the {!Ast_core}
    to fetch its definitions.

    During the traversal, some fields will be
    left blank or filled with a dummy value,
    they are meant to be filled in later passes.

*)
module Of_Ast = struct
  (**
    Options specifying which parts of the AST should not be traversed.

    By default, the whole AST should be traversed.

    The user, however, can provide a custom value with some fields set to [true]
    in order to perform a custom AST-traversal without traversing certain specific nodes.
    *)
  module Waivers = struct
    type t =
      { (* Useful for Stdlib AST traversal, when declaration rhs are unwanted *)
        d_value_expr : bool
      ; d_type_expr : bool
      ; d_irrefutable_match_expr : bool
      }

    let default : t =
      { d_value_expr = false; d_type_expr = false; d_irrefutable_match_expr = false }


    let of_opt : t option -> t = function
      | Some t -> t
      | None -> default


    (** Takes a function [f] and returns a wrapper function which :
        - Takes an optional [unless] boolean argument
          (defaults to [false])
        - Returns [f] if [unless] is [false]
        - Returns the identity function if [unless] is [true]

        It is meant for wrapping AST-traversal functions without re-implementing the [unless] logic each time.
        *)
    let wrap_with_unless (type x acc) (f : x -> acc -> acc) =
      fun ?(unless = false) -> if unless then fun _ acc -> acc else f
  end

  let linear_pattern ~(body : AST.expression)
      : AST.type_expression option Linear_pattern.t -> t -> t
    =
   fun ptrn acc ->
    let ptrn_binders = AST.Pattern.binders ptrn in
    let f defs binder = add_binder ~body binder defs in
    let defs = List.fold ~init:acc ~f ptrn_binders in
    defs


  let rec expression ~(waivers : Waivers.t) : AST.expression -> t -> t =
   fun e acc ->
    let self = Waivers.wrap_with_unless @@ expression ~waivers in
    let declarations = Waivers.wrap_with_unless @@ declarations ~waivers in
    let defs_of_lambda : _ Lambda.t -> t -> t =
     fun { binder; output_type = _; result } acc ->
      let vvar = Param.get_var binder in
      self result @@ add_vvar ~body:result vvar @@ acc
    in
    match e.expression_content with
    (* Base *)
    | E_variable _ -> acc
    | E_literal _ -> acc
    | E_constant _ -> acc
    | E_application { lamb; args } -> self lamb @@ self args @@ acc
    | E_lambda lambda -> defs_of_lambda lambda acc
    | E_recursive { fun_name = _; fun_type = _; lambda; force_lambdarec = _ } ->
      (* fun_name is already added by the parent E_let_in so don't need to add it here *)
      defs_of_lambda lambda acc
    | E_type_abstraction { type_binder = _; result } -> self result acc
    | E_let_in { let_binder; rhs; let_result; attributes = _ }
    | E_let_mut_in { let_binder; rhs; let_result; attributes = _ } ->
      linear_pattern ~body:rhs let_binder @@ self rhs @@ self let_result @@ acc
    | E_type_in { type_binder; rhs; let_result } ->
      add_tvar ~bindee:rhs type_binder @@ self let_result @@ acc
    | E_mod_in { module_binder; rhs; let_result } ->
      let mod_case = mod_case_of_mod_expr ~defs_of_decls:declarations rhs in
      add_mvar ~mod_case ~bindee:rhs module_binder @@ self let_result @@ acc
    | E_raw_code _ -> []
    (* Variant *)
    | E_constructor { constructor = _; element } -> self element acc
    | E_matching { matchee; cases } ->
      let defs_of_match_cases cases acc =
        let defs_of_match_case acc ({ pattern; body } : _ AST.Match_expr.match_case) =
          linear_pattern ~body pattern @@ self body @@ acc
        in
        List.fold ~init:acc ~f:defs_of_match_case cases
      in
      defs_of_match_cases cases @@ self matchee @@ acc
    (* Record *)
    | E_record r -> Record.fold ~init:acc ~f:(fun acc entry -> self entry acc) r
    | E_accessor { struct_; path = _ } ->
      self struct_ acc (* Is it possible to have decl in there ? *)
    | E_update { struct_; path = _; update } -> self struct_ @@ self update @@ acc
    (* Advanced *)
    | E_ascription { anno_expr; type_annotation = _ } -> self anno_expr acc
    | E_module_accessor _ -> acc
    (* Imperative *)
    | E_assign { binder = _; expression } ->
      (* binder := new_value, the binder is already declared so we don't add it to the dec list *)
      self expression acc
    | E_for { binder; start; final; incr; f_body } ->
      add_vvar ~body:f_body binder
      @@ self start
      @@ self final
      @@ self incr
      @@ self f_body
      @@ acc
    | E_for_each
        { fe_binder = vvar1, vvar2_opt; collection; collection_type = _; fe_body } ->
      let body = fe_body in
      let acc =
        match vvar2_opt with
        | Some vvar -> add_vvar ~body vvar acc
        | None -> acc
      in
      self fe_body @@ self collection @@ add_vvar ~body vvar1 @@ acc
    | E_while { cond; body } -> self cond @@ self body @@ acc


  and declaration ~(waivers : Waivers.t) : AST.declaration -> t -> t =
   fun decl acc ->
    let expression = Waivers.wrap_with_unless @@ expression ~waivers in
    let declarations = Waivers.wrap_with_unless @@ declarations ~waivers in
    match Location.unwrap decl with
    | D_value { binder; expr; attr = _ } ->
      add_binder ~body:expr binder @@ expression ~unless:waivers.d_value_expr expr @@ acc
    | D_irrefutable_match { pattern; expr; attr = _ } ->
      linear_pattern ~body:expr pattern
      @@ expression ~unless:waivers.d_irrefutable_match_expr expr
      @@ acc
    | D_type { type_binder; type_expr; type_attr = _ } ->
      add_tvar ~bindee:type_expr type_binder acc
    | D_module { module_binder; module_; module_attr = _ } ->
      (* Here, the module body's defs are within the lhs_def,
         mod_case_of_mod_expr recursively calls declaration *)
      let mod_case : Types.mod_case =
        mod_case_of_mod_expr ~defs_of_decls:declarations module_
      in
      add_mvar ~mod_case ~bindee:module_ module_binder @@ acc


  and declarations ~(waivers : Waivers.t) : AST.declaration list -> t -> t =
   fun decls acc ->
    List.fold ~init:acc ~f:(fun accu decl -> declaration ~waivers decl accu) decls


  let program ?(waivers = Waivers.default) : AST.program -> t -> t =
   fun prg acc -> declarations prg acc ~waivers
end

module Of_Stdlib_Ast = struct
  let program : AST.program -> t =
   fun prg ->
    let waivers =
      { Of_Ast.Waivers.default with d_value_expr = true; d_irrefutable_match_expr = true }
    in
    Of_Ast.program ~waivers prg []
end
