(* MAVRYK: PascaLIGO. Decompiler Ast_unified -> Cst.Pascaligo.
   Only the LSP-critical folders are implemented: [ty_expr] (fully), [sig_expr] and
   [sig_entry] (for signature hover). Expression/statement/program decompilation is out of
   scope (LSP hover only needs types and signatures) and stays as [todo]. Mirrors
   src/passes/03-unification/cameligo/decompile.ml, adapted to PascaLIGO CST node shapes. *)

open Lexing_pascaligo.Token
module CST = Cst.Pascaligo
module AST = Ast_unified
module Helpers = Unification_shared.Helpers
module Utils = Simple_utils.Utils
module Location = Simple_utils.Location
module Label = Ligo_prim.Label
module Ty_escaped_var = Nano_prim.Ty_escaped_var
module Abstraction = Ligo_prim.Abstraction
module Abstractions = Ligo_prim.Abstractions

let w = Region.wrap_ghost
let ghost_tvar v = ghost_ident (Format.asprintf "%a" AST.Ty_variable.pp v)
let ghost_mvar v = ghost_uident (Format.asprintf "%a" AST.Mod_variable.pp v)
let decompile_mvar : AST.Mod_variable.t -> CST.module_name = ghost_mvar

let decompile_tvar : AST.Ty_variable.t -> CST.type_expr =
 fun t -> CST.T_Var (Var (ghost_tvar t))


let decompile_tvar_esc : Ty_escaped_var.t -> CST.type_expr = function
  | Ty_escaped_var.Raw v -> CST.T_Var (Var (ghost_tvar v))
  | Esc v -> CST.T_Var (Esc (ghost_tvar v))


let decompile_attr : AST.Attribute.t -> CST.attribute =
 fun { key; value } -> ghost_attr key (Option.map ~f:(fun x -> Attr.String x) value)


(* [A.B.field] module path, mirroring the CameLIGO decompiler. *)
let decompile_mod_path
    : type a. (AST.Mod_variable.t Nonempty_list.t, a) AST.Mod_access.t -> a CST.module_path
  =
 fun { module_path; field; field_as_open = _ } ->
  let module_path =
    Utils.nsepseq_of_ne_list ~sep:ghost_dot
    @@ Nonempty_list.map ~f:decompile_mvar module_path
  in
  CST.{ module_path; selector = ghost_dot; field }


(* Wrap a type in parentheses. *)
let parens : CST.type_expr -> CST.type_expr =
 fun t -> CST.T_Par (w CST.{ lpar = ghost_lpar; inside = t; rpar = ghost_rpar })


(* Parenthesise a type when used as an operand where it would otherwise re-associate.
   The rhs of an arrow keeps its own arrow un-parenthesised (right associativity). *)
let p : ?arrow_rhs:bool -> CST.type_expr -> CST.type_expr =
 fun ?(arrow_rhs = false) t ->
  let needs_parens : CST.type_expr -> bool = function
    | T_Fun _ | T_Cart _ | T_Sum _ | T_Union _ | T_Attr _ | T_ParameterOf _ -> true
    | T_App _ | T_Par _ | T_Var _ | T_Record _ | T_ModPath _ | T_Int _ | T_String _ ->
      false
  in
  if needs_parens t
  then
    if arrow_rhs
    then (
      match t with
      | T_Fun _ -> t
      | _ -> parens t)
    else parens t
  else t


let decompile_variant
    : AST.Label.t -> CST.type_expr option -> AST.Attribute.t list -> CST.variant
  =
 fun (AST.Label.Label (constr_name, _)) t attributes ->
  let ctor = ghost_ident constr_name in
  let ctor_args = Option.map ~f:(fun t -> ghost_of, p t) t in
  let attributes = List.map ~f:decompile_attr attributes in
  CST.{ ctor; ctor_args; attributes }


let decompile_field : AST.Label.t * CST.type_expr -> CST.field_decl CST.reg =
 fun (AST.Label.Label (field, _), t) ->
  w
    CST.
      { attributes = []
      ; field_name = Var (ghost_ident field)
      ; field_type = Some (ghost_colon, t)
      }


let record_of_fields : CST.field_decl CST.reg list -> CST.type_expr =
 fun fields ->
  let elements = Utils.list_to_sepseq fields ghost_semi in
  CST.(
    T_Record
      (w
         { kind = ghost_record
         ; opening = ghost_lbracket
         ; elements
         ; terminator = None
         ; closing = ghost_rbracket
         }))


let ty_expr : CST.type_expr AST.ty_expr_ -> CST.type_expr =
 fun te ->
  match Location.unwrap te with
  | T_attr (attr, t) -> CST.T_Attr (decompile_attr attr, t)
  | T_int (_s, z) -> CST.T_Int (ghost_int z)
  (* PascaLIGO has no type-level nat literal; render as an int. *)
  | T_nat (_s, z) -> CST.T_Int (ghost_int z)
  | T_string s -> CST.T_String (ghost_string s)
  (* PascaLIGO has no quoted type variables; render as a plain type variable. *)
  | T_arg s -> CST.T_Var (Var (ghost_ident s))
  | T_var t -> decompile_tvar t
  | T_var_esc t -> decompile_tvar_esc t
  | T_fun (_param_names, t1, t2) ->
    CST.T_Fun (w (p t1, ghost_arrow, p ~arrow_rhs:true t2))
  | T_prod (first :: rest) ->
    (match Utils.list_to_sepseq rest ghost_times with
    | Some nsepseq -> CST.T_Cart (w (first, ghost_times, nsepseq))
    | None -> first)
  | T_sum { fields; layout = _ } ->
    let f (constr, t) = w @@ decompile_variant constr (Some t) [] in
    (match Utils.list_to_sepseq (Core.Map.to_alist fields) ghost_vbar with
    | None -> failwith "Decompiler: got a T_sum with no elements"
    | Some nsepseq ->
      CST.T_Sum
        (w CST.{ lead_vbar = Some ghost_vbar; variants = Utils.nsepseq_map f nsepseq }))
  | T_record { fields; layout = _ } ->
    record_of_fields (List.map ~f:decompile_field (Core.Map.to_alist fields))
  | T_app { constr; type_args } ->
    let inside = Utils.nsepseq_of_ne_list ~sep:ghost_comma type_args in
    let tuple = w CST.{ lpar = ghost_lpar; inside; rpar = ghost_rpar } in
    CST.T_App (w (constr, tuple))
  | T_record_raw row ->
    let fields =
      List.map
        row
        ~f:(fun ( AST.Label.Label (field, _)
                , { associated_type; attributes; decl_pos = _ } ) ->
          w
            CST.
              { attributes = List.map ~f:decompile_attr attributes
              ; field_name = Var (ghost_ident field)
              ; field_type = Option.map ~f:(fun t -> ghost_colon, t) associated_type
              })
    in
    record_of_fields fields
  | T_sum_raw row ->
    let f (constr, AST.Non_linear_rows.{ associated_type; attributes; _ }) =
      w @@ decompile_variant constr associated_type attributes
    in
    (match Utils.list_to_sepseq (List.map ~f row) ghost_vbar with
    | None -> failwith "Decompiler: got a T_sum_raw with no fields"
    | Some nsepseq ->
      CST.T_Sum (w CST.{ lead_vbar = Some ghost_vbar; variants = nsepseq }))
  | T_module_open_in { module_path; field; field_as_open } ->
    CST.T_ModPath
      (w
      @@ decompile_mod_path
           { module_path = [ module_path ]; field = parens field; field_as_open })
  | T_module_access { module_path; field; field_as_open } ->
    CST.T_ModPath
      (w
      @@ decompile_mod_path { module_path; field = decompile_tvar field; field_as_open })
  | T_union summands ->
    (* MAVRYK: PascaLIGO. PascaLIGO now emits T_union too (anonymous union types). *)
    (match Utils.list_to_sepseq summands ghost_vbar with
     | None -> failwith "Decompiler: got a T_union with no members"
     | Some nsepseq -> CST.T_Union (w nsepseq))
  | T_named_fun _ -> failwith "Decompiler: named arguments should appear only in JsLIGO"
  | T_contract_parameter x ->
    CST.T_ParameterOf
      (w
         (Utils.nsepseq_of_ne_list
            (Nonempty_list.map ~f:decompile_mvar x)
            ~sep:ghost_dot))
  (* PascaLIGO has no [forall] type; drop the quantifiers, keep the inner type (LSP display). *)
  | T_abstraction Abstraction.{ type_; _ } | T_for_all Abstraction.{ type_; _ } -> type_
  | T_for_alls Abstractions.{ type_; _ } -> type_
  | T_module_app _ | T_constant _ ->
    Helpers.failwith_not_initial_node_decompiler @@ `Ty_expr te


let sig_expr
    :  (CST.signature_expr, CST.sig_item, CST.type_expr) AST.sig_expr_
    -> CST.signature_expr
  = function
  | { wrap_content = AST.S_body sig_items; _ } ->
    CST.(S_Sig (w { kwd_sig = ghost_sig; sig_items; kwd_end = ghost_end }))
  | { wrap_content = AST.S_path lst; _ } ->
    let lst = Nonempty_list.map ~f:decompile_mvar lst in
    let (last :: rev) = Nonempty_list.reverse lst in
    (match rev with
    | [] -> CST.S_Var last
    | hd :: tl ->
      let module_path =
        Utils.nsepseq_of_ne_list ~sep:ghost_dot (Nonempty_list.reverse (hd :: tl))
      in
      CST.S_Path (w CST.{ module_path; selector = ghost_dot; field = last }))


let sig_entry
    :  (CST.signature_expr, CST.sig_item, CST.type_expr) AST.sig_entry_
    -> CST.sig_item
  = function
  | { wrap_content = AST.S_value (v, ty, _optional); _ } ->
    CST.Sig_Value
      (w
         CST.
           { kwd_const = ghost_const
           ; var = Var (ghost_ident (Format.asprintf "%a" AST.Variable.pp v))
           ; colon = ghost_colon
           ; val_type = ty
           })
  | { wrap_content = AST.S_type (v, _params, ty); _ } ->
    CST.Sig_Type
      (w CST.{ kwd_type = ghost_type; name = Var (ghost_tvar v); type_rhs = Some (ghost_is, ty) })
  | { wrap_content = AST.S_type_var v; _ } ->
    CST.Sig_Type (w CST.{ kwd_type = ghost_type; name = Var (ghost_tvar v); type_rhs = None })
  | { wrap_content = AST.S_include se; _ } ->
    CST.Sig_Include (w CST.{ kwd_include = ghost_include; signature_expr = se })
  | { wrap_content = AST.S_attr (attr, si); _ } ->
    CST.Sig_Attr (w (decompile_attr attr, si))


let rec folder =
  let todo _ = failwith ("TODO" ^ __LOC__) in
  AST.Catamorphism.
    { expr = todo
    ; ty_expr
    ; pattern = todo
    ; statement = todo
    ; block = todo
    ; mod_expr = todo
    ; instruction = todo
    ; declaration = todo
    ; program_entry = todo
    ; program = todo
    ; sig_expr
    ; sig_entry
    }


and decompile_program p = AST.Catamorphism.cata_program ~f:folder p
and decompile_expression e = AST.Catamorphism.cata_expr ~f:folder e
and decompile_type_expression e = AST.Catamorphism.cata_ty_expr ~f:folder e
and decompile_pattern p = AST.Catamorphism.cata_pattern ~f:folder p
and decompile_sig_expr s = AST.Catamorphism.cata_sig_expr ~f:folder s
