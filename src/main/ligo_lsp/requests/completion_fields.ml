(* Completions for things that come after dot, like [List.le] or [person.na]*)
(* TODO: we should handle field completion using ast_typed rather than scopes *)

open Lsp_helpers
module Utils = Simple_utils.Utils
module Fold = Cst_shared.Fold

type ('module_expr, 'module_type_expr) expr_kind =
  | Module_path_expr of 'module_expr
  | Module_path_type_expr of 'module_type_expr

type lexeme = Cst_shared.Types.lexeme
type 'a wrap = 'a Cst_shared.Types.wrap
type dot = lexeme wrap

type 'a fold_instruction =
  ( Cst_cameligo.Fold.some_node
  , Cst_jsligo.Fold.some_node
  , 'a Fold.fold_control )
  Dialect_cst.from_dialect

module type Compatible_CST = sig
  type cst
  type expr
  type type_expr
  type projection
  type selection (* Note: this selection must include the dot (as in JsLIGO) *)
  type 'a module_path

  val expr_to_region : expr -> Region.t
  val fold_map_cst : 'a Fold.monoid -> 'a fold_instruction -> cst -> 'a
  val expr_of_projection : projection -> expr
  val try_get_projection : expr -> projection option
  val dot_of_selection : selection -> dot option
  val lexeme_of_selection : selection -> lexeme option
  val selections_of_projection : projection -> selection Utils.nseq

  val lexemes_of_module_path
    :  (expr module_path, type_expr module_path) expr_kind
    -> lexeme wrap list
end

module C_CameLIGO : Compatible_CST with type cst = Cst_cameligo.CST.t = struct
  include Cst_cameligo.CST

  type nonrec selection = dot * selection

  let fold_map_cst monoid instruction =
    let open Dialect_cst in
    Cst_cameligo.Fold.fold_map_cst monoid instruction.cameligo


  let expr_of_projection node = node.record_or_tuple

  let try_get_projection = function
    | E_Proj proj -> Some proj.value
    | _ -> None


  let dot_of_selection (dot, _expr) = Some dot

  let lexeme_of_selection (_dot, expr) =
    expr
    |> function
    | FieldName v ->
      (match v with
      | Var name -> Some name#payload
      | Esc name -> Some ("@" ^ name#payload))
    | Component _ -> None


  let selections_of_projection node =
    let hd, tl = node.field_path in
    (node.selector, hd), tl


  let lexemes_of_module_path = function
    | Module_path_expr node -> Utils.nsepseq_to_list node.module_path
    | Module_path_type_expr node -> Utils.nsepseq_to_list node.module_path
end

module C_JsLIGO : Compatible_CST with type cst = Cst_jsligo.CST.t = struct
  include Cst_jsligo.CST

  type 'a module_path = 'a namespace_path

  let fold_map_cst monoid instruction =
    let open Dialect_cst in
    Cst_jsligo.Fold.fold_map_cst monoid instruction.jsligo


  let expr_of_projection node = node.object_or_array

  let try_get_projection = function
    | E_Proj proj -> Some proj.value
    | _ -> None


  let dot_of_selection = function
    | PropertyName (dot, _) -> Some dot
    | _ -> None


  let lexeme_of_selection = function
    | PropertyName (_dot, v) ->
      (match v with
      | Var name -> Some name#payload
      | Esc name -> Some ("@" ^ name#payload))
    | PropertyStr _ -> None
    | Component _ -> None


  let selections_of_projection node = node.property_path

  let lexemes_of_module_path = function
    | Module_path_expr (node : _ namespace_path) ->
      Utils.nsepseq_to_list node.namespace_path
    | Module_path_type_expr (node : _ namespace_path) ->
      Utils.nsepseq_to_list node.namespace_path
end

let complete_fields
    (type a)
    (module C : Compatible_CST with type cst = a)
    (cst : a)
    (path : Path.t)
    (syntax : Syntax_types.t)
    (pos : Position.t)
    (definitions : Def.t list)
    : CompletionItem.t list
  =
  failwith "LETS SUPPOSE I HAVE AN IMPLEMENTATION HERE"


let complete_fields
    :  Dialect_cst.t -> Path.t -> Syntax_types.t -> Position.t -> Def.t list
    -> CompletionItem.t list
  = function
  | CameLIGO cst -> complete_fields (module C_CameLIGO) cst
  | JsLIGO cst -> complete_fields (module C_JsLIGO) cst
