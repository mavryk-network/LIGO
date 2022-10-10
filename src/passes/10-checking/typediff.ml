(**
    This modules aims at finding the difference between two large record types
    for improved error message during type mismatch.
    For example when type [a * b * c * d * e] cannot unify with [c * d * e],
    the module will find the "diff" between the two, [a * b] here,
    so the message can be augmented into :
    > Cannot unify a * b * c * d * e with c * d * e
    > Diff : a * b
*)

module Location = Simple_utils.Location
open Simple_utils.Trace
open Ast_typed
open Ligo_prim
module ValueMap = Simple_utils.Map.Make (Value_var)
module TypeMap = Ast_typed.Helpers.IdMap.Make (Type_var)
module ModuleMap = Ast_typed.Helpers.IdMap.Make (Module_var)
open Ast_typed
open Ligo_prim

type t =
| Missing_prefix of type_expression
| Missing_suffix of type_expression
| DummyTypeDiff of string
(* | Missing_infix type_expression *)
| NoDiff

let get_diff : type_expression -> type_expression -> t = fun t1 t2 ->
  match t1, t2 with
  | _ -> DummyTypeDiff "TODO NP add useful type diff"

let pp ppf (td : t) : unit = match td with
| Missing_prefix te -> Format.fprintf ppf "@.Diff: missing prefix %a" Ast_typed.PP.type_expression te
| Missing_suffix te -> Format.fprintf ppf "@.Diff: missing suffix %a" Ast_typed.PP.type_expression te
| DummyTypeDiff s   -> Format.fprintf ppf "@.Diff: %s" s
| NoDiff                     -> Format.fprintf ppf ""
