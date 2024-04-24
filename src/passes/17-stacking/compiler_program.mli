open Ligo_prim
open Mavryk_micheline.Micheline

type meta = Mini_c.meta

type compiled_expression =
  { expr_ty : (meta, string) node
  ; expr : (meta, string) node
  }

open Ligo_coq_ocaml.Compiler

(* TODO ugh *)
(* type ('meta, 'base_type, 'op, 'lit, 'static_args, 'micheline) expr *)

val compile_expr
  :  (meta, (meta, string) Mavryk_micheline.Micheline.node) ty list
  -> (meta, (meta, string) node, Literal_value.t, (meta, string) node) expr
  -> (meta, string) node Lwt.t

val compile_function_body
  :  (meta, (meta, string) node, Literal_value.t, (meta, string) node) binds
  -> (meta, string) node Lwt.t
