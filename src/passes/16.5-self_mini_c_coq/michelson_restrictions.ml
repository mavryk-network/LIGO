open Errors
open Simple_utils.Trace

module I = Ligo_coq_ocaml.Compiler
module Micheline = Tezos_micheline.Micheline

(* TODO: make it shared *)
type meta = Mini_c.meta
type bynder_meta = Mini_c.binder_meta
type base_type = (meta, string) Micheline.node
type oty = (meta, base_type) I.ty
type expr = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.expr
type binds = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.binds
type args = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.args


let self_in_lambdas ~raise : expr -> expr =
  fun e ->
    match e with
    | I.E_lam (_, I.Binds (_, _, body), _) ->
      let f = fun ~raise e -> match e with
				| I.E_inline_michelson (_, _, [Micheline.Prim (_, "SELF", _, _)], _) ->
           raise.error (bad_self_address Ligo_prim.Constant.C_SELF)
				| _ -> e
			in
      let _self_in_lambdas : expr = Helpers.map_expr (f ~raise) body in
      e
    | _ -> e
