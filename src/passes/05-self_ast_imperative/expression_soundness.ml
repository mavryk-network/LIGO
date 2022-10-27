module Var = Simple_utils.Var
module Trace = Simple_utils.Trace
open Ligo_prim
open Ast_imperative
open Errors
open Simple_utils.Trace

let is_pattern_linear p =
  List.contains_dup ~compare:(Binder.compare_var) (Pattern.binders p)

let check_linearity_record_fields ~raise : expression -> unit = fun exp ->
  match exp.expression_content with
  | E_record x ->
    if List.contains_dup ~compare:Label.compare @@ List.map ~f:fst x
      then raise.error (non_linear_record exp)
  | _ -> ()

let check_linearity_patterns ~raise : expression -> unit = fun exp ->
  match exp.expression_content with
  | E_let_in {let_binder;_}
  | E_let_mut_in {let_binder;_} ->
    if is_pattern_linear let_binder
    then raise.error (non_linear_pattern let_binder)
    else ()
  | E_matching x ->
    let _patterns = List.map ~f:(fun x -> x.pattern) x.cases in
    List.iter _patterns
      ~f:(fun p ->
        if is_pattern_linear p then raise.error (non_linear_pattern p)
      )
  | _ -> ()

let checks_linearity ~raise : expression -> unit =
  fun x ->
    check_linearity_record_fields ~raise x;
    check_linearity_patterns ~raise x;
    ()

let linearity_prg ~raise : program -> program =
  fun x ->
    let f : declaration -> unit = fun x ->
      match x.wrap_content with
      | D_pattern { pattern; expr; attr } ->
        if is_pattern_linear pattern
        then ()
        else raise.error (non_linear_pattern pattern)
      | D_value _ | D_type _ | D_module _ -> ()
    in
    let _ : unit list = List.map ~f x in
    x


let linearity ~raise m = (fun x -> checks_linearity ~raise x ; x) m
