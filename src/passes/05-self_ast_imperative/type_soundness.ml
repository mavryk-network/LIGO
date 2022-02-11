module Var = Simple_utils.Var
module Trace = Simple_utils.Trace
open Ast_imperative
open Errors
open Simple_utils.Trace

let predefined_data_constructor = ["Some" ; "None" ; "Unit"]

let linearity_type_parameters : raise:[<Errors.self_ast_imperative_error] Trace.raise -> type_expression -> unit =
  fun ~raise x ->
    match x.type_content with
    | T_abstraction {ty_binder ; type_ ; _ } ->
      let aux : type_expression -> type_variable list -> type_variable list = fun ty acc ->
        match ty.type_content with
        | T_abstraction x -> (x.ty_binder)::acc
        | _ -> acc
      in
      let lst = aux type_ [ty_binder] in
      if List.contains_dup ~compare:Var.compare lst then raise.raise (non_linear_type_decl x)
    | T_for_all {ty_binder ; type_ ; _ } ->
      let aux : type_expression -> type_variable list -> type_variable list = fun ty acc ->
        match ty.type_content with
        | T_for_all x -> (x.ty_binder)::acc
        | _ -> acc
      in
      let lst = aux type_ [ty_binder] in
      if List.contains_dup ~compare:Var.compare lst then raise.raise (non_linear_type_decl x)
    | _ -> ()

let linearity_rows : raise:[<Errors.self_ast_imperative_error] Trace.raise -> type_expression -> unit =
  fun ~raise x ->
    match x.type_content with
    | (T_sum {fields ; _} | T_record {fields ; _}) ->
      let lst = List.map ~f:(fun (Label l,_) ->l) fields in
      if List.contains_dup ~compare:String.compare lst then raise.raise (non_linear_row x)
    | _ -> ()

let predefined_data_constructor : raise:[<Errors.self_ast_imperative_error] Trace.raise -> type_expression -> unit =
  fun ~raise x ->
    match x.type_content with
    | T_sum {fields ; _} -> (
      let lst = List.map ~f:(fun (Label l,_) ->l) fields in
      match List.find_a_dup ~compare:String.compare (lst @ predefined_data_constructor) with
      | Some s -> raise.raise (reserved_name s x.location)
      | None -> ()
    )
    | _ -> ()


let checks_linearity : raise:[<Errors.self_ast_imperative_error] Trace.raise -> type_expression -> unit =
  fun ~raise x ->
    linearity_type_parameters ~raise x;
    linearity_rows ~raise x

let checks_predefined : raise:[<Errors.self_ast_imperative_error] Trace.raise -> type_expression -> unit =
  fun ~raise x ->
    predefined_data_constructor ~raise x

let predefined_names ~(raise:[<Errors.self_ast_imperative_error] Trace.raise) m = (fun x -> checks_predefined ~raise x ; x) m
let linearity ~(raise:[<Errors.self_ast_imperative_error] Trace.raise) m = (fun x -> checks_linearity ~raise x ; x) m