open Helpers
open Errors
open Ast_imperative
open Simple_utils.Trace
open Ligo_prim

let get_of m l =
  List.filter_map ~f:(fun v ->
      match List.find ~f:(Value_var.equal v) l with
      | Some d -> Some (Value_var.get_location d, v)
      | None -> None) m

let add_binder (b : _ Binder.t) vars =
  let var = Binder.get_var b in
  let vars = remove_from var vars in
  if Binder.is_mutable b then var :: vars else vars

let rec capture_expression ~raise : ?vars:Value_var.t list -> expression -> expression = fun ?(vars = []) e ->
  let self = capture_expression ~raise in
  let _ = fold_map_expression
                 (fun (vars : Value_var.t list) expr ->
                   match expr.expression_content with
                   | E_lambda {binder;output_type=_;result=_} ->
                      let fv_expr = Free_variables.expression expr in
                      let fv_expr = get_of fv_expr vars in
                      if not (List.is_empty fv_expr) then
                        raise.error @@ vars_captured fv_expr
                      else
                        let vars = add_binder binder vars in
                        (true, vars, expr)
                   | E_let_in {let_binder;rhs;let_result;attributes=_} ->
                      let _ = self ~vars rhs in
                      let vars = add_binder let_binder vars in
                      let _ = self ~vars let_result in
                      (false, vars, expr)
                   | E_matching {matchee;cases} ->
                      let f Match_expr.{pattern;body} =
                        let all_pattern_vars = get_pattern pattern in
                        let vars = List.fold_right ~f:remove_from all_pattern_vars ~init:vars in
                        let const_pattern_vars = get_pattern ~pred:Fun.id pattern in
                        let vars =
                          List.fold_right ~f:(fun var vars -> var :: remove_from var vars) const_pattern_vars ~init:vars in
                        self ~vars body in
                      let _ = self ~vars matchee in
                      let _ = List.map ~f:f cases in
                      (false, vars, expr)
                   | E_recursive {fun_name=_;fun_type=_;lambda={binder;output_type=_;result=_}} ->
                      let fv_expr = Free_variables.expression expr in
                      let fv_expr = get_of fv_expr vars in
                      if not (List.is_empty fv_expr) then
                        raise.error @@ vars_captured fv_expr
                      else
                        let vars = add_binder binder vars in
                        (true, vars, expr)
                   | _  ->
                      (true, vars, expr)
                   ) vars e in
  e

let capture_expression ~raise : expression -> expression =
  capture_expression ~raise
