module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Trace
open Stage_common.Maps

let equal_var = Location.equal_content ~equal:Var.equal
(* let compare_var = Location.compare_content ~compare:Var.compare *)

let rec add_to_end (expression: O.expression) to_add =
  match expression.expression_content with
  | O.E_let_in lt ->
    let lt = {lt with let_result = add_to_end lt.let_result to_add} in
    {expression with expression_content = O.E_let_in lt}
  | O.E_sequence seq ->
    let seq = {seq with expr2 = add_to_end seq.expr2 to_add} in
    {expression with expression_content = O.E_sequence seq}
  | _ -> O.e_sequence expression to_add

and store_mutable_variable (free_vars : I.expression_variable list) =
  if (List.length free_vars == 0) then
    O.e_unit ()
  else
    let aux (var:I.expression_variable) = (O.Label (Var.to_name var.wrap_content), O.e_variable var) in
    O.e_record @@ O.LMap.of_list (List.map ~f:aux free_vars)

and restore_mutable_variable (expr : O.expression->O.expression) (free_vars : O.expression_variable list) (env : O.expression_variable) =
  let aux (f: O.expression -> O.expression) (ev: O.expression_variable) =
    fun expr -> f (O.e_let_in_ez ev true [] (O.e_accessor (O.e_variable env) [O.Access_record (Var.to_name ev.wrap_content)]) expr)
  in
  let ef = List.fold_left ~f:aux ~init:(fun e -> e) free_vars in
  fun e -> match e with
    | None -> expr (ef (O.e_skip ()))
    | Some e -> expr (ef e)


let rec compile_type_expression : I.type_expression -> (O.type_expression,Errors.purification_error) result =
  fun te ->
  let self = compile_type_expression in
  let return tc = ok @@ O.make_t ~loc:te.location tc in
  match te.type_content with
    | I.T_sum sum ->
      let* sum = rows self sum in
      return @@ O.T_sum sum
    | I.T_record record ->
      let* record = rows self record in
      return @@ O.T_record record
    | I.T_tuple tuple ->
      let* tuple = bind_map_list self tuple in
      return @@ O.T_tuple tuple
    | I.T_arrow arr ->
      let* arr = arrow self arr in
      return @@ T_arrow arr
    | I.T_variable type_variable -> return @@ T_variable type_variable
    | I.T_app {type_operator;arguments=[l;r]} when Var.equal Stage_common.Constant.v_michelson_or type_operator ->
      let* (l, l_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let* (r, r_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let* (l,r) = bind_map_pair compile_type_expression (l,r) in
      let sum : (O.label * _ O.row_element) list = [
        (O.Label "M_left" , {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (O.Label "M_right", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 1}); ]
      in
      return @@ O.T_sum { fields = O.LMap.of_list sum ; attributes = [] }
    | I.T_app {type_operator;arguments=[l;r]} when Var.equal Stage_common.Constant.v_michelson_pair type_operator ->
      let* (l, l_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let* (r, r_ann) = trace_option (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let* (l,r) = bind_map_pair compile_type_expression (l,r) in
      let sum : (O.label * _ O.row_element) list = [
        (O.Label "0", {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (O.Label "1", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 1}); ]
      in
      return @@ O.T_record { fields = (O.LMap.of_list sum) ; attributes = [] }
    | I.T_app c ->
      let* c = type_app self c in
      return @@ T_app c
    | I.T_module_accessor ma ->
      let* ma = module_access self ma in
      return @@ O.T_module_accessor ma
    | I.T_annoted (ty, _) -> self ty
    | I.T_singleton t -> return @@ O.T_singleton t


let rec compile_expression : I.expression -> (O.expression , _) result =
  fun e ->
  let* e = compile_expression' e in
  ok @@ e None

and compile_expression' : I.expression -> (O.expression option -> O.expression, Errors.purification_error) result =
  fun e ->
  let self = compile_expression in
  let self_type = compile_type_expression in
  let return' expr = ok @@ function
    | None -> expr
    | Some e -> O.e_sequence expr e
  in
  let return expr = return' @@ O.make_e ~loc:e.location expr in
  match e.expression_content with
    | I.E_literal literal   -> return @@ O.E_literal literal
    | I.E_constant {cons_name;arguments} ->
      let* arguments = bind_map_list compile_expression arguments in
      return' @@ O.e_constant ~loc:e.location (Stage_common.Types.const_name cons_name) arguments
    | I.E_variable name     -> return @@ O.E_variable name
    | I.E_application app ->
      let* app = application self app in
      return @@ O.E_application app
    | I.E_lambda lamb ->
      let* lamb = lambda self self_type lamb in
      return @@ O.E_lambda lamb
    | I.E_recursive recs ->
      let* recs = recursive self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;attributes;rhs;let_result} ->
      let* let_binder = binder self_type let_binder in
      let* rhs = self rhs in
      let* let_result = self let_result in
      return @@ O.E_let_in {let_binder;mut=false; attributes; rhs; let_result}
    | I.E_type_in ti ->
      let* ti = type_in self self_type ti in
      return @@ O.E_type_in ti
    | I.E_mod_in mi ->
      let* mi = mod_in self self_type mi in
      return @@ O.E_mod_in mi
    | I.E_mod_alias ma ->
      let* ma = mod_alias self ma in
      return @@ O.E_mod_alias ma
    | I.E_raw_code rc ->
      let* rc = raw_code self rc in
      return @@ O.E_raw_code rc
    | I.E_constructor const ->
      let* const = constructor self const in
      return @@ O.E_constructor const
    | I.E_matching m ->
      let* m = compile_matching m e.location in
      ok @@ m
    | I.E_record recd ->
      let* recd = record self recd in
      return @@ O.E_record recd
    | I.E_accessor acc ->
      let* acc = accessor self acc in
      return @@ O.E_accessor acc
    | I.E_update up ->
      let* up = update self up in
      return @@ O.E_update up
    | I.E_map map ->
      let* map = bind_map_list (
        bind_map_pair self
      ) map
      in
      return @@ O.E_map map
    | I.E_big_map big_map ->
      let* big_map = bind_map_list (
        bind_map_pair self
      ) big_map
      in
      return @@ O.E_big_map big_map
    | I.E_list lst ->
      let* lst = bind_map_list self lst in
      return @@ O.E_list lst
    | I.E_set set ->
      let* set = bind_map_list self set in
      return @@ O.E_set set
    | I.E_ascription ascr ->
      let* ascr = ascription self self_type ascr in
      return @@ O.E_ascription ascr
    | I.E_module_accessor ma ->
      let* ma = module_access self ma in
      return @@ O.E_module_accessor ma
    | I.E_cond {condition;then_clause;else_clause} ->
      let* condition    = self condition in
      let* then_clause' = self then_clause in
      let* else_clause' = self else_clause in
      return' @@ O.e_cond ~loc:e.location condition then_clause' else_clause'
    | I.E_sequence {expr1; expr2} ->
      let* expr1 = compile_expression' expr1 in
      let* expr2 = compile_expression' expr2 in
      ok @@ fun e -> (expr1 (Some (expr2 e))
        )
    | I.E_skip -> return @@ O.E_skip
    | I.E_tuple tuple ->
      let* tuple = bind_map_list self tuple in
      return @@ O.E_tuple tuple
    | I.E_assign {variable; access_path; expression} ->
      let* access_path = path self access_path in
      let* expression = self expression in
      let loc = e.location in
      let rhs = match access_path with
        [] -> expression
      | _  -> O.e_update ~loc (O.e_variable ~loc variable) access_path expression in
      return @@ E_assign {lvalue=variable; value=rhs}
    | I.E_for f ->
      let* f = compile_for f in
      ok @@ f
    | I.E_for_each fe ->
      let* fe = compile_for_each fe in
      ok @@ fe
    | I.E_while w ->
      let* w = compile_while w in
      ok @@ w

and compile_matching : I.matching -> Location.t -> (O.expression option -> O.expression, Errors.purification_error) result =
  fun {matchee;cases} loc ->
  let return expr = ok @@ function
    | None -> expr
    | Some e -> O.e_sequence expr e
  in
  let* matchee = compile_expression matchee in
  let aux = function ({pattern; body} : _ I.match_case) ->
              let* body = compile_expression body in
              let* pattern = Stage_common.Helpers.map_pattern_t (binder compile_type_expression) pattern in
              ok @@ ({pattern; body} : _ O.match_case) in
  let* l = bind_map_list aux cases in
  return (O.e_matching ~loc matchee l)

and compile_while I.{cond;body} =
  let env_rec = Location.wrap @@ Var.fresh ~name:"env_rec" () in
  let binder  = Location.wrap @@ Var.fresh ~name:"binder"  () in

  let* cond = compile_expression cond in
  let ctrl =
    (O.e_variable binder)
  in

  let* for_body = compile_expression body in
  let for_body = add_to_end for_body ctrl in

  let aux name expr=
    O.e_let_in_ez name false [] (O.e_accessor (O.e_variable binder) [Access_tuple Z.zero; Access_record (Var.to_name name.wrap_content)]) expr
  in
  let init_rec = O.e_tuple [store_mutable_variable @@ []] in
  let restore = fun expr -> List.fold_right ~f:aux [] ~init:expr in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [for_body] in
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable binder] in
  let aux_func =
    O.e_lambda_ez binder None @@
    restore @@
    O.e_cond cond continue_expr stop_expr in
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let return_expr = fun expr ->
    O.e_let_in_ez env_rec false [] init_rec @@
    O.e_let_in_ez env_rec false [] loop @@
    O.e_let_in_ez env_rec false [] (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  ok @@ restore_mutable_variable return_expr [] env_rec


and compile_for I.{binder;start;final;incr;f_body} =
  let env_rec = Location.wrap @@ Var.fresh ~name:"env_rec" () in
  let loop_binder = Location.wrap @@ Var.fresh ~name:"loop_binder" () in
  (*Make the cond and the step *)
  let cond = I.e_annotation (I.e_constant (Const C_LE) [I.e_variable binder ; final]) (I.t_bool ()) in
  let* cond = compile_expression cond in
  let* step = compile_expression incr in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [(O.e_variable loop_binder)] in
  let ctrl =
    O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] (O.e_constant C_ADD [ O.e_variable binder ; step ]) @@
    O.e_let_in_ez loop_binder false [] (O.e_update (O.e_variable loop_binder) [Access_tuple Z.one] @@ O.e_variable binder)@@
    continue_expr
  in
  (* Modify the body loop*)
  let* body = compile_expression f_body in
  let captured_name_list = [] in
  let for_body = add_to_end body ctrl in

  let aux name expr=
    O.e_let_in_ez name false [] (O.e_accessor (O.e_variable loop_binder) [Access_tuple Z.zero; Access_record (Var.to_name name.wrap_content)]) expr
  in

  (* restores the initial value of the free_var*)
  let restore = fun expr -> List.fold_right ~f:aux captured_name_list ~init:expr in

  (*Prep the lambda for the fold*)
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable loop_binder] in
  let aux_func = O.e_lambda_ez loop_binder None @@
                 O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] (O.e_accessor (O.e_variable loop_binder) [Access_tuple Z.one]) @@
                 O.e_cond cond (restore for_body) (stop_expr) in

  (* Make the fold_while en precharge the vakye *)
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let init_rec = O.e_pair (store_mutable_variable captured_name_list) @@ O.e_variable binder in

  let* start = compile_expression start in
  let return_expr = fun expr ->
    O.e_let_in_ez binder ~ascr:(O.t_int ()) false [] start @@
    O.e_let_in_ez env_rec false [] init_rec @@
    O.e_let_in_ez env_rec false [] loop @@
    O.e_let_in_ez env_rec false [] (O.e_accessor (O.e_variable env_rec) [Access_tuple Z.zero]) @@
    expr
  in
  ok @@ restore_mutable_variable return_expr captured_name_list env_rec

and compile_for_each I.{fe_binder;collection;collection_type; fe_body} =
  let env_rec = Location.wrap @@ Var.fresh ~name:"env_rec" () in
  let args    = Location.wrap @@ Var.fresh ~name:"args" () in

  let* body = compile_expression fe_body in
  let free_vars = [] in
  let for_body = add_to_end body @@ (O.e_accessor (O.e_variable args) [Access_tuple Z.zero]) in

  let init_record = store_mutable_variable free_vars in
  let* collect = compile_expression collection in
  let aux name expr=
    O.e_let_in_ez name false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.zero; Access_record (Var.to_name name.wrap_content)]) expr
  in
  let restore = fun expr -> List.fold_right ~f:aux free_vars ~init:expr in
  let restore = match collection_type with
    | Map -> (match snd fe_binder with
      | Some v -> fun expr -> restore (O.e_let_in_ez (fst fe_binder) false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero])
                                    (O.e_let_in_ez v false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.one]) expr))
      | None -> fun expr -> restore (O.e_let_in_ez (fst fe_binder) false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one; Access_tuple Z.zero]) expr)
    )
    | _ -> fun expr -> restore (O.e_let_in_ez (fst fe_binder) false [] (O.e_accessor (O.e_variable args) [Access_tuple Z.one]) expr)
  in
  let lambda = O.e_lambda_ez args None (restore for_body) in
  let* op_name = match collection_type with
   | Map -> ok @@ O.C_MAP_FOLD | Set -> ok @@ O.C_SET_FOLD | List -> ok @@ O.C_LIST_FOLD | Any -> ok @@ O.C_FOLD
  in
  let fold = fun expr ->
    O.e_let_in_ez env_rec false [] (O.e_constant op_name [lambda; collect ; init_record]) expr
  in
  ok @@ restore_mutable_variable fold free_vars env_rec

and compile_declaration : I.declaration Location.wrap -> _ =
  fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with
  | I.Declaration_type dt ->
    let* dt = declaration_type compile_type_expression dt in
    return @@ O.Declaration_type dt
  | I.Declaration_constant dc ->
    let* dc = declaration_constant compile_expression compile_type_expression dc in
    return @@ O.Declaration_constant dc
  | I.Declaration_module dm ->
    let* dm = declaration_module compile_expression compile_type_expression dm in
    return @@ O.Declaration_module dm
  | I.Module_alias ma ->
    let* ma = module_alias ma in
    return @@ O.Module_alias ma

and compile_module : I.module_ -> (O.module_ , Errors.purification_error) result = fun m ->
  module' compile_expression compile_type_expression m
