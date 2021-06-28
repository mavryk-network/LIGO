open Helpers
open Ast_typed
open Trace

let equal_var = Location.equal_content ~equal:Var.equal
let compare_var = Location.compare_content ~compare:Var.compare

let e_sequence ?(name="daca") (expr1 : expression) (expr2 : expression) : expression =
  e_a_let_in (Location.wrap (Var.fresh ~name ())) expr1 expr2 false

let to_label (v : expression_variable) =
  Label (Var.to_name v.wrap_content)

let rec lift_to_module : _ -> module_fully_typed -> (module_fully_typed, 'err) result = fun f (Module_Fully_Typed p) ->
  let aux = fun (x : declaration) ->
    let return (d : declaration) = ok @@ d in
    match x with
    | Declaration_constant {name; binder; expr ; inline} -> (
        let* expr = f expr in
        return @@ Declaration_constant {name; binder; expr ; inline}
    )
    | Declaration_type t -> return @@ Declaration_type t
    | Declaration_module {module_binder;module_} ->
      let* module_ = lift_to_module f module_ in
      return @@ Declaration_module {module_binder; module_}
    | Module_alias _ -> return x
  in
  let* p = bind_map_list (bind_map_location aux) p in
  ok @@ Module_Fully_Typed p

let has_assign : expression -> (bool, _) result =
  fun e -> let* b, _ = fold_map_expression (fun b e ->
                           match e.expression_content with
                           | E_assign _ ->
                              ok @@ (false, true, e)
                           | _ ->
                              ok @@ (true, b, e)) false e in
           ok @@ b

type env_type = (expression_variable * type_expression) list

let rec sp_expression : expression -> (expression , _) result =
  fun e ->
  let* b = has_assign e in
  if b then
    begin
      match e.expression_content with
      | E_lambda { binder ; result } ->
         let* result,_ = sp_expression' result in
         let result = make_e (e_record_accessor result (Label "0")) e.type_expression in
         ok @@ make_e (E_lambda { binder ; result }) e.type_expression
      | _ ->
         let* e, _ = sp_expression' e in
         ok @@ make_e (e_record_accessor e (Label "0")) e.type_expression
    end
  else
    ok e

and sp_expression' : expression -> (expression * env_type, _) result =
  fun e ->
  let pair_env env_var expr = (e_record (LMap.of_list [(Label "0", expr); (Label "1", env_var)])) in
  let return ?(fv = []) env_var expr = ok @@ (make_e ~location:e.location (pair_env env_var expr) (t_pair e.type_expression env_var.type_expression), fv) in
  match e.expression_content with
  | E_literal _ ->
     return (e_a_record ~layout:L_comb @@ LMap.of_list []) @@ e
  | E_variable _ ->
     return (e_a_record ~layout:L_comb @@ LMap.of_list []) @@ e
  | E_assign {lvalue;value} ->
     return ~fv:[(lvalue, value.type_expression)] (e_a_record ~layout:L_comb @@ LMap.of_list [(to_label lvalue, value)]) e_a_unit
  | E_let_in {let_binder;rhs;let_result;inline=_} ->
     (* Recurse on RHS and LET_RESULT *)
     let* rhs = sp_expression' rhs in
     let* let_result = sp_expression' let_result in
     (* Make final expression *)
     bind rhs (fun (p_rhs, p_rhs_type, rhs_vars) ->
     let v_rhs = e_a_variable p_rhs p_rhs_type in
     let* result, result_vars = bind let_result (fun (p_let_result, p_let_result_type, let_result_vars) ->
                                let v_let_result = e_a_variable p_let_result p_let_result_type in
                                let p_env = Location.wrap (Var.fresh ~name:"p_ENV" ()) in
                                let all_vars, p_env_type = type_of_vars @@ rhs_vars @ let_result_vars in
                                let v_env = e_a_variable p_env p_env_type in
                                let result = make_pair (make_access v_let_result "0") v_env in
                                let result = e_a_let_in p_env (make_e (e_record (LMap.of_list (List.map ~f:(fun (v, t) -> ((to_label v), e_a_variable v t)) all_vars))) p_env_type) result false in
                                ok @@ (result, all_vars)) in
     ok @@ (e_a_let_in let_binder (make_access v_rhs "0") result false, result_vars))
  | E_matching {matchee;cases=Match_variant {tv;cases}} ->
     let body_type = e.type_expression in
     let* matchee = sp_expression' matchee in
     bind matchee (fun (p_matchee, p_matchee_type, matchee_vars) ->
     let v_matchee = e_a_variable p_matchee p_matchee_type in
     let aux {constructor;pattern;body} =
       let* body, body_vars = sp_expression' body in
       ok @@ ({constructor;pattern;body}, body_vars) in
     let* cases_and_vars = bind_map_list aux cases in
     let cases_vars = List.concat @@ List.map ~f:snd cases_and_vars in
     let cases_vars, cases_env_type = type_of_vars @@ cases_vars in
     (* let cases = List.map ~f:fst cases_and_vars in *)
     let all_vars, p_env_type = type_of_vars @@ cases_vars @ matchee_vars in
     let init_env = (make_e (e_record (LMap.of_list (List.map ~f:(fun (v, t) -> ((to_label v), e_a_variable v t)) all_vars))) p_env_type) in
     bind' init_env (fun (p_env, _) ->
     let v_env = e_a_variable p_env p_env_type in
     let aux ({constructor;pattern;body}, body_vars) : matching_content_case =
       let p_branch = Location.wrap (Var.fresh ~name:"p_BRANCH" ()) in
       let p_branch_type = body.type_expression in
       let v_branch = e_a_variable p_branch p_branch_type in
       let new_env = make_e (e_record (LMap.of_list (List.map ~f:(fun (v, t) -> ((to_label v),
            if List.mem (List.map ~f:fst body_vars) v ~equal:equal_var then make_access (make_access v_branch "1") (Var.to_name v.wrap_content) else e_a_variable v t)) all_vars))) p_env_type in
       let body = e_a_let_in p_branch body
                             (make_pair (make_access v_branch "0")
                                        new_env) false in
       {constructor;pattern;body} in
     let cases = List.map ~f:aux cases_and_vars in
     let match_type = t_pair body_type cases_env_type in
     let match_expr = make_e (e_matching (make_access v_matchee "0") (Match_variant {tv;cases})) match_type in
     bind (match_expr, cases_vars) (fun (p_match, p_match_type, _) ->
     let v_match = e_a_variable p_match p_match_type in
     let result = make_pair (make_access v_match "0") v_env in
     let result = e_a_let_in p_env (make_e (e_record (LMap.of_list (List.map ~f:(fun (v, t) -> ((to_label v), e_a_variable v t)) all_vars))) p_env_type) result false in
     ok @@ (result, all_vars))))
  | E_matching {matchee;cases=Match_record {tv;fields;body}} ->
     let* matchee = sp_expression' matchee in
     bind matchee (fun (p_matchee, p_matchee_type, matchee_vars) ->
     let v_matchee = e_a_variable p_matchee p_matchee_type in
     let* body, body_vars = sp_expression' body in
     let match_expr = make_e (e_matching (make_access v_matchee "0") (Match_record {tv;fields;body})) body.type_expression in
     bind (match_expr, body_vars) (fun (p_match, p_match_type, _) ->
     let v_match = e_a_variable p_match p_match_type in
     let p_env = Location.wrap (Var.fresh ~name:"p_ENV" ()) in
     let all_vars, p_env_type = type_of_vars @@ body_vars @ matchee_vars in
     let v_env = e_a_variable p_env p_env_type in
     let result = make_pair (make_access v_match "0") v_env in
     let result = e_a_let_in p_env (make_e (e_record (LMap.of_list (List.map ~f:(fun (v, t) -> ((to_label v), e_a_variable v t)) all_vars))) p_env_type) result false in
     ok @@ (result, all_vars)))
  | E_lambda { binder ; result } ->
    let* result,_ = sp_expression' result in
    ok @@ (make_e (E_lambda { binder ; result }) e.type_expression, [])
  | _ ->
     return e_a_unit @@ e

and prefix_with_env : expression -> env_type -> expression -> expression =
  fun env_expr upd_vars expr ->
  List.fold_right upd_vars ~f:(fun (v,t) e ->
      e_a_let_in v
        (make_e (e_record_accessor env_expr (to_label v)) t)
        e false) ~init:expr

and type_of_vars : (expression_variable * type_expression) list -> _ =
  fun l ->
  let upd_vars = List.dedup_and_sort ~compare:(fun (v1, _) (v2, _) -> compare_var v1 v2) l in
  let upd_vars_ty = List.mapi ~f:(fun i (n, t) -> (to_label n, {associated_type = t; michelson_annotation = None; decl_pos = i})) upd_vars in
  upd_vars, t_record ~layout:L_comb (LMap.of_list upd_vars_ty)

and vars_of_type : type_expression -> _ =
  fun t ->
  let record = get_t_record_exn t in
  let l = LMap.map (fun {associated_type; _} -> associated_type) record.content in
  let l = LMap.to_kv_list l in
  List.map ~f:(fun (Label s, t) -> (Location.wrap (Var.of_name s), t)) l

and make_access record label =
  let record_type = record.type_expression in
  let {content;_} = get_t_record_exn record_type in
  let {associated_type;_} = LMap.find (Label label) content in
  make_e (e_record_accessor record (Label label)) associated_type

and make_pair l r =
  make_e (e_record (LMap.of_list [(Label "0", l); (Label "1", r)])) (t_pair l.type_expression r.type_expression)

and bind : expression * env_type -> (expression_variable * type_expression * env_type -> (expression * env_type, _) result) -> (expression * env_type, _) result =
  fun (expr, expr_env) k ->
  let p_expr = Location.wrap (Var.fresh ~name:"p_EXPR" ()) in
  let v_expr = e_a_variable p_expr expr.type_expression in
  let* result, result_vars = k (p_expr, expr.type_expression, expr_env) in
  let result = prefix_with_env (make_access v_expr "1") expr_env result in
  ok @@ (e_a_let_in p_expr expr result false, result_vars)

and bind' : expression -> (expression_variable * type_expression -> (expression * env_type, _) result) -> (expression * env_type, _) result =
  fun expr k ->
  let p_expr = Location.wrap (Var.fresh ~name:"p_EXPR" ()) in
  let* result, result_vars = k (p_expr, expr.type_expression) in
  ok @@ (e_a_let_in p_expr expr result false, result_vars)
