module I = Ast_imperative
module O = Ast_sugar
open Trace

module Errors = struct
  let bad_collection expr =
    let title () = "" in
    let message () = Format.asprintf "\nCannot loop over this collection : %a\n" I.PP.expression expr in
    let data = [
      ("location",
      fun () -> Format.asprintf  "%a" Location.pp expr.location)
    ] in
    error ~data title message
end

let rec add_to_end (expression: O.expression) to_add =
  match expression.expression_content with
  | O.E_let_in lt -> 
    let lt = {lt with let_result = add_to_end lt.let_result to_add} in
    {expression with expression_content = O.E_let_in lt}
  | O.E_sequence seq -> 
    let seq = {seq with expr2 = add_to_end seq.expr2 to_add} in
    {expression with expression_content = O.E_sequence seq}
  | _ -> O.e_sequence expression to_add

let repair_mutable_variable_in_matching (match_body : O.expression) (element_names : O.expression_variable list) (env : I.expression_variable) =
  let%bind ((dv,fv),mb) = Self_ast_sugar.fold_map_expression
    (* TODO : these should use Variables sets *)
    (fun (decl_var,free_var : O.expression_variable list * O.expression_variable list) (ass_exp : O.expression) ->
      match ass_exp.expression_content with
        | E_let_in {let_binder;mut=false;rhs;let_result} ->
          let (name,_) = let_binder in
          ok (true,(name::decl_var, free_var),O.e_let_in let_binder false false rhs let_result)
        | E_let_in {let_binder;mut=true; rhs;let_result} ->
          let (name,_) = let_binder in
          if List.mem name decl_var then 
            ok (true,(decl_var, free_var), O.e_let_in let_binder false false rhs let_result)
          else(
            let free_var = if (List.mem name free_var) then free_var else name::free_var in
            let expr = O.e_let_in (env,None) false false (O.e_update (O.e_variable env) (Var.to_name name) (O.e_variable name)) let_result in
            ok (true,(decl_var, free_var), O.e_let_in let_binder false  false rhs expr)
          )
        | E_constant {cons_name=C_MAP_FOLD;arguments= _}
        | E_constant {cons_name=C_SET_FOLD;arguments= _}
        | E_constant {cons_name=C_LIST_FOLD;arguments= _} 
        | E_matching _ -> ok @@ (false, (decl_var,free_var),ass_exp)
      | _ -> ok (true, (decl_var, free_var),ass_exp)
    )
      (element_names,[])
      match_body in
  ok @@ ((dv,fv),mb)

and repair_mutable_variable_in_loops (for_body : O.expression) (element_names : O.expression_variable list) (env : O.expression_variable) =
  let%bind ((dv,fv),fb) = Self_ast_sugar.fold_map_expression
    (* TODO : these should use Variables sets *)
    (fun (decl_var,free_var : O.expression_variable list * O.expression_variable list) (ass_exp : O.expression) ->
      (* Format.printf "debug: dv:%a; fv:%a; expr:%a \n%!" 
        (I.PP.list_sep_d I.PP.expression_variable) decl_var
        (I.PP.list_sep_d I.PP.expression_variable) decl_var
        O.PP.expression ass_exp
      ;*)
      match ass_exp.expression_content with
        | E_let_in {let_binder;mut=false;} ->
          let (name,_) = let_binder in
          ok (true,(name::decl_var, free_var),ass_exp)
        | E_let_in {let_binder;mut=true; rhs;let_result} ->
          let (name,_) = let_binder in
          if List.mem name decl_var then 
            ok (true,(decl_var, free_var), O.e_let_in let_binder false false rhs let_result)
          else(
            let free_var = if (List.mem name free_var) then free_var else name::free_var in
            let expr = O.e_let_in (env,None) false false (
              O.e_update (O.e_variable env) ("0") 
              (O.e_update (O.e_accessor (O.e_variable env) "0") (Var.to_name name) (O.e_variable name))
              )
              let_result in
            ok (true,(decl_var, free_var), O.e_let_in let_binder false  false rhs expr)
          )
        | E_constant {cons_name=C_MAP_FOLD;arguments= _}
        | E_constant {cons_name=C_SET_FOLD;arguments= _}
        | E_constant {cons_name=C_LIST_FOLD;arguments= _} 
        | E_matching _ -> ok @@ (false,(decl_var,free_var),ass_exp)
      | _ -> ok (true,(decl_var, free_var),ass_exp)
    )
      (element_names,[])
      for_body in
  ok @@ ((dv,fv),fb)

and store_mutable_variable (free_vars : I.expression_variable list) =
  if (List.length free_vars == 0) then
    O.e_unit ()
  else
    let aux var = (Var.to_name var, O.e_variable var) in
    O.e_record_ez (List.map aux free_vars)
 
and restore_mutable_variable (expr : O.expression->O.expression_content) (free_vars : O.expression_variable list) (env : O.expression_variable) =
  let aux (f: O.expression -> O.expression) (ev: O.expression_variable) =
    fun expr -> f (O.e_let_in (ev,None) true false (O.e_accessor (O.e_variable env) (Var.to_name ev)) expr)
  in
  let ef = List.fold_left aux (fun e -> e) free_vars in
  expr (ef (O.e_skip ()))


let rec compile_type_expression : I.type_expression -> O.type_expression result =
  fun te ->
  let return te = ok @@ O.make_t te in
  match te.type_content with
    | I.T_sum sum -> 
      let sum = I.CMap.to_kv_list sum in
      let%bind sum = 
        bind_map_list (fun (k,v) ->
          let%bind v = compile_type_expression v in
          ok @@ (k,v)
        ) sum
      in
      return @@ O.T_sum (O.CMap.of_list sum)
    | I.T_record record -> 
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (k,v) ->
          let%bind v = compile_type_expression v in
          ok @@ (k,v)
        ) record
      in
      return @@ O.T_record (O.LMap.of_list record)
    | I.T_arrow {type1;type2} ->
      let%bind type1 = compile_type_expression type1 in
      let%bind type2 = compile_type_expression type2 in
      return @@ T_arrow {type1;type2}
    | I.T_variable type_variable -> return @@ T_variable type_variable 
    | I.T_constant type_constant -> return @@ T_constant type_constant
    | I.T_operator type_operator ->
      let%bind type_operator = compile_type_operator type_operator in
      return @@ T_operator type_operator

and compile_type_operator : I.type_operator -> O.type_operator result =
  fun t_o ->
  match t_o with
    | TC_contract c -> 
      let%bind c = compile_type_expression c in
      ok @@ O.TC_contract c
    | TC_option o ->
      let%bind o = compile_type_expression o in
      ok @@ O.TC_option o
    | TC_list l ->
      let%bind l = compile_type_expression l in
      ok @@ O.TC_list l
    | TC_set s ->
      let%bind s = compile_type_expression s in
      ok @@ O.TC_set s
    | TC_map (k,v) ->
      let%bind (k,v) = bind_map_pair compile_type_expression (k,v) in
      ok @@ O.TC_map (k,v)
    | TC_big_map (k,v) ->
      let%bind (k,v) = bind_map_pair compile_type_expression (k,v) in
      ok @@ O.TC_big_map (k,v)
    | TC_arrow (i,o) ->
      let%bind (i,o) = bind_map_pair compile_type_expression (i,o) in
      ok @@ O.TC_arrow (i,o)

let rec compile_expression : I.expression -> O.expression result =
  fun e ->
  let return expr = ok @@ O.make_expr ~loc:e.location expr in
  match e.expression_content with
    | I.E_literal literal   -> return @@ O.E_literal literal
    | I.E_constant {cons_name;arguments} -> 
      let%bind arguments = bind_map_list compile_expression arguments in
      return @@ O.E_constant {cons_name;arguments}
    | I.E_variable name     -> return @@ O.E_variable name
    | I.E_application {lamb;args} -> 
      let%bind lamb = compile_expression lamb in
      let%bind args = compile_expression args in
      return @@ O.E_application {lamb;args}
    | I.E_lambda lambda ->
      let%bind lambda = compile_lambda lambda in
      return @@ O.E_lambda lambda
    | I.E_recursive {fun_name;fun_type;lambda} ->
      let%bind fun_type = compile_type_expression fun_type in
      let%bind lambda = compile_lambda lambda in
      return @@ O.E_recursive {fun_name;fun_type;lambda}
    | I.E_let_in {let_binder;mut;inline;rhs;let_result} ->
      let (binder,ty_opt) = let_binder in
      let%bind ty_opt = bind_map_option compile_type_expression ty_opt in
      let%bind rhs = compile_expression rhs in
      let%bind let_result = compile_expression let_result in
      return @@ O.E_let_in {let_binder=(binder,ty_opt);mut;inline;rhs;let_result}
    | I.E_constructor {constructor;element} ->
      let%bind element = compile_expression element in
      return @@ O.E_constructor {constructor;element}
    | I.E_matching m ->
      let%bind m = compile_matching m in
      return @@ m 
    | I.E_record record ->
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (k,v) ->
          let%bind v =compile_expression v in
          ok @@ (k,v)
        ) record
      in
      return @@ O.E_record (O.LMap.of_list record)
    | I.E_record_accessor {expr;label} ->
      let%bind expr = compile_expression expr in
      return @@ O.E_record_accessor {expr;label}
    | I.E_record_update {record;path;update} ->
      let%bind record = compile_expression record in
      let%bind update = compile_expression update in
      return @@ O.E_record_update {record;path;update}
    | I.E_map map ->
      let%bind map = bind_map_list (
        bind_map_pair compile_expression
      ) map
      in
      return @@ O.E_map map
    | I.E_big_map big_map ->
      let%bind big_map = bind_map_list (
        bind_map_pair compile_expression
      ) big_map
      in
      return @@ O.E_big_map big_map
    | I.E_list lst ->
      let%bind lst = bind_map_list compile_expression lst in
      return @@ O.E_list lst
    | I.E_set set ->
      let%bind set = bind_map_list compile_expression set in
      return @@ O.E_set set 
    | I.E_look_up look_up ->
      let%bind look_up = bind_map_pair compile_expression look_up in
      return @@ O.E_look_up look_up
    | I.E_ascription {anno_expr; type_annotation} ->
      let%bind anno_expr = compile_expression anno_expr in
      let%bind type_annotation = compile_type_expression type_annotation in
      return @@ O.E_ascription {anno_expr; type_annotation}
    | I.E_sequence {expr1; expr2} ->
      let%bind expr1 = compile_expression expr1 in
      let%bind expr2 = compile_expression expr2 in
      ok @@ add_to_end expr1 expr2 
    | I.E_skip -> return @@ O.E_skip
    | I.E_for f -> 
      let%bind f = compile_for f in
      return @@ f
    | I.E_for_each fe -> 
      let%bind fe = compile_for_each fe in
      return @@ fe
    | I.E_while w ->
      let%bind w = compile_while w in
      return @@ w

and compile_lambda : I.lambda -> O.lambda result =
  fun {binder;input_type;output_type;result}->
    let%bind input_type = bind_map_option compile_type_expression input_type in
    let%bind output_type = bind_map_option compile_type_expression output_type in
    let%bind result = compile_expression result in
    ok @@ O.{binder;input_type;output_type;result}

and compile_matching : I.matching -> O.expression_content result =
  fun {matchee;cases} ->
  let%bind matchee = compile_expression matchee in
  match cases with 
    | I.Match_bool {match_true;match_false} ->
      let%bind match_true' = compile_expression match_true in
      let%bind match_false' = compile_expression match_false in
      let env = Var.fresh () in
      let%bind ((_,free_vars_true), match_true) = repair_mutable_variable_in_matching match_true' [] env in
      let%bind ((_,free_vars_false), match_false) = repair_mutable_variable_in_matching match_false' [] env in
      let match_true  = add_to_end match_true (O.e_variable env) in
      let match_false = add_to_end match_false (O.e_variable env) in

      let free_vars = List.sort_uniq Var.compare @@ free_vars_true @ free_vars_false in
      if (List.length free_vars != 0) then 
        let match_expr  = O.e_matching matchee (O.Match_bool {match_true; match_false}) in
        let return_expr = fun expr ->
          O.E_let_in {let_binder=(env,None); mut=false; inline=false;rhs=(store_mutable_variable free_vars);
          let_result=O.e_let_in (env,None) false false match_expr @@
          expr 
          }
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      else
        ok @@ O.E_matching {matchee;cases=O.Match_bool {match_true=match_true';match_false=match_false'}}
    | I.Match_option {match_none;match_some} ->
      let%bind match_none' = compile_expression match_none in
      let (n,expr,tv) = match_some in
      let%bind expr' = compile_expression expr in
      let env = Var.fresh () in
      let%bind ((_,free_vars_none), match_none) = repair_mutable_variable_in_matching match_none' [] env in
      let%bind ((_,free_vars_some), expr) = repair_mutable_variable_in_matching expr' [n] env in
      let match_none = add_to_end match_none (O.e_variable env) in
      let expr       = add_to_end expr (O.e_variable env) in
      let free_vars = List.sort_uniq Var.compare @@ free_vars_none @ free_vars_some in
      if (List.length free_vars != 0) then
        let match_expr  = O.e_matching matchee (O.Match_option {match_none; match_some=(n,expr,tv)}) in
        let return_expr = fun expr ->
          O.E_let_in {let_binder=(env,None); mut=false; inline=false;rhs=(store_mutable_variable free_vars);
          let_result=O.e_let_in (env,None) false false match_expr @@
          expr 
          }
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      else
        ok @@ O.E_matching {matchee; cases=O.Match_option {match_none=match_none'; match_some=(n,expr',tv)}}
    | I.Match_list {match_nil;match_cons} ->
      let%bind match_nil' = compile_expression match_nil in
      let (hd,tl,expr,tv) = match_cons in
      let%bind expr' = compile_expression expr in
      let env = Var.fresh () in
      let%bind ((_,free_vars_nil), match_nil) = repair_mutable_variable_in_matching match_nil' [] env in
      let%bind ((_,free_vars_cons), expr) = repair_mutable_variable_in_matching expr' [hd;tl] env in
      let match_nil = add_to_end match_nil (O.e_variable env) in
      let expr       = add_to_end expr (O.e_variable env) in
      let free_vars = List.sort_uniq Var.compare @@ free_vars_nil @ free_vars_cons in
      if (List.length free_vars != 0) then
        let match_expr  = O.e_matching matchee (O.Match_list {match_nil; match_cons=(hd,tl,expr,tv)}) in
        let return_expr = fun expr ->
          O.E_let_in {let_binder=(env,None); mut=false; inline=false;rhs=(store_mutable_variable free_vars);
          let_result=O.e_let_in (env,None) false false match_expr @@
          expr 
          }
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      else
        ok @@ O.E_matching {matchee;cases=O.Match_list {match_nil=match_nil'; match_cons=(hd,tl,expr',tv)}}
    | I.Match_tuple ((lst,expr), tv) ->
      let%bind expr = compile_expression expr in
      ok @@ O.E_matching {matchee; cases=O.Match_tuple ((lst,expr), tv)}
    | I.Match_variant (lst,tv) ->
      let env = Var.fresh () in
      let aux fv ((c,n),expr) =
        let%bind expr = compile_expression expr in
        let%bind ((_,free_vars), case_clause) = repair_mutable_variable_in_matching expr [n] env in
        let case_clause'= expr in
        let case_clause = add_to_end case_clause (O.e_variable env) in
        ok (free_vars::fv,((c,n), case_clause, case_clause')) in
      let%bind (fv,cases) = bind_fold_map_list aux [] lst in
      let free_vars = List.sort_uniq Var.compare @@ List.concat fv in
      if (List.length free_vars == 0) then (
        let cases = List.map (fun case -> let (a,_,b) = case in (a,b)) cases in
        ok @@ O.E_matching{matchee; cases=O.Match_variant (cases,tv)}
      ) else (
        let cases = List.map (fun case -> let (a,b,_) = case in (a,b)) cases in
        let match_expr = O.e_matching matchee @@ O.Match_variant (cases,tv) in
        let return_expr = fun expr ->
          O.E_let_in {let_binder=(env,None); mut=false; inline=false; rhs=(store_mutable_variable free_vars);
          let_result=O.e_let_in (env,None) false false match_expr @@
          expr 
          }
        in
        ok @@ restore_mutable_variable return_expr free_vars env
      )
 
and compile_while I.{condition;body} =
  let env_rec = Var.fresh () in
  let binder  = Var.fresh () in

  let%bind cond = compile_expression condition in
  let ctrl = 
    (O.e_variable binder)
  in

  let%bind for_body = compile_expression body in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops for_body [] binder in
  let for_body = add_to_end for_body ctrl in

  let aux name expr=
    O.e_let_in (name,None) false false (O.e_accessor (O.e_accessor (O.e_variable binder) "0") (Var.to_name name)) expr
  in
  let init_rec = O.e_tuple [store_mutable_variable @@ captured_name_list] in
  let restore = fun expr -> List.fold_right aux captured_name_list expr in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [for_body] in
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable binder] in
  let aux_func = 
    O.e_lambda binder None None @@ 
    restore @@
    O.e_cond cond continue_expr stop_expr in
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let let_binder = (env_rec,None) in
  let return_expr = fun expr -> 
    O.E_let_in {let_binder; mut=false; inline=false; rhs=init_rec; let_result=
    O.e_let_in let_binder false false loop @@
    O.e_let_in let_binder false false (O.e_accessor (O.e_variable env_rec) "0") @@
    expr
    }
  in
  ok @@ restore_mutable_variable return_expr captured_name_list env_rec 


and compile_for I.{binder;start;final;increment;body} =
  let env_rec = Var.fresh () in
  (*Make the cond and the step *)
  let cond = I.e_annotation (I.e_constant C_LE [I.e_variable binder ; final]) I.t_bool in
  let%bind cond = compile_expression cond in
  let%bind step = compile_expression increment in
  let continue_expr = O.e_constant C_FOLD_CONTINUE [(O.e_variable env_rec)] in
  let ctrl = 
    O.e_let_in (binder,Some O.t_int) false false (O.e_constant C_ADD [ O.e_variable binder ; step ]) @@
    O.e_let_in (env_rec, None) false false (O.e_update (O.e_variable env_rec) "1" @@ O.e_variable binder)@@
    continue_expr
  in
  (* Modify the body loop*)
  let%bind body = compile_expression body in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable_in_loops body [binder] env_rec in
  let for_body = add_to_end for_body ctrl in

  let aux name expr=
    O.e_let_in (name,None) false false (O.e_accessor (O.e_accessor (O.e_variable env_rec) "0") (Var.to_name name)) expr
  in

  (* restores the initial value of the free_var*)
  let restore = fun expr -> List.fold_right aux captured_name_list expr in

  (*Prep the lambda for the fold*)
  let stop_expr = O.e_constant C_FOLD_STOP [O.e_variable env_rec] in
  let aux_func = O.e_lambda env_rec None None @@ 
                 O.e_let_in (binder,Some O.t_int) false false (O.e_accessor (O.e_variable env_rec) "1") @@
                 O.e_cond cond (restore for_body) (stop_expr) in

  (* Make the fold_while en precharge the vakye *)
  let loop = O.e_constant C_FOLD_WHILE [aux_func; O.e_variable env_rec] in
  let init_rec = O.e_pair (store_mutable_variable captured_name_list) @@ O.e_variable binder in
  
  let%bind start = compile_expression start in
  let let_binder = (env_rec,None) in
  let return_expr = fun expr -> 
    O.E_let_in {let_binder=(binder, Some O.t_int);mut=false; inline=false;rhs=start;let_result=
    O.e_let_in let_binder false false init_rec @@
    O.e_let_in let_binder false false loop @@
    O.e_let_in let_binder false false (O.e_accessor (O.e_variable env_rec) "0") @@
    expr
    }
  in
  ok @@ restore_mutable_variable return_expr captured_name_list env_rec 

and compile_for_each I.{binder;collection;collection_type; body} =
  let args = Var.fresh () in
  let%bind element_names = ok @@ match snd binder with
    | Some v -> [fst binder;v]
    | None -> [fst binder] 
  in
  
  let env = Var.fresh () in
  let%bind body = compile_expression body in
  let%bind ((_,free_vars), body) = repair_mutable_variable_in_loops body element_names args in
  let for_body = add_to_end body @@ (O.e_accessor (O.e_variable args) "0") in

  let init_record = store_mutable_variable free_vars in
  let%bind collect = compile_expression collection in
  let aux name expr=
    O.e_let_in (name,None) false false (O.e_accessor (O.e_accessor (O.e_variable args) "0") (Var.to_name name)) expr
  in
  let restore = fun expr -> List.fold_right aux free_vars expr in
  let restore = match collection_type with
    | Map -> (match snd binder with 
      | Some v -> fun expr -> restore (O.e_let_in (fst binder, None) false false (O.e_accessor (O.e_accessor (O.e_variable args) "1") "0") 
                                    (O.e_let_in (v, None) false false (O.e_accessor (O.e_accessor (O.e_variable args) "1") "1") expr))
      | None -> fun expr -> restore (O.e_let_in (fst binder, None) false false (O.e_accessor (O.e_accessor (O.e_variable args) "1") "0") expr) 
    )
    | _ -> fun expr -> restore (O.e_let_in (fst binder, None) false false (O.e_accessor (O.e_variable args) "1") expr)
  in
  let lambda = O.e_lambda args None None (restore for_body) in
  let%bind op_name = match collection_type with
   | Map -> ok @@ O.C_MAP_FOLD | Set -> ok @@ O.C_SET_FOLD | List -> ok @@ O.C_LIST_FOLD 
  in
  let fold = fun expr -> 
    O.E_let_in {let_binder=(env,None);mut=false; inline=false;rhs=(O.e_constant op_name [lambda; collect ; init_record]);
    let_result=expr;}
  in
  ok @@ restore_mutable_variable fold free_vars env
let compile_declaration : I.declaration Location.wrap -> _ =
  fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | I.Declaration_constant (n, te_opt, inline, expr) ->
    let%bind expr = compile_expression expr in
    let%bind te_opt = bind_map_option compile_type_expression te_opt in
    return @@ O.Declaration_constant (n, te_opt, inline, expr)
  | I.Declaration_type (n, te) ->
    let%bind te = compile_type_expression te in
    return @@ O.Declaration_type (n,te)

let compile_program : I.program -> O.program result =
  fun p ->
  bind_map_list compile_declaration p

(* uncompiling *)
let rec uncompile_type_expression : O.type_expression -> I.type_expression result =
  fun te ->
  let return te = ok @@ I.make_t te in
  match te.type_content with
    | O.T_sum sum -> 
      let sum = I.CMap.to_kv_list sum in
      let%bind sum = 
        bind_map_list (fun (k,v) ->
          let%bind v = uncompile_type_expression v in
          ok @@ (k,v)
        ) sum
      in
      return @@ I.T_sum (O.CMap.of_list sum)
    | O.T_record record -> 
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (k,v) ->
          let%bind v = uncompile_type_expression v in
          ok @@ (k,v)
        ) record
      in
      return @@ I.T_record (O.LMap.of_list record)
    | O.T_arrow {type1;type2} ->
      let%bind type1 = uncompile_type_expression type1 in
      let%bind type2 = uncompile_type_expression type2 in
      return @@ T_arrow {type1;type2}
    | O.T_variable type_variable -> return @@ T_variable type_variable 
    | O.T_constant type_constant -> return @@ T_constant type_constant
    | O.T_operator type_operator ->
      let%bind type_operator = uncompile_type_operator type_operator in
      return @@ T_operator type_operator

and uncompile_type_operator : O.type_operator -> I.type_operator result =
  fun t_o ->
  match t_o with
    | TC_contract c -> 
      let%bind c = uncompile_type_expression c in
      ok @@ I.TC_contract c
    | TC_option o ->
      let%bind o = uncompile_type_expression o in
      ok @@ I.TC_option o
    | TC_list l ->
      let%bind l = uncompile_type_expression l in
      ok @@ I.TC_list l
    | TC_set s ->
      let%bind s = uncompile_type_expression s in
      ok @@ I.TC_set s
    | TC_map (k,v) ->
      let%bind (k,v) = bind_map_pair uncompile_type_expression (k,v) in
      ok @@ I.TC_map (k,v)
    | TC_big_map (k,v) ->
      let%bind (k,v) = bind_map_pair uncompile_type_expression (k,v) in
      ok @@ I.TC_big_map (k,v)
    | TC_arrow (i,o) ->
      let%bind (i,o) = bind_map_pair uncompile_type_expression (i,o) in
      ok @@ I.TC_arrow (i,o)

let rec uncompile_expression : O.expression -> I.expression result =
  fun e ->
  let return expr = ok @@ I.make_expr ~loc:e.location expr in
  match e.expression_content with 
    O.E_literal lit -> return @@ I.E_literal lit
  | O.E_constant {cons_name;arguments} -> 
    let%bind arguments = bind_map_list uncompile_expression arguments in
    return @@ I.E_constant {cons_name;arguments}
  | O.E_variable name     -> return @@ I.E_variable name
  | O.E_application {lamb; args} -> 
    let%bind lamb = uncompile_expression lamb in
    let%bind args = uncompile_expression args in
    return @@ I.E_application {lamb; args}
  | O.E_lambda lambda ->
    let%bind lambda = uncompile_lambda lambda in
    return @@ I.E_lambda lambda
  | O.E_recursive {fun_name;fun_type;lambda} ->
    let%bind fun_type = uncompile_type_expression fun_type in
    let%bind lambda = uncompile_lambda lambda in
    return @@ I.E_recursive {fun_name;fun_type;lambda}
  | O.E_let_in {let_binder;inline;rhs;let_result} ->
    let (binder,ty_opt) = let_binder in
    let%bind ty_opt = bind_map_option uncompile_type_expression ty_opt in
    let%bind rhs = uncompile_expression rhs in
    let%bind let_result = uncompile_expression let_result in
    return @@ I.E_let_in {let_binder=(binder,ty_opt);mut=false;inline;rhs;let_result}
  | O.E_constructor {constructor;element} ->
    let%bind element = uncompile_expression element in
    return @@ I.E_constructor {constructor;element}
  | O.E_matching {matchee; cases} ->
    let%bind matchee = uncompile_expression matchee in
    let%bind cases   = uncompile_matching cases in
    return @@ I.E_matching {matchee;cases}
  | O.E_record record ->
    let record = I.LMap.to_kv_list record in
    let%bind record = 
      bind_map_list (fun (k,v) ->
        let%bind v = uncompile_expression v in
        ok @@ (k,v)
      ) record
    in
    return @@ I.E_record (O.LMap.of_list record)
  | O.E_record_accessor {expr;label} ->
    let%bind expr = uncompile_expression expr in
    return @@ I.E_record_accessor {expr;label}
  | O.E_record_update {record;path;update} ->
    let%bind record = uncompile_expression record in
    let%bind update = uncompile_expression update in
    return @@ I.E_record_update {record;path;update}
  | O.E_map map ->
    let%bind map = bind_map_list (
      bind_map_pair uncompile_expression
    ) map
    in
    return @@ I.E_map map
  | O.E_big_map big_map ->
    let%bind big_map = bind_map_list (
      bind_map_pair uncompile_expression
    ) big_map
    in
    return @@ I.E_big_map big_map
  | O.E_list lst ->
    let%bind lst = bind_map_list uncompile_expression lst in
    return @@ I.E_list lst
  | O.E_set set ->
    let%bind set = bind_map_list uncompile_expression set in
    return @@ I.E_set set 
  | O.E_look_up look_up ->
    let%bind look_up = bind_map_pair uncompile_expression look_up in
    return @@ I.E_look_up look_up
  | O.E_ascription {anno_expr; type_annotation} ->
    let%bind anno_expr = uncompile_expression anno_expr in
    let%bind type_annotation = uncompile_type_expression type_annotation in
    return @@ I.E_ascription {anno_expr; type_annotation}
  | O.E_sequence {expr1; expr2} ->
    let%bind expr1 = uncompile_expression expr1 in
    let%bind expr2 = uncompile_expression expr2 in
    return @@ I.E_sequence {expr1; expr2}
  | O.E_skip -> return @@ I.E_skip

and uncompile_lambda : O.lambda -> I.lambda result =
  fun {binder;input_type;output_type;result}->
    let%bind input_type = bind_map_option uncompile_type_expression input_type in
    let%bind output_type = bind_map_option uncompile_type_expression output_type in
    let%bind result = uncompile_expression result in
    ok @@ I.{binder;input_type;output_type;result}
and uncompile_matching : O.matching_expr -> I.matching_expr result =
  fun m -> 
  match m with 
    | O.Match_bool {match_true;match_false} ->
      let%bind match_true = uncompile_expression match_true in
      let%bind match_false = uncompile_expression match_false in
      ok @@ I.Match_bool {match_true;match_false}
    | O.Match_list {match_nil;match_cons} ->
      let%bind match_nil = uncompile_expression match_nil in
      let (hd,tl,expr,tv) = match_cons in
      let%bind expr = uncompile_expression expr in
      ok @@ I.Match_list {match_nil; match_cons=(hd,tl,expr,tv)}
    | O.Match_option {match_none;match_some} ->
      let%bind match_none = uncompile_expression match_none in
      let (n,expr,tv) = match_some in
      let%bind expr = uncompile_expression expr in
      ok @@ I.Match_option {match_none; match_some=(n,expr,tv)}
    | O.Match_tuple ((lst,expr), tv) ->
      let%bind expr = uncompile_expression expr in
      ok @@ O.Match_tuple ((lst,expr), tv)
    | O.Match_variant (lst,tv) ->
      let%bind lst = bind_map_list (
        fun ((c,n),expr) ->
          let%bind expr = uncompile_expression expr in
          ok @@ ((c,n),expr)
      ) lst 
      in
      ok @@ I.Match_variant (lst,tv)
