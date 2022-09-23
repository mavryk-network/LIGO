module AST = Ast_aggregated
open Ligo_prim

module type MONAD = sig
  val t : AST.type_expression -> AST.type_expression
  val ret : AST.expression -> AST.expression
  val bind : AST.expression -> AST.expression -> AST.expression
end

module MONADIC_TRANSFORM (M : MONAD) = struct

  let bindx transform v x result =
    let binder = Binder.make x v.AST.type_expression in
    let output_type = result.AST.type_expression in
    M.bind (transform v) AST.(e_a_lambda { binder ; output_type ; result } v.type_expression result.type_expression)

  let bind transform v result =
    let x = Value_var.fresh () in
    let binder = Binder.make x v.AST.type_expression in
    let result = result AST.(e_a_variable x v.type_expression) in
    let output_type = result.AST.type_expression in
    M.bind (transform v) AST.(e_a_lambda { binder ; output_type ; result } v.type_expression result.type_expression)

  let binds transform args k =
    let exprs vs =
      let rec exprs vs es =
        match vs with
        | [] -> k es
        | (v :: vs) ->
          exprs vs (v :: es) in
      exprs vs [] in
    let rec binds args vs =
      match args with
      | [] -> exprs vs
      | arg :: args ->
        bind transform arg (fun v -> binds args (v :: vs)) in
    binds args []

  let ret = M.ret

  let rec monadic_type (t : AST.type_expression) : AST.type_expression =
    match t.type_content with
    | T_variable _ ->
      failwith "There shouldn't be any variable"
    | T_constant { language ; injection ; parameters } ->
      let parameters = List.map ~f:monadic_type parameters in
      let type_content = AST.T_constant { language ; injection ; parameters } in
      { t with type_content }
    | T_arrow { type1 ; type2 } ->
      let type2 = M.t type2 in
      let type_content = AST.T_arrow { type1 ; type2 } in
      { t with type_content }
    | _ ->
      failwith "write me"

  let rec monadic_expression (e : AST.expression) : AST.expression =
    let self_type = monadic_type in
    let self = monadic_expression in
    let bind = bind self in
    let bindx = bindx self in
    let binds = binds self in
    match e.expression_content with
    | E_variable _ | E_literal _ | E_raw_code _ ->
      ret e
    | E_constant { cons_name ; arguments } ->
      binds arguments (fun es -> ret @@ { e with expression_content = E_constant { cons_name ; arguments = es } })
    | E_application { lamb ; args } ->
      bind args (fun args -> bind lamb (fun lamb -> { e with expression_content = E_application { lamb ; args } }))
    | E_lambda { binder ; output_type ; result } ->
      let result = self result in
      ret @@ { e with expression_content = E_lambda { binder ; output_type ; result } }
    | E_recursive { fun_name ; fun_type ; lambda = { binder ; output_type ; result } } ->
      let result = self result in
      ret @@ { e with expression_content = E_recursive { fun_name ; fun_type ; lambda = { binder ; output_type ; result } } }
    | E_let_in { let_binder ; rhs ; let_result ; attr = _ } ->
      let let_result = self let_result in
      bindx rhs (Binder.get_var let_binder) let_result
    | E_type_inst _ | E_type_abstraction _ ->
      failwith "Polymorphism should be removed already"
    | E_constructor { constructor ; element } ->
      bind element (fun element -> { e with expression_content = E_constructor { constructor ; element } })
    | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
      bind matchee (fun matchee ->
          let self_case AST.{ constructor ; pattern ; body } =
            let body = self body in
            AST.{ constructor ; pattern ; body } in
          let cases = List.map ~f:self_case cases in
          let tv = self_type tv in
          let expression_content = AST.E_matching { matchee ; cases = Match_variant { cases ; tv } } in
          { e with expression_content }
        )
    | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
      bind matchee (fun matchee ->
          let body = self body in
          let tv = self_type tv in
          let expression_content = AST.E_matching { matchee ; cases = Match_record { fields ; body ; tv } } in
          { e with expression_content }
        )
    | E_record map ->
      let ls, es = List.unzip @@ Record.LMap.to_kv_list map in
      binds es (fun es ->
          let map = Record.LMap.of_list @@ List.zip_exn ls es in
          ret @@ {e with expression_content = E_record map }
        )
    | _ ->
      failwith "write me"
end
