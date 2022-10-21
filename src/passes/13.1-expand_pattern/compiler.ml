module I = Ast_aggregated
module O = Ast_pattern_expanded

open Ligo_prim

let rec compile_pattern : I.expression -> O.expression = fun exp ->
  let self = compile_pattern in
  let return : O.expression_content -> O.expression = fun expression_content ->
    { expression_content ; type_expression = exp.type_expression ; location = exp.location } in
  match exp.expression_content with
  | E_matching {matchee;cases} ->
    failwith "ll"
  | E_let_in { let_binder ; rhs ; let_result; attributes } ->
    failwith "lol"

  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_result = self let_result in
    return (O.E_let_mut_in { let_binder; rhs; let_result; attributes })

  
  | E_record m -> (
    let m = Record.map ~f:self m in
    return (O.E_record m)
  )
  | E_accessor acc -> (
    let acc = I.Accessor.map self acc in
    return (O.E_accessor acc)
  )
  | E_update u -> (
    let u = I.Update.map self u in
    return (O.E_update u)
  )
  | E_constructor c -> (
      let e' = self c.element in
      return (O.E_constructor {c with element = e'})
  )
  | E_application {lamb;args} -> (
      let ab = (lamb, args) in
      let (a,b) = Simple_utils.Pair.map ~f:self ab in
      return (O.E_application {lamb=a;args=b})
    )
  | E_type_inst { forall ; type_ } -> (
    let  forall = self forall in
    return (O.E_type_inst { forall ; type_ })
  )
  | E_lambda l -> (
      let l = Lambda.map self Fn.id l in
      return (O.E_lambda l)
    )
  | E_type_abstraction ta -> (
      let ta = Type_abs.map self ta in
      return (O.E_type_abstraction ta)
    )
  | E_recursive r ->
      let r = Recursive.map self Fn.id r in
      return (O.E_recursive r)
  | E_constant c -> (
      let c = Constant.map self c in
      return (O.E_constant c)
    )
  | E_raw_code {language;code} -> (
    let code = self code in
    return (O.E_raw_code { language ; code }))
  | E_assign a ->
    let a = Assign.map self Fn.id a in
    return (O.E_assign a)
  | E_for f ->
    let f = For_loop.map self f in
    return (O.E_for f)
  | E_for_each fe ->
    let fe = For_each_loop.map self fe in
    return (O.E_for_each fe)
  | E_while w ->
    let w = While_loop.map self w in
    return (O.E_while w)

  | E_deref x -> return (O.E_deref x)
  | E_literal x -> return (O.E_literal x)
  | E_variable x -> return (O.E_variable x)

(*
 exp
 
 
 compile_pattern exp
  
 compile_e_let_mut_in


 assert (no_E_temporary_)

*)

(* let compile_cases : loc:Location.t -> _ -> _ -> I.expression -> _ -> _ =
  fun ~loc path scope matchee cases ->
    let matchee_type = matchee.type_expression in
    let eqs = List.map cases 
      ~f:(fun {pattern ; body} -> 
          let pattern = I.Pattern.map (compile_type_expression path scope) pattern in
          let binders = I.Pattern.binders pattern |> List.map ~f:Binder.get_var in
          let scope = List.fold binders ~init:scope ~f:Scope.push_func_or_case_binder in
          let body = compile_expression path scope body in
          pattern, matchee_type, body) in
    match matchee.expression_content with
    | E_variable var ->
      let match_expr =
        let var = Binder.make var matchee.type_expression in
        Pattern_matching.compile_matching ~err_loc:loc var eqs
      in
      match_expr.expression_content
    | _ ->
      let var = Value_var.fresh ~loc ~name:"match_" () in
      let match_expr =
        let var = Binder.make var matchee.type_expression in
        Pattern_matching.compile_matching ~err_loc:loc var eqs
      in
      O.E_let_in
        { let_binder = Binder.make var matchee.type_expression
        ; rhs = matchee
        ; let_result = { match_expr with location = loc }
        ; attr =
            { inline = false
            ; no_mutation = false
            ; public = true
            ; view = false
            ; hidden = false
            ; thunk = false
            }
        } *)


        (* 

var (a,b) = <..>
a := a + 1
<..>

| ->

var (a,b) = <..> in
a := a + 1 in
<..>

| ->

match <...> with
| (a,b) ->
  var a = a in
  var b = b in
  a := a + 1 in
  <...>

*)