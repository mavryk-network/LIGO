open Ast_typed
open Errors
open Simple_utils.Trace

let assert_type_expression_eq ~raise (loc:Location.t) ((tv',tv):type_expression * type_expression) : unit = 
  trace_option ~raise (assert_equal loc tv' tv) @@
    assert_type_expression_eq (tv' , tv)

type typer = type_expression list -> type_expression option -> type_expression

let typer_2 ~raise : Location.t -> string -> (type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ] -> f a b
  | _ -> raise.raise @@ wrong_param_number l s 2 lst

let eq_1 a cst = type_expression_eq (a , cst)
let eq_2 (a , b) cst = type_expression_eq (a , cst) && type_expression_eq (b , cst)

open Stage_common
module Pair = Simple_utils.Pair

type 'a fold_mapper = 'a -> expression -> bool * 'a * expression
let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
  let self = fold_map_expression f in
  let (continue, init,e') = f a e in
  if (not continue) then (init,e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
      let (res, e') = self init e in
      let (res,cases') = fold_map_cases f res cases in
      (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record_accessor {record; path} -> (
      let (res, record) = self init record in
      (res, return @@ E_record_accessor {record; path})
    )
  | E_record m -> (
    let (res,m') = LMap.fold_map ~f:(fun _ e res -> self res e) ~init m in
    (res, return @@ E_record m')
  )
  | E_record_update {record; path; update} -> (
    let (res, record) = self init record in
    let (res, update) = self res update in
    (res, return @@ E_record_update {record;path;update})
  )
  | E_constructor c -> (
      let (res,e') = self init c.element in
      (res, return @@ E_constructor {c with element = e'})
  )
  | E_application {lamb;args} -> (
      let ab = (lamb, args) in
      let (res,(a,b)) = Pair.fold_map ~f:self ~init ab in
      (res, return @@ E_application {lamb=a;args=b})
    )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
      let (res,rhs) = self init rhs in
      let (res,let_result) = self res let_result in
      (res, return @@ E_let_in { let_binder ; rhs ; let_result ; attr })
    )
  | E_type_in { type_binder ; rhs ; let_result} -> (
      let (res,let_result) = self init let_result in
      (res, return @@ E_type_in { type_binder ; rhs ; let_result })
    )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
    let (res,let_result) = self init let_result in
    let (res,rhs) = fold_map_expression_in_module_expr f res rhs in
    (res, return @@ E_mod_in { module_binder ; rhs ; let_result })
  )
  | E_type_inst { forall ; type_ } -> (
    let (res, forall) = self init forall in
    ( res, return @@ E_type_inst { forall ; type_ })
  )
  | E_lambda { binder ; result } -> (
      let (res,result) = self init result in
      ( res, return @@ E_lambda { binder ; result })
    )
  | E_type_abstraction ta -> (
      let res, ta = Fold_maps.type_abs self init ta in
      res, return @@ E_type_abstraction ta
    )
  | E_recursive { fun_name; fun_type; lambda={binder;result}} -> (
      let (res,result) = self init result in
      (res, return @@ E_recursive {fun_name; fun_type; lambda={binder;result}})
    )
  | E_constant c -> (
      let (res,args) = List.fold_map ~f:self ~init c.arguments in
      (res, return @@ E_constant {c with arguments=args})
    )
  | E_raw_code {language;code} -> (
    let (res,code) = self init code in
    (res, return @@ E_raw_code { language ; code }))
  | E_literal _ | E_variable _  | E_module_accessor _ as e' -> (init, return e')

and fold_map_cases : 'a fold_mapper -> 'a -> matching_expr -> 'a * matching_expr = fun f init m ->
  match m with
  | Match_variant {cases ; tv} -> (
      let aux init {constructor ; pattern ; body} =
        let (init, body) = fold_map_expression f init body in
        (init, {constructor; pattern ; body})
      in
      let (init,cases) = List.fold_map ~f:aux ~init cases in
      (init, Match_variant {cases ; tv})
    )
  | Match_record { fields; body; tv } ->
      let (init, body) = fold_map_expression f init body in
      (init, Match_record { fields ; body ; tv })

and fold_map_module : 'a fold_mapper -> 'a -> module_ -> 'a * module_ = fun m init p ->
  let aux = fun acc (x : declaration) ->
    match Location.unwrap x with
    | Declaration_constant {binder ; expr ; attr } -> (
      let (acc', expr) = fold_map_expression m acc expr in
      let wrap_content = Declaration_constant {binder ; expr ; attr} in
      (acc', {x with wrap_content})
    )
    | Declaration_type t -> (
      let wrap_content = Declaration_type t in
      (acc, {x with wrap_content})
    )
    | Declaration_module {module_binder; module_; module_attr} -> (
      let (acc', module_) = fold_map_expression_in_module_expr m acc module_ in
      let wrap_content = Declaration_module {module_binder; module_; module_attr} in
      (acc', {x with wrap_content})
    )
  in
  let (a,p) = List.fold_map ~f:aux ~init p in
  (a, p)

and fold_map_expression_in_module_expr : 'a fold_mapper -> 'a -> module_expr -> 'a * module_expr = fun fold_mapper acc x ->
  let return r wrap_content = (r, { x with wrap_content }) in
  match x.wrap_content with
  | M_struct decls ->
    let res,decls = fold_map_module fold_mapper acc decls in
    return res (M_struct decls)
  | M_module_path _ as x -> return acc x
  | M_variable _ as x -> return acc x


type 'a folder = 'a -> expression -> 'a
let rec fold_expression : 'a folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ | E_module_accessor _ -> init
  | E_constant {arguments=lst;cons_name=_} -> (
    let res = List.fold ~f:self ~init lst in
    res
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let res = Pair.fold ~f:self ~init ab in
    res
  )
  | E_type_inst { forall = e; type_ = _}
  | E_lambda { binder = _ ; result = e }
  | E_type_abstraction { type_binder = _ ; result = e}
  | E_recursive {lambda= {result=e;binder=_};fun_name=_;fun_type=_}
  | E_constructor {element=e;constructor=_} -> (
    let res = self init e in
    res
  )
  | E_matching {matchee=e; cases} -> (
    let res = self init e in
    let res = fold_cases f res cases in
    res
  )
  | E_record m -> (
    let aux _ expr init =
      let res = fold_expression self init expr in
      res
    in
    let res = LMap.fold aux m init in
    res
  )
  | E_record_update {record;update;path=_} -> (
    let res = self init record in
    let res = fold_expression self res update in
    res
  )
  | E_record_accessor {record;path=_} -> (
    let res = self init record in
    res
  )
  | E_let_in { let_binder = _ ; rhs ; let_result ; attr=_} -> (
      let res = self init rhs in
      let res = self res let_result in
      res
    )
  | E_type_in { type_binder=_; rhs = _ ; let_result} ->
    let res = self init let_result in
    res
  | E_mod_in { module_binder = _ ; rhs ; let_result } -> (
    let res = fold_expression_in_module_expr self init rhs in
    let res = self res let_result in
    res
  )

and fold_expression_in_module_expr : ('a -> expression -> 'a)  -> 'a -> module_expr -> 'a = fun self acc x ->
  match x.wrap_content with
  | M_struct decls ->
    List.fold
      ~f:( fun acc (x: declaration) ->
        match x.wrap_content with
        | Declaration_constant x -> self acc x.expr
        | Declaration_module x -> fold_expression_in_module_expr self acc x.module_
        | Declaration_type _ ->  acc
      )
      ~init:acc
      decls
  | M_module_path _ -> acc
  | M_variable _ -> acc

and fold_cases : 'a folder -> 'a -> matching_expr -> 'a = fun f init m ->
  match m with
  | Match_variant {cases;tv=_} -> (
      let aux init' {constructor=_; pattern=_ ; body} =
        let res' = fold_expression f init' body in
        res' in
      let res = List.fold ~f:aux ~init cases in
      res
    )
  | Match_record {fields = _; body; tv = _} ->
    fold_expression f init body

and fold_module : 'a folder -> 'a -> module_ -> 'a = fun f init m ->
  let aux = fun acc (x : declaration) ->
    let return (d : 'a) = d in
    match Location.unwrap x with
    | Declaration_constant {binder=_; expr ; attr = { inline=_ ; no_mutation = _ ; view =_ ;public=_;thunk=_;hidden=_}} -> (
        let res = fold_expression f acc expr in
        return @@ res
    )
    | Declaration_type _t -> return @@ acc
    | Declaration_module {module_binder=_;module_ ; module_attr=_} ->
      let res = fold_expression_in_module_expr f acc module_ in
      return @@ res
  in
  let res = List.fold ~f:aux ~init m in
  res