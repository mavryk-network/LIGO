open Errors
open Ast_typed
open Simple_utils.Trace
open Ast_typed.Helpers
module Pair = Simple_utils.Pair
open Stage_common

type 'a decl_folder = 'a -> declaration -> 'a
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
    | Declaration_constant {binder=_; expr ; attr = { inline=_ ; no_mutation = _ ; view = _ ;public = _ ; thunk = _ ; hidden = _ }} -> (
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

type ty_mapper = type_expression -> unit
let rec iter_type_expression : ty_mapper -> type_expression -> unit = fun f t ->
  let self = iter_type_expression f in
  let () = f t in
  match t.type_content with
  | T_variable _ -> ()
  | T_constant x -> List.iter ~f:self x.parameters
  | T_sum x -> List.iter ~f:(fun x -> self x.associated_type) (LMap.to_list x.content)
  | T_record x -> List.iter ~f:(fun x -> self x.associated_type) (LMap.to_list x.content)
  | T_arrow x ->
    let () = self x.type1 in
    self x.type2
  | T_module_accessor _ -> ()
  | T_singleton _ -> ()
  | T_abstraction x -> self x.type_
  | T_for_all x -> self x.type_

type 'err mapper = expression -> expression
let rec map_expression : 'err mapper -> expression -> expression = fun f e ->
  let self = map_expression f in
  let e' = f e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
    let e' = self e in
    let cases' = map_cases f cases in
    return @@ E_matching {matchee=e';cases=cases'}
  )
  | E_record_accessor {record; path} -> (
    let record = self record in
    return @@ E_record_accessor {record; path}
  )
  | E_record m -> (
    let m' = LMap.map self m in
    return @@ E_record m'
  )
  | E_record_update {record; path; update} -> (
    let record = self record in
    let update = self update in
    return @@ E_record_update {record;path;update}
  )
  | E_constructor c -> (
    let e' = self c.element in
    return @@ E_constructor {c with element = e'}
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let (a,b) = Pair.map ~f:self ab in
    return @@ E_application {lamb=a;args=b}
  )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in { let_binder ; rhs ; let_result; attr }
  )
  | E_type_in {type_binder; rhs; let_result} -> (
    let let_result = self let_result in
    return @@ E_type_in {type_binder; rhs; let_result}
  )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
    let rhs = map_expression_in_module_expr f rhs in
    let let_result = self let_result in
    return @@ E_mod_in { module_binder ; rhs ; let_result }
  )
  | E_lambda { binder ; result } -> (
    let result = self result in
    return @@ E_lambda { binder ; result }
  )
  | E_type_abstraction ta -> (
      let ta = Maps.type_abs self ta in
      return @@ E_type_abstraction ta
  )
  | E_type_inst { forall ; type_ } -> (
    let forall = self forall in
    return @@ E_type_inst { forall ; type_ }
  )
  | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
    let result = self result in
    return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}
  )
  | E_constant c -> (
    let args = List.map ~f:self c.arguments in
    return @@ E_constant {c with arguments=args}
  )
  | E_module_accessor ma-> return @@ E_module_accessor ma
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'

and map_expression_in_module_expr : (expression -> expression) -> module_expr -> module_expr = fun self x ->
  let return wrap_content = { x with wrap_content } in
  match x.wrap_content with
  | M_struct decls ->
    let decls = map_module self decls in
    return (M_struct decls)
  | M_module_path _ -> x
  | M_variable _ -> x

and map_cases : 'err mapper -> matching_expr -> matching_expr = fun f m ->
  match m with
  | Match_variant {cases;tv} -> (
      let aux { constructor ; pattern ; body } =
        let body = map_expression f body in
        {constructor;pattern;body}
      in
      let cases = List.map ~f:aux cases in
      Match_variant {cases ; tv}
    )
  | Match_record {fields; body; tv} ->
    let body = map_expression f body in
    Match_record {fields; body; tv}

and map_module : 'err mapper -> module_ -> module_ = fun m p ->
  let aux = fun (x : declaration_content) ->
    let return (d : declaration_content) = d in
    match x with
    | Declaration_constant {binder; expr ; attr} -> (
        let expr = map_expression m expr in
        return @@ Declaration_constant {binder; expr ; attr}
    )
    | Declaration_type t -> return @@ Declaration_type t
    | Declaration_module {module_binder;module_;module_attr} ->
      let module_ = map_expression_in_module_expr m module_ in
      return @@ Declaration_module {module_binder; module_; module_attr}
  in
  List.map ~f:(Location.map aux) p

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
  
let fetch_entry_type ~raise : string -> module_ -> (type_expression * Location.t) = fun main_fname m ->
  let aux (declt : declaration) = match Location.unwrap declt with
    | Declaration_constant ({ binder ; expr=_ ; attr=_ } as p) ->
        if ValueVar.is_name binder.var main_fname
        then Some p
        else None
    | Declaration_type   _
    | Declaration_module _ ->
      None
  in
  let main_decl_opt = List.find_map ~f:aux @@ List.rev m in
  let main_decl =
    trace_option ~raise (corner_case ("Entrypoint '"^main_fname^"' does not exist")) @@
      main_decl_opt
    in
  let { binder=_ ; expr ; attr=_} = main_decl in
  expr.type_expression, expr.location

type contract_type = {
  parameter : Ast_typed.type_expression ;
  storage : Ast_typed.type_expression ;
}

let fetch_contract_type ~raise : expression_variable -> module_ -> contract_type = fun main_fname m ->
  let aux (declt : declaration) = match Location.unwrap declt with
    | Declaration_constant ({ binder ; expr=_ ; attr=_} as p) ->
       if ValueVar.equal binder.var main_fname
       then Some p
       else None
    | Declaration_type   _
    | Declaration_module _ ->
      None
  in
  let main_decl_opt = List.find_map ~f:aux @@ List.rev m in
  let main_decl =
    trace_option ~raise (corner_case (Format.asprintf "Entrypoint %a does not exist" ValueVar.pp main_fname : string)) @@
      main_decl_opt
  in
  let { binder=_ ; expr ; attr=_} = main_decl in
  match expr.type_expression.type_content with
  | T_arrow {type1 ; type2} -> (
    match type1.type_content , type2.type_content with
    | T_record tin , T_record tout when (is_tuple_lmap tin.content) && (is_tuple_lmap tout.content) ->
      let (parameter,storage) = trace_option ~raise (expected_pair_in_contract expr.location) @@ Ast_typed.Helpers.get_pair tin.content in
      let (listop,storage') = trace_option ~raise (expected_pair_out expr.location) @@ Ast_typed.Helpers.get_pair tout.content in
      let () = trace_option ~raise (expected_list_operation main_fname listop expr) @@
        Ast_typed.assert_t_list_operation listop in
      let () = trace_option ~raise (expected_same_entry main_fname storage storage' expr) @@
        Ast_typed.assert_type_expression_eq (storage,storage') in
      (* TODO: on storage/parameter : asert_storable, assert_passable ? *)
      { parameter ; storage }
    |  _ -> raise.raise @@ bad_contract_io main_fname expr
  )
  | _ -> raise.raise @@ bad_contract_io main_fname expr

type view_type = {
  arg : Ast_typed.type_expression ;
  storage : Ast_typed.type_expression ;
  return : Ast_typed.type_expression ;
}

let fetch_view_type ~raise : expression_variable -> module_ -> (view_type * Location.t) = fun main_fname m ->
  let aux (declt : declaration) = match Location.unwrap declt with
    | Declaration_constant ({ binder ; expr=_ ; attr=_ } as p) ->
        if ValueVar.equal binder.var main_fname
        then Some p
        else None
    | Declaration_type   _
    | Declaration_module _ -> None
  in
  let main_decl_opt = List.find_map ~f:aux @@ List.rev m in
  let main_decl =
    trace_option ~raise (corner_case (Format.asprintf "Entrypoint %a does not exist" ValueVar.pp main_fname : string)) @@
      main_decl_opt
    in
  let { binder ; expr ; attr=_ } = main_decl in
  match get_t_arrow expr.type_expression with
  | Some { type1 = tin ; type2  = return } -> (
    match get_t_tuple tin with
    | Some [ arg ; storage ] -> ({ arg ; storage ; return }, expr.location)
    | _ -> raise.raise (expected_pair_in_view @@ ValueVar.get_location binder.var)
  )
  | None -> raise.raise @@ bad_view_io main_fname expr


module Free_variables :
  sig
    val expression : expression -> (module_variable list * expression_variable list)
  end
  = struct
  module VarSet    = Caml.Set.Make(ValueVar)
  module ModVarSet = Caml.Set.Make(ModuleVar)
  module VarMap    = Caml.Map.Make(ModuleVar)

  type moduleEnv' = {modVarSet : ModVarSet.t; moduleEnv: moduleEnv; varSet: VarSet.t}
  and moduleEnv = moduleEnv' VarMap.t

  let rec merge =fun {modVarSet=x1;moduleEnv=y1;varSet=z1} {modVarSet=x2;moduleEnv=y2;varSet=z2} ->
    let aux : module_variable -> moduleEnv' -> moduleEnv' -> moduleEnv' option =
      fun _ a b -> Some (merge a b)
    in
      {modVarSet=ModVarSet.union x1 x2;moduleEnv=VarMap.union aux y1 y2;varSet=VarSet.union z1 z2}

  let unions : moduleEnv' list -> moduleEnv' =
    fun l -> List.fold l ~init:{modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}
    ~f:merge
  let rec get_fv_expr : expression -> moduleEnv' = fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v ->
      {modVarSet=ModVarSet.empty; moduleEnv=VarMap.empty ;varSet=VarSet.singleton v}
    | E_literal _ | E_raw_code _ ->
      {modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}
    | E_constant {cons_name=_;arguments} ->
      unions @@ List.map ~f:self arguments
    | E_application {lamb; args} ->
      merge (self lamb) (self args)
    | E_type_inst {forall;type_=_} ->
      self forall
    | E_lambda {binder ; result} ->
      let {modVarSet=fmv;moduleEnv;varSet=fv} = self result in
      {modVarSet=fmv;moduleEnv;varSet=VarSet.remove binder @@ fv}
    | E_type_abstraction {type_binder=_ ; result} ->
      self result
    | E_recursive {fun_name; lambda = {binder; result};fun_type=_} ->
      let {modVarSet;moduleEnv;varSet=fv} = self result in
      {modVarSet;moduleEnv;varSet=VarSet.remove fun_name @@ VarSet.remove binder @@ fv}
    | E_constructor {constructor=_;element} ->
      self element
    | E_matching {matchee; cases} ->
      merge (self matchee)(get_fv_cases cases)
    | E_record m ->
      let res = LMap.map self m in
      let res = LMap.to_list res in
      unions res
    | E_record_update {record;update;path=_} ->
      merge (self record) (self update)
    | E_record_accessor {record;path=_} ->
      self record
    | E_let_in { let_binder ; rhs ; let_result ; attr=_} ->
      let {modVarSet;moduleEnv;varSet=fv2} = (self let_result) in
      let fv2 = VarSet.remove let_binder fv2 in
      merge (self rhs) {modVarSet;moduleEnv;varSet=fv2}
    | E_type_in {let_result;type_binder=_;rhs=_} ->
      self let_result
    | E_mod_in { module_binder; rhs ; let_result } ->
      let {modVarSet;moduleEnv;varSet} = (self let_result) in
      let modVarSet = ModVarSet.remove module_binder modVarSet in
      merge (get_fv_module_expr rhs) {modVarSet;moduleEnv;varSet}
    | E_module_accessor { module_path ; element } ->
      ignore element;
      {modVarSet = ModVarSet.of_list module_path (* not sure about that *) ;moduleEnv=VarMap.empty ;varSet=VarSet.empty}

  and get_fv_cases : matching_expr -> moduleEnv' = fun m ->
    match m with
    | Match_variant {cases;tv=_} ->
      let aux {constructor=_; pattern ; body} =
        let {modVarSet;moduleEnv;varSet} = get_fv_expr body in
        {modVarSet;moduleEnv;varSet=VarSet.remove pattern @@ varSet} in
      unions @@  List.map ~f:aux cases
    | Match_record {fields; body; tv = _} ->
      let pattern = LMap.values fields |> List.map ~f:fst in
      let {modVarSet;moduleEnv;varSet} = get_fv_expr body in
      {modVarSet;moduleEnv;varSet=List.fold_right pattern ~f:VarSet.remove ~init:varSet}

  and get_fv_module_expr : module_expr -> moduleEnv' =
    fun x ->
      match x.wrap_content with
      | M_struct prg -> get_fv_module prg
      | M_variable _ -> {modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}
      | M_module_path _ -> {modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}

  and get_fv_module : module_ -> moduleEnv' = fun m ->
    let aux = fun (x : declaration) ->
      match Location.unwrap x with
      | Declaration_constant {binder=_; expr ; attr=_} ->
        get_fv_expr expr
      | Declaration_module {module_binder=_;module_; module_attr=_} ->
        get_fv_module_expr module_
      | Declaration_type _t ->
        {modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}
    in
    unions @@ List.map ~f:aux m

  let expression e =
    let {modVarSet;moduleEnv=_;varSet} = get_fv_expr e in
    let fmv = ModVarSet.fold (fun v r -> v :: r) modVarSet [] in
    let fv = VarSet.fold (fun v r -> v :: r) varSet [] in
    (fmv, fv)
end
