open Ast_core
open Simple_utils.Trace
open Stage_common

include Ast_core.PP

let map_lmap_t f map =
  LMap.map
    (fun ({associated_type;_} as field) ->
      let field' = f associated_type in
      {field with associated_type = field'})
    map

type ('a,'err,'warn) folder = raise:('err,'warn) raise -> 'a -> term -> 'a

let rec fold_term ~raise : ('a, 'err, 'warn) folder -> 'a -> term -> 'a = fun f init t ->
  let self = fold_term ~raise f in
  let idle = fun acc _ -> acc in
  let init = f ~raise init t in
  match t.term_content with
  | T_literal _ | T_variable _ | T_raw_code _ | T_module_accessor _ | T_type -> init
  | T_constant c -> Folds.constant self init c
  | T_application app -> Folds.application self init app
  | T_lambda l -> Folds.lambda self idle init l
  | T_ascription a -> Folds.ascription self idle init a
  | T_constructor c -> Folds.constructor self init c
  | T_matching {matchee=e; cases} -> (
    let res = self init e in
    let aux acc ({body ; _ }: _ match_case) = self acc body in
    let res = List.fold ~f:aux ~init:res cases in
    res
  )
  | T_record m -> Folds.record self init m
  | T_record_update ru -> Folds.record_update self init ru
  | T_record_accessor ra -> Folds.record_accessor self init ra
  | T_let_in { let_binder = _ ; rhs ; let_result ; attr=_ } -> (
      let res = self init rhs in
      let res = self res let_result in
      res
    )
  | T_mod_in mi ->
    let res = fold_term_in_module_expr self init mi.rhs in
    let res = self res mi.let_result in
    res
  | T_recursive r -> Folds.recursive self idle init r
  | T_assign a -> Folds.assign self idle init a
  | T_sum rows | T_prod rows -> 
    LMap.fold
      (fun _ { associated_type; _ } init -> self associated_type init)
      rows.fields
      init
  | T_arrow arrow -> Folds.arrow self init arrow
  | T_pi pi -> Folds.pi self self init pi


and fold_term_in_module_expr : ('a -> term -> 'a)  -> 'a -> module_expr -> 'a = fun self acc x ->
  match x.wrap_content with
  | M_struct decls ->
    List.fold
      ~f:( fun acc (x: declaration) ->
        match x.wrap_content with
        | Declaration_constant x -> self acc x.expr
        | Declaration_module x -> fold_term_in_module_expr self acc x.module_
        | Declaration_type _ ->  acc
      )
      ~init:acc
      decls
  | M_module_path _
  | M_variable _ -> acc

type ('err,'warn) term_mapper = raise:('err,'warn) raise -> term -> term

let rec map_term ~raise : ('err,'warn) term_mapper -> term -> term = fun f e ->
  let self = map_term ~raise f in
  let e' = f ~raise e in
  let return term_content = { e' with term_content } in
  match e'.term_content with
  | T_ascription ascr -> (
      let ascr = Maps.ascription self (fun a -> a) ascr in
      return @@ T_ascription ascr
    )
  | T_matching {matchee=e;cases} -> (
    let e' = self e in
    let aux { pattern ; body } =
      let body' = self body in
      { pattern ; body = body'}
    in
    let cases' = List.map ~f:aux cases in
    return @@ T_matching {matchee=e';cases=cases'}
  )
  | T_record_accessor acc -> (
      let acc = Maps.record_accessor self acc in
      return @@ T_record_accessor acc
    )
  | T_record m -> (
    let m' = LMap.map self m in
    return @@ T_record m'
  )
  | T_record_update ru -> (
    let ru = Maps.record_update self ru in
    return @@ T_record_update ru
  )
  | T_constructor c -> (
    let c = Maps.constructor self c in
    return @@ T_constructor c
  )
  | T_application app -> (
    let app = Maps.application self app in
    return @@ T_application app
  )
  | T_let_in { let_binder ; rhs ; let_result; attr } -> (
      let rhs = self rhs in
      let let_result = self let_result in
      return @@ T_let_in { let_binder ; rhs ; let_result; attr }
    )
  | T_mod_in  mi ->
    let rhs = map_term_in_module_expr self mi.rhs in
    let let_result = self mi.let_result in
    return @@ T_mod_in {mi with rhs;let_result}
  | T_lambda l -> (
      let l = Maps.lambda self (fun a -> a) l in
      return @@ T_lambda l
    )
  | T_recursive r ->
      let r = Maps.recursive self (fun a -> a) r in
      return @@ T_recursive r
  | T_constant c -> (
      let c = Maps.constant self c in
      return @@ T_constant c
    )
  | T_assign a ->
    let a = Maps.assign self (fun a -> a) a in
    return @@ T_assign a
  | T_sum { fields ; layout } ->
    let fields = map_lmap_t self fields in
    return @@ (T_sum { fields ; layout })
  | T_prod {fields ; layout} ->
    let fields = map_lmap_t self fields in
    return @@ T_prod {fields;layout}
  | T_arrow arr ->
    let arr = Maps.arrow self arr in
    return @@ T_arrow arr
  | T_pi pi ->
    let pi = Maps.pi self self pi in
    return @@ T_pi pi
  | T_literal _ | T_variable _ | T_raw_code _ | T_type | T_module_accessor _ as e' -> return e'

and map_term_in_declarations : (term -> term) -> module_ -> module_ = fun self xs ->
  List.map
    ~f:( fun (x: declaration) ->
      let return wrap_content = { x with wrap_content } in
      match x.wrap_content with
      | Declaration_constant x ->
        let expr = self x.expr in
        return (Declaration_constant { x with expr })
      | Declaration_module x ->
        let module_ = map_term_in_module_expr self x.module_ in
        return (Declaration_module { x with module_ })
      | Declaration_type _ -> x
    )
    xs

and map_term_in_module_expr : (term -> term) -> module_expr -> module_expr = fun self x ->
  let return wrap_content = { x with wrap_content } in
  match x.wrap_content with
  | M_struct decls ->
    let decls = map_term_in_declarations self decls in
    return (M_struct decls)
  | M_module_path _
  | M_variable _ -> x


type 'a fold_mapper = 'a -> term -> bool * 'a * term

let rec fold_map_term : type a . a fold_mapper -> a -> term -> a * term = fun f a e ->
  let self = fold_map_term f in
  let idle acc a =  (acc,a) in
  let (continue, init,e') = f a e in
  if (not continue) then (init,e')
  else
  let return term_content = { e' with term_content } in
  match e'.term_content with
  | T_ascription ascr -> (
      let (res,ascr) = Fold_maps.ascription self idle init ascr in
      (res, return @@ T_ascription ascr)
    )
  | T_matching {matchee=e;cases} -> (
      let (res,e') = self init e in
      let aux acc { pattern ; body } =
        let (res,body') = self acc body in
        (res,{ pattern ; body = body'})
      in
      let (res, cases') = List.fold_map ~f:aux ~init:res cases in
       (res, return @@ T_matching {matchee=e';cases=cases'})
    )
  | T_record m -> (
    let (res, m') = LMap.fold_map ~f:(fun _ e res -> self res e) ~init m in
    (res, return @@ T_record m')
  )
  | T_record_accessor acc -> (
      let (res, acc) = Fold_maps.record_accessor self init acc in
      (res, return @@ T_record_accessor acc)
    )
  | T_record_update ru -> (
    let res,ru = Fold_maps.record_update self init ru in
    (res, return @@ T_record_update ru)
  )
  | T_constructor c -> (
      let (res,c) = Fold_maps.constructor self init c in
      (res, return @@ T_constructor c)
  )
  | T_application app -> (
      let res,app = Fold_maps.application self init app in
      (res, return @@ T_application app)
    )
  | T_let_in { let_binder ; rhs ; let_result; attr } -> (
      let (res,rhs) = self init rhs in
      let (res,let_result) = self res let_result in
      (res, return @@ T_let_in { let_binder ; rhs ; let_result ; attr })
    )
  | T_mod_in  mi ->
    let res,rhs = fold_map_term_in_module_expr self init mi.rhs in
    let res,let_result = self res mi.let_result in
    (res, return @@ T_mod_in {mi with rhs;let_result})
  | T_lambda l -> (
      let res,l = Fold_maps.lambda self idle init l in
      ( res, return @@ T_lambda l)
    )
  | T_recursive r ->
      let res,r = Fold_maps.recursive self idle init r in
      ( res, return @@ T_recursive r)
  | T_constant c -> (
      let res,c = Fold_maps.constant self init c in
      (res, return @@ T_constant c)
    )
  | T_assign a ->
    let (res,a) = Fold_maps.assign self idle init a in
    (res, return @@ T_assign a)
  | T_sum { fields; layout } ->
    let acc,fields = LMap.fold_map
      ~f:(fun _ {associated_type; michelson_annotation; decl_pos} acc ->
        let acc, associated_type = self acc associated_type in
        (acc,({ associated_type; michelson_annotation; decl_pos }))) 
      ~init 
      fields 
    in
    (acc,return @@ T_sum {fields; layout})
  | T_prod { fields; layout } ->
    let acc,fields = LMap.fold_map
      ~f:(fun _ {associated_type; michelson_annotation; decl_pos} acc ->
        let acc, associated_type = self acc associated_type in
        (acc,({ associated_type; michelson_annotation; decl_pos }))) 
      ~init 
      fields 
    in
    (acc,return @@ T_prod {fields; layout})
  | T_arrow arr ->
    let acc, arr = Fold_maps.arrow self init arr in
    (acc, return @@ T_arrow arr)
  | T_pi { binder; result } ->
    let acc, binder = Fold_maps.binder self init binder in
    let acc, result = self acc result in
    (acc, return @@ T_pi { binder; result })
  | T_literal _ | T_variable _ | T_raw_code _ | T_type | T_module_accessor _ as e' -> (init, return e')

and fold_map_term_in_module_expr : type a . (a -> term -> a * term) -> a -> module_expr -> a * module_expr = fun self acc x ->
  let return r wrap_content = (r, { x with wrap_content }) in
  match x.wrap_content with
  | M_struct decls ->
    let res,decls = List.fold_map
      ~f:( fun acc (x: declaration) ->
        let return r wrap_content = (r, { x with wrap_content }) in
        match x.wrap_content with
        | Declaration_constant x ->
          let res,expr = self acc x.expr in
          return res (Declaration_constant { x with expr })
        | Declaration_module x ->
          let res,module_ = fold_map_term_in_module_expr self acc x.module_ in
          return res (Declaration_module { x with module_ })
        | Declaration_type _ -> (acc,x)
      )
      ~init:acc
      decls
    in
    return res (M_struct decls)
  | M_module_path _ as x -> return acc x
  | M_variable _ as x ->
    return acc x
