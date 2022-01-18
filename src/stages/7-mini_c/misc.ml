open Types

module Free_variables = struct

  type bindings = expression_variable list
  let var_equal = Location.equal_content ~equal:Var.equal
  let mem : bindings -> expression_variable -> bool = List.mem ~equal:var_equal
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let mem_count : expression_variable -> bindings -> int =
    fun x fvs ->
    List.length (List.filter ~f:(var_equal x) fvs)
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : expression_variable list -> bindings = fun x -> x

  let rec expression : bindings -> expression -> bindings = fun b e ->
    let self = expression b in
    match e.content with
    | E_literal _ -> empty
    | E_closure f -> lambda b f
    | E_constant (c) -> unions @@ List.map ~f:self c.arguments
    | E_application (f, x) -> unions @@ [ self f ; self x ]
    | E_variable n -> var_name b n
    | E_iterator (_, ((v, _), body), expr) ->
      unions [ expression (union (singleton v) b) body ;
               self expr ;
             ]
    | E_fold (((v, _), body), collection, initial) ->
      unions [ expression (union (singleton v) b) body ;
               self collection ;
               self initial ;
             ]
    | E_fold_right (((v, _), body), (collection,_elem_type), initial) ->
      unions [ expression (union (singleton v) b) body ;
               self collection ;
               self initial ;
             ]
    | E_if_bool (x, bt, bf) -> unions [ self x ; self bt ; self bf ]
    | E_if_none (x, bn, ((s, _), bs)) ->
      unions [ self x ;
               self bn ;
               expression (union (singleton s) b) bs ;
             ]
    | E_if_cons (x, bnil , (((h, _) , (t, _)) , bcons)) ->
      unions [ self x ;
               self bnil ;
               expression (unions [ singleton h ; singleton t ; b ]) bcons ;
             ]
    | E_if_left (x, ((l, _), bl), ((r, _), br)) ->
      unions [ self x ;
               expression (union (singleton l) b) bl ;
               expression (union (singleton r) b) br ;
             ]
    | E_let_in (expr, _ , ((v , _) , body) )->
      unions [ self expr ;
               expression (union (singleton v) b) body ;
             ]
    | E_tuple exprs ->
      unions (List.map ~f:self exprs)
    | E_let_tuple (expr, (fields , body)) ->
      unions [ self expr ;
               expression (unions (List.map ~f:(fun (x, _) -> singleton x) fields @ [b])) body
             ]
    | E_proj (expr, _i, _n) ->
      self expr
    | E_update (expr, _i, update, _n) ->
      unions [ self expr; self update ]
    | E_raw_michelson _ -> empty

  and var_name : bindings -> var_name -> bindings = fun b n ->
    if mem b n
    then empty
    else singleton n

  and lambda : bindings -> anon_function -> bindings = fun b l ->
    let b = union (singleton l.binder) b in
    expression b l.body

end

module Free_variables_term = struct

  type bindings = int Bindings.t
  let singleton : expression_variable -> bindings = fun s -> Bindings.add s 1 Bindings.empty
  let mem_count : expression_variable -> bindings -> int =
    fun x fvs ->
    match Bindings.find_opt x fvs with
    | None -> 0
    | Some n -> n
  let mem : bindings -> expression_variable -> bool = fun fvs x -> mem_count x fvs > 0
  let union : bindings -> bindings -> bindings = fun l r -> Bindings.union (fun _ n m -> Some (n + m)) l r
  let unions : bindings list -> bindings = List.fold_right ~f:union ~init:Bindings.empty
  let empty : bindings = Bindings.empty
  let of_list : expression_variable list -> bindings = fun x -> unions @@ List.map ~f:singleton x
  let to_list : bindings -> (expression_variable * int) list = fun l -> Bindings.to_kv_list l

  let rec expression : expression -> expression = fun e ->
    let self = expression in
    let return content fvs = { e with content ; fvs = Some fvs } in
    let unions' = unions in
    let unions : bindings option list -> bindings = List.fold_right ~f:(fun l r -> match l with | None -> r | Some l -> union l r) ~init:Bindings.empty in
    let unions : expression list -> bindings = fun l -> unions @@ List.map ~f:(fun { fvs ; _ } -> fvs) l in
    let extract { fvs ; _ } = match fvs with
      | None -> failwith "error extract"
      | Some fvs -> fvs in
    match e.content with
    | E_literal _ -> return e.content empty
    | E_closure { binder ; body } ->
       let body = self body in
       let fvs = extract body in
       return (E_closure { binder ; body }) @@ Bindings.remove binder fvs
    | E_constant { cons_name ; arguments } ->
       let arguments = List.map ~f:self arguments in
       return (E_constant { cons_name ; arguments }) @@ unions @@ arguments
    | E_application (f, x) ->
       let f = self f in
       let x = self x in
       return (E_application (f, x)) @@ unions @@ [ f ; x ]
    | E_variable n -> return e.content @@ singleton n
    | E_iterator (x, ((v, y), body), expr) ->
      let body = self body in
      let fvs_body = Bindings.remove v @@ extract body in
      let expr = self expr in
      let fvs_expr = extract expr in
      return (E_iterator (x, ((v, y), body), expr)) @@ unions' [ fvs_body ; fvs_expr ]
    | E_fold (((v, x), body), collection, initial) ->
      let body = self body in
      let fvs_body = Bindings.remove v @@ extract body in
      let collection = self collection in
      let fvs_collection = extract collection in
      let initial = self initial in
      let fvs_initial = extract initial in
      return (E_fold (((v, x), body), collection, initial)) @@ unions' [ fvs_body ; fvs_collection ; fvs_initial ]
    | E_fold_right (((v, x), body), (collection,_elem_type), initial) ->
      let body = self body in
      let fvs_body = Bindings.remove v @@ extract body in
      let collection = self collection in
      let fvs_collection = extract collection in
      let initial = self initial in
      let fvs_initial = extract initial in
      return (E_fold_right (((v, x), body), (collection,_elem_type), initial)) @@ unions' [ fvs_body ; fvs_collection ; fvs_initial ]
    | E_if_bool (x, bt, bf) ->
      let x = self x in
      let bt = self bt in
      let bf = self bf in
      return (E_if_bool (x, bt, bf)) @@ unions [ x ; bt ; bf ]
    | E_if_none (x, bn, ((s, w), bs)) ->
      let bs = self bs in
      let fvs_bs = Bindings.remove s @@ extract bs in
      let x = self x in
      let fvs_x = extract x in
      let bn = self bn in
      let fvs_bn = extract bn in
      return (E_if_none (x, bn, ((s, w), bs))) @@ unions' [ fvs_x ; fvs_bn ; fvs_bs ]
    | E_if_cons (x, bnil , (((h, v) , (t, w)) , bcons)) ->
      let bcons = self bcons in
      let fvs_bcons = Bindings.remove h @@ Bindings.remove t @@ extract bcons in
      let x = self x in
      let fvs_x = extract x in
      let bnil = self bnil in
      let fvs_bnil = extract bnil in
      return (E_if_cons (x, bnil , (((h, v) , (t, w)) , bcons))) @@ unions' [ fvs_x ; fvs_bnil ; fvs_bcons ]
    | E_if_left (x, ((l, v), bl), ((r, w), br)) ->
      let bl = self bl in
      let fvs_bl = Bindings.remove l @@ extract bl in
      let br = self br in
      let fvs_br = Bindings.remove r @@ extract br in
      let x = self x in
      let fvs_x = extract x in
      return (E_if_left (x, ((l, v), bl), ((r, w), br))) @@ unions' [ fvs_x ; fvs_bl ; fvs_br ]
    | E_let_in (expr, x , ((v , y) , body) )->
      let body = self body in
      let fvs_body = Bindings.remove v @@ extract body in
      let expr = self expr in
      let fvs_expr = extract expr in
      return (E_let_in (expr, x , ((v , y) , body) )) @@ unions' [ fvs_expr ; fvs_body ]
    | E_tuple exprs ->
      let exprs = List.map ~f:self exprs in
      return (E_tuple exprs) @@ unions exprs
    | E_let_tuple (expr, (fields , body)) ->
      (* let body = expression (unions' (List.map ~f:(fun (x, _) -> singleton x) fields @ [b])) body in *)
      let body = self body in
      let fvs_body = List.fold_right ~f:Bindings.remove ~init:(extract body) @@ List.map ~f:(fun (x, _) -> x) fields in
      let expr = self expr in
      let fvs_expr = extract expr in
      return (E_let_tuple (expr, (fields , body))) @@ unions' [ fvs_expr ; fvs_body ]
    | E_proj (expr, _i, _n) ->
      let expr = self expr in
      { e with content = (E_proj (expr, _i, _n)) ; fvs = expr.fvs }
    | E_update (expr, _i, update, _n) ->
      let expr = self expr in
      let update = self update in
      return (E_update (expr, _i, update, _n)) @@ unions [ expr ; update ]
    | E_raw_michelson _ -> return e.content @@ empty

end
