module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Ligo_prim
open Types

(* TODO: does that need to be cleaned-up ? *)
module Free_variables = struct
  module Set = Value_var.Set

  let rec expression_content : bound:Set.t -> expression_content -> Set.t =
   fun ~bound ec ->
    let self ?(bound = bound) expr = expression ~bound expr in
    match ec with
    | E_lambda l -> lambda ~bound l
    | E_literal _ -> Set.empty
    | E_constant { arguments; _ } ->
      arguments |> List.map ~f:self |> Set.union_list
    | E_variable var ->
      (match Set.mem bound var with
      | true -> Set.empty
      | false -> Set.singleton var)
    | E_application { lamb; args } -> Set.union (self lamb) (self args)
    | E_constructor { element; _ } -> self element
    | E_record record -> record |> Map.map ~f:self |> Map.data |> Set.union_list
    | E_accessor { struct_; _ } -> self struct_
    | E_update { struct_; update; _ } -> Set.union (self struct_) (self update)
    | E_matching { matchee; cases; _ } ->
      Set.union (self matchee) (matching_expression ~bound cases)
    | E_let_in { let_binder; rhs; let_result; _ } ->
      Set.union
        (self ~bound:Set.(add bound (Binder.get_var let_binder)) let_result)
        (self rhs)
    | E_type_abstraction { type_binder = _; result } -> self result
    | E_mod_in { module_binder = _; rhs = _; let_result } -> self let_result
    | E_raw_code _ -> Set.empty
    | E_type_inst { type_ = _; forall } -> self forall
    | E_recursive { fun_name; lambda = l; _ } ->
      lambda ~bound:Set.(add bound fun_name) l
    | E_module_accessor _ -> Set.empty
    | E_let_mut_in { rhs; let_result; _ } ->
      Set.union (self let_result) (self rhs)
    | E_assign { expression; _ } -> self expression
    | E_deref _ -> Set.empty
    | E_for { binder; start; final; incr; f_body } ->
      let bound = Set.add bound binder in
      Set.union_list
        [ self start; self final; self ~bound incr; self ~bound f_body ]
    | E_for_each
        { fe_binder = binder, None; collection; fe_body; collection_type = _ }
      ->
      Set.union_list
        [ self collection; self ~bound:Set.(add bound binder) fe_body ]
    | E_for_each { fe_binder = binder, Some binder'; collection; fe_body; _ } ->
      let bound = Set.(union bound (of_list [ binder; binder' ])) in
      Set.union_list [ self collection; self ~bound fe_body ]
    | E_while { cond; body } -> Set.union (self cond) (self body)
    | E_skip -> Set.empty
    | E_cond { condition; then_clause; else_clause } ->
      Set.union_list [ self condition; self then_clause; self else_clause ]
    | E_sequence { expr1; expr2 } -> Set.union (self expr1) (self expr2)
    | E_set exprs | E_list exprs | E_tuple exprs ->
      exprs |> List.map ~f:self |> Set.union_list
    | E_map map_expr | E_big_map map_expr ->
      map_expr
      |> List.map ~f:(fun (expr1, expr2) -> Set.union (self expr1) (self expr2))
      |> Set.union_list


  and lambda : bound:Set.t -> (expr, ty_expr) Lambda.t -> Set.t =
   fun ~bound { binder; result; _ } ->
    let bound =
      if Param.is_imm binder
      then Set.add bound (Param.get_var binder)
      else bound
    in
    expression ~bound result


  and expression : bound:Set.t -> expression -> Set.t =
   fun ~bound e -> expression_content ~bound e.expression_content


  and matching_variant_case
      :  (bound:Set.t -> expression -> Set.t) -> bound:Set.t
      -> _ matching_content_case -> Set.t
    =
   fun f ~bound { constructor = _; pattern; body } ->
    f ~bound:Set.(add bound pattern) body


  and matching
      :  (bound:Set.t -> expression -> Set.t) -> bound:Set.t -> matching_expr
      -> Set.t
    =
   fun f ~bound m ->
    match m with
    | Match_variant { cases; tv = _ } ->
      cases |> List.map ~f:(matching_variant_case f ~bound) |> Set.union_list
    | Match_record { fields; body; tv = _ } ->
      let bound =
        Set.(
          union
            bound
            (of_list (fields |> Map.data |> List.map ~f:Binder.get_var)))
      in
      f ~bound body


  and matching_expression ~bound match_expr =
    matching expression ~bound match_expr


  type bindings = Ligo_prim.Value_var.t list

  let lambda bindings l =
    let bound = Set.of_list bindings in
    lambda ~bound l |> Set.to_list
end

let assert_eq a b = if Caml.( = ) a b then Some () else None

let assert_same_size a b =
  if List.length a = List.length b then Some () else None


let rec assert_list_eq f a b =
  let open Option.Let_syntax in
  match a, b with
  | [], [] -> Some ()
  | [], _ -> None
  | _, [] -> None
  | hda :: tla, hdb :: tlb ->
    let%bind () = f hda hdb in
    assert_list_eq f tla tlb


let constant_compare ia ib = Literal_types.compare ia ib

(* Alistair: Morally, I'm against this. Needs to be rewritten into [equal_type_expression] *)
let rec assert_type_expression_eq ((a, b) : type_expression * type_expression)
    : unit option
  =
  let open Option.Let_syntax in
  match a.type_content, b.type_content with
  | T_tuple tuple1, T_tuple tuple2 ->
    assert_list_eq (fun a b -> assert_type_expression_eq (a, b)) tuple1 tuple2
  | ( T_constant { language = la; injection = ia; parameters = lsta }
    , T_constant { language = lb; injection = ib; parameters = lstb } ) ->
    if String.equal la lb && constant_compare ia ib = 0
    then (
      let%bind _ = assert_same_size lsta lstb in
      List.fold_left
        ~f:(fun acc p ->
          match acc with
          | None -> None
          | Some () -> assert_type_expression_eq p)
        ~init:(Some ())
        (List.zip_exn lsta lstb))
    else None
  | T_constant _, _ -> None
  | T_sum row1, T_sum row2 | T_record row1, T_record row2 ->
    Option.some_if
      (Map.equal
         (fun (row_elem1 : _ Rows.Elem.t) row_elem2 ->
           assert_type_expression_eq
             (row_elem1.content.associated_type, row_elem2.content.associated_type)
           |> Option.is_some)
         row1.fields
         row2.fields)
      ()
  | T_sum _, _ | T_record _, _ -> None
  | T_arrow { type1; type2 }, T_arrow { type1 = type1'; type2 = type2' } ->
    let%bind _ = assert_type_expression_eq (type1, type1') in
    assert_type_expression_eq (type2, type2')
  | T_arrow _, _ -> None
  | T_variable x, T_variable y ->
    (* TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding *)
    if Type_var.equal x y then Some () else None
  | T_variable _, _ -> None
  | T_singleton a, T_singleton b -> assert_literal_eq (a, b)
  | T_singleton _, _ -> None
  | T_abstraction a, T_abstraction b ->
    let%bind _ = assert_type_expression_eq (a.type_, b.type_) in
    Some (assert (Kind.equal a.kind b.kind))
  | T_for_all a, T_for_all b ->
    let%bind _ = assert_type_expression_eq (a.type_, b.type_) in
    Some (assert (Kind.equal a.kind b.kind))
  | T_abstraction _, _ -> None
  | T_for_all _, _ -> None
  | T_tuple _, _ -> None


and type_expression_eq ab = Option.is_some @@ assert_type_expression_eq ab

and assert_literal_eq ((a, b) : Literal_value.t * Literal_value.t) : unit option
  =
  match a, b with
  | Literal_int a, Literal_int b when Z.equal a b -> Some ()
  | Literal_int _, Literal_int _ -> None
  | Literal_int _, _ -> None
  | Literal_nat a, Literal_nat b when Z.equal a b -> Some ()
  | Literal_nat _, Literal_nat _ -> None
  | Literal_nat _, _ -> None
  | Literal_timestamp a, Literal_timestamp b when Z.equal a b -> Some ()
  | Literal_timestamp _, Literal_timestamp _ -> None
  | Literal_timestamp _, _ -> None
  | Literal_mutez a, Literal_mutez b when Z.equal a b -> Some ()
  | Literal_mutez _, Literal_mutez _ -> None
  | Literal_mutez _, _ -> None
  | Literal_string a, Literal_string b when Ligo_string.equal a b -> Some ()
  | Literal_string _, Literal_string _ -> None
  | Literal_string _, _ -> None
  | Literal_bytes a, Literal_bytes b when Bytes.equal a b -> Some ()
  | Literal_bytes _, Literal_bytes _ -> None
  | Literal_bytes _, _ -> None
  | Literal_unit, Literal_unit -> Some ()
  | Literal_unit, _ -> None
  | Literal_address a, Literal_address b when String.equal a b -> Some ()
  | Literal_address _, Literal_address _ -> None
  | Literal_address _, _ -> None
  | Literal_signature a, Literal_signature b when String.equal a b -> Some ()
  | Literal_signature _, Literal_signature _ -> None
  | Literal_signature _, _ -> None
  | Literal_key a, Literal_key b when String.equal a b -> Some ()
  | Literal_key _, Literal_key _ -> None
  | Literal_key _, _ -> None
  | Literal_key_hash a, Literal_key_hash b when String.equal a b -> Some ()
  | Literal_key_hash _, Literal_key_hash _ -> None
  | Literal_key_hash _, _ -> None
  | Literal_chain_id a, Literal_chain_id b when String.equal a b -> Some ()
  | Literal_chain_id _, Literal_chain_id _ -> None
  | Literal_chain_id _, _ -> None
  | Literal_operation _, Literal_operation _ -> None
  | Literal_operation _, _ -> None
  | Literal_bls12_381_g1 a, Literal_bls12_381_g1 b when Bytes.equal a b ->
    Some ()
  | Literal_bls12_381_g1 _, Literal_bls12_381_g1 _ -> None
  | Literal_bls12_381_g1 _, _ -> None
  | Literal_bls12_381_g2 a, Literal_bls12_381_g2 b when Bytes.equal a b ->
    Some ()
  | Literal_bls12_381_g2 _, Literal_bls12_381_g2 _ -> None
  | Literal_bls12_381_g2 _, _ -> None
  | Literal_bls12_381_fr a, Literal_bls12_381_fr b when Bytes.equal a b ->
    Some ()
  | Literal_bls12_381_fr _, Literal_bls12_381_fr _ -> None
  | Literal_bls12_381_fr _, _ -> None
  | Literal_chest a, Literal_chest b when Bytes.equal a b -> Some ()
  | Literal_chest _, Literal_chest _ -> None
  | Literal_chest _, _ -> None
  | Literal_chest_key a, Literal_chest_key b when Bytes.equal a b -> Some ()
  | Literal_chest_key _, Literal_chest_key _ -> None
  | Literal_chest_key _, _ -> None


let get_entry (lst : program) (name : Value_var.t) : expression option =
  let aux x =
    match Location.unwrap x with
    | D_value
        { binder
        ; expr
        ; attr =
            { inline = _
            ; no_mutation = _
            ; view = _
            ; public = _
            ; hidden = _
            ; thunk = _
            }
        } ->
      if Binder.apply (Value_var.equal name) binder then Some expr else None
    | D_type _ | D_module _ -> None
  in
  List.find_map ~f:aux (List.rev lst)


let get_type_of_contract ty =
  match ty with
  | T_arrow { type1; type2 } ->
    (match type1.type_content, type2.type_content with
    | T_tuple [ parameter; storage ], T_tuple [ listop; storage' ]  ->
      let open Option.Let_syntax in
      let%bind () = Combinators.assert_t_list_operation listop in
      let%bind () = assert_type_expression_eq (storage, storage') in
      (* TODO: on storage/parameter : asert_storable, assert_passable ? *)
      return (parameter, storage)
    | _ -> None)
  | _ -> None
