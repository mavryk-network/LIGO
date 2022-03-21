open Simple_utils
open Types

let assert_eq = fun a b -> if Caml.(=) a b then Some () else None
let assert_same_size = fun a b -> if (List.length a = List.length b) then Some () else None

let rec assert_type_expression_eq (a, b: (type_expression * type_expression)) : unit option =
  let open Option in
  match (a.type_content, b.type_content) with
  | T_app {type_operator=toa;arguments=lsta}, T_app {type_operator=tob;arguments=lstb} -> (
    if (TypeVar.equal toa tob) then (
      let* _ = assert_same_size lsta lstb in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> assert_type_expression_eq p) ~init:(Some ()) (List.zip_exn lsta lstb)
    ) else
      None
  )
  | T_app _, _ -> None
  | T_sum sa, T_sum sb -> (
      let sa' = LMap.to_kv_list_rev sa.fields in
      let sb' = LMap.to_kv_list_rev sb.fields in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let* _ = assert_eq ka kb in
        assert_type_expression_eq (va, vb)
      in
      let* _ = assert_same_size sa' sb' in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> aux p) ~init:(Some ()) (List.zip_exn sa' sb')
    )
  | T_sum _, _ -> None
  | T_record ra, T_record rb
       when Bool.(<>) (Helpers.is_tuple_lmap ra.fields) (Helpers.is_tuple_lmap rb.fields) -> None
  | T_record ra, T_record rb -> (
      let sort_lmap r' = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) r' in
      let ra' = sort_lmap @@ LMap.to_kv_list_rev ra.fields in
      let rb' = sort_lmap @@ LMap.to_kv_list_rev rb.fields in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let Label ka = ka in
        let Label kb = kb in
        let* _ = assert_eq ka kb in
        assert_type_expression_eq (va, vb)
      in
      let* _ = assert_eq ra.layout rb.layout in
      let* _ = assert_same_size ra' rb' in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> aux p) ~init:(Some ()) (List.zip_exn ra' rb')

    )
  | T_record _, _ -> None
  | T_arrow {type1;type2}, T_arrow {type1=type1';type2=type2'} ->
    let* _ = assert_type_expression_eq (type1, type1') in
    assert_type_expression_eq (type2, type2')
  | T_arrow _, _ -> None
  | T_variable x, T_variable y ->
     (* TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding *)
     if TypeVar.equal x y then Some () else None
  | T_variable _, _ -> None
  | T_module_accessor {module_name=mna;element=ea}, T_module_accessor {module_name=mnb;element=eb} when ModuleVar.equal mna mnb ->
    assert_type_expression_eq (ea, eb)
  | T_module_accessor _, _ -> None
  | T_singleton a , T_singleton b -> assert_literal_eq (a , b)
  | T_singleton _ , _ -> None
  | T_abstraction a , T_abstraction b ->
    assert_type_expression_eq (a.type_, b.type_) >>= fun _ ->
    Some (assert (equal_kind a.kind b.kind))
  | T_for_all a , T_for_all b ->
    assert_type_expression_eq (a.type_, b.type_) >>= fun _ ->
    Some (assert (equal_kind a.kind b.kind))
  | T_abstraction _ , _ -> None
  | T_for_all _ , _ -> None

(* and type_expression_eq ab = Option.is_some @@ assert_type_expression_eq ab *)

and assert_literal_eq (a, b : literal * literal) : unit option =
  match (a, b) with
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
  | Literal_operation opa, Literal_operation opb when Bytes.equal opa opb -> Some ()
  | Literal_operation _, _ -> None
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
  | Literal_bls12_381_g1 a, Literal_bls12_381_g1 b when Bytes.equal a b -> Some ()
  | Literal_bls12_381_g1 _, Literal_bls12_381_g1 _ -> None
  | Literal_bls12_381_g1 _, _ -> None
  | Literal_bls12_381_g2 a, Literal_bls12_381_g2 b when Bytes.equal a b -> Some ()
  | Literal_bls12_381_g2 _, Literal_bls12_381_g2 _ -> None
  | Literal_bls12_381_g2 _, _ -> None
  | Literal_bls12_381_fr a, Literal_bls12_381_fr b when Bytes.equal a b -> Some ()
  | Literal_bls12_381_fr _, Literal_bls12_381_fr _ -> None
  | Literal_bls12_381_fr _, _ -> None
  | Literal_chest a, Literal_chest b when Bytes.equal a b -> Some ()
  | Literal_chest _, Literal_chest _ -> None
  | Literal_chest _, _ -> None
  | Literal_chest_key a, Literal_chest_key b when Bytes.equal a b -> Some ()
  | Literal_chest_key _, Literal_chest_key _ -> None
  | Literal_chest_key _, _ -> None

(* TODO this was supposed to mean equality of _values_; if
   assert_value_eq (a, b) = Some (), then a and b should be values *)
let rec assert_value_eq (a, b: (expression * expression )) : unit option =
  match (a.expression_content , b.expression_content) with
  | E_literal a , E_literal b ->
    assert_literal_eq (a, b)
  | E_constant (ca) , E_constant (cb) when Caml.(=) ca.cons_name cb.cons_name -> (
      let lst = List.zip_exn ca.arguments cb.arguments in
      let all = List.map ~f:assert_value_eq lst in
      if List.exists ~f:(Option.is_none) all then None else Some ()
    )
  | E_constructor (ca), E_constructor (cb) when Caml.(=) ca.constructor cb.constructor -> (
      assert_value_eq (ca.element, cb.element)
    )
  | E_module_accessor {module_name=maa;element=a}, E_module_accessor {module_name=mab;element=b} when ModuleVar.equal maa mab -> (
      assert_value_eq (a,b)
  )
  | E_record sma, E_record smb -> (
      let aux _ a b =
        match a, b with
        | Some a, Some b -> assert_value_eq (a, b)
        | _ -> None
      in
      let all = LMap.merge aux sma smb in
      if    ((LMap.cardinal all) = (LMap.cardinal sma))
         || ((LMap.cardinal all) = (LMap.cardinal smb)) then
        Some ()
      else None
    )
  | E_record_update ura, E_record_update urb -> (
    match assert_value_eq (ura.record, urb.record) with
    | None -> None
    | Some () ->
      let aux (Label a,Label b) =
        assert (String.equal a b)
      in
      let () = aux (ura.path, urb.path) in
      assert_value_eq (ura.update,urb.update)
  )
  | E_record_update _, _ -> None
  | (E_ascription a ,  _b') -> assert_value_eq (a.anno_expr , b)
  | (_a' , E_ascription b) -> assert_value_eq (a , b.anno_expr)

  | (E_variable _, _) | (E_lambda _, _) | (E_type_abstraction _, _)
  | (E_application _, _) | (E_let_in _, _)
  | (E_type_in _, _) | (E_mod_in _, _) | (E_mod_alias _, _)
  | (E_raw_code _, _)
  | (E_recursive _,_) | (E_record_accessor _, _)
  | (E_matching _, _)
  | E_module_accessor _, _
   -> None

  | E_literal _ , _
  | E_constant _ , E_constant _
  | E_constant _ , _
  | E_constructor _, E_constructor _
  | E_record _, _
  | E_constructor _, _ ->
      None
