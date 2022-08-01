open Types


let assert_literal_eq (a, b : literal * literal) : unit option =
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

let rec assert_list_eq f = fun a b -> match (a,b) with
  | [], [] -> Some ()
  | [], _  -> None
  | _ , [] -> None
  | hda::tla, hdb::tlb -> Simple_utils.Option.(
    let* () = f hda hdb in
    assert_list_eq f tla tlb
  )


(* TODO this was supposed to mean equality of _values_; if
   assert_value_eq (a, b) = Some (), then a and b should be values *)
let rec assert_value_eq (a, b: (expression * expression )) : unit option =
  match (a.term_content , b.term_content) with
  | T_literal a , T_literal b ->
    assert_literal_eq (a, b)
  | T_constant (ca) , T_constant (cb) when Caml.(=) ca.cons_name cb.cons_name -> (
      let lst = List.zip_exn ca.arguments cb.arguments in
      let all = List.map ~f:assert_value_eq lst in
      if List.exists ~f:(Option.is_none) all then None else Some ()
    )
  | T_constructor (ca), T_constructor (cb) when Caml.(=) ca.constructor cb.constructor -> (
      assert_value_eq (ca.element, cb.element)
    )
  | T_module_accessor {module_path=maa;element=a}, T_module_accessor {module_path=mab;element=b} -> (
    let open Simple_utils.Option in
    let* _ = assert_value_eq (a, b) in
    assert_list_eq (fun a b -> if ModuleVar.equal a b then Some () else None) maa mab
  )
  | T_record sma, T_record smb -> (
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
  | T_record_update ura, T_record_update urb -> (
    match assert_value_eq (ura.record, urb.record) with
    | None -> None
    | Some () ->
      let aux (Label a,Label b) =
        assert (String.equal a b)
      in
      let () = aux (ura.path, urb.path) in
      assert_value_eq (ura.update,urb.update)
  )
  | T_record_update _, _ -> None
  | (T_ascription a ,  _b') -> assert_value_eq (a.anno_expr , b)
  | (_a' , T_ascription b) -> assert_value_eq (a , b.anno_expr)
  | (T_variable _, _) | (T_lambda _, _) 

  | (T_application _, _) | (T_let_in _, _) | (T_assign _, _)
  | (T_mod_in _, _)
  | (T_raw_code _, _)
  | (T_recursive _,_) | (T_record_accessor _, _)
  | (T_matching _, _)
  | T_module_accessor _, _
   -> None

  | T_literal _ , _
  | T_constant _ , T_constant _
  | T_constant _ , _
  | T_constructor _, T_constructor _
  | T_record _, _
  | T_constructor _, _ 
  | _ ->
      None
