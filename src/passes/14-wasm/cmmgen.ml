module I = Mini_c.Types
module O = Cmm.Types

module Value_var = Ligo_prim.Value_var

type env = {
  todo: int
}

(** 
 * Convert a variable to a string which we can use for symbols 
 *)
let var_to_string name =
let name, hash = Value_var.internal_get_name_and_counter name in
name ^ "#" ^ string_of_int hash

let unique_name name =
  let unique_name = Value_var.fresh ~name () in
  let name = var_to_string unique_name in
  name  

let o_type_expression: I.type_expression -> O.type_content -> O.type_expression = 
  fun t l ->
    O.{type_content = l; location = t.location; source_type = t.source_type}
  
let rec type_expression: I.type_expression -> O.type_expression = 
  fun t ->
    let tc = match t.type_content with 
    | I.T_tuple l             -> O.T_tuple (List.map ~f:annot_type l)
    | T_or (a, b)             -> T_or (annot_type a, annot_type b)
    | T_function (a, b)       -> T_function (type_expression a, type_expression b)
    | T_base tb               -> T_base (t_base tb)
    | T_map (a, b)            -> T_map (type_expression a, type_expression b)
    | T_big_map (a, b)        -> T_big_map (type_expression a, type_expression b)
    | T_list l                -> T_list (type_expression l)
    | T_set s                 -> T_set (type_expression s)
    | T_contract c            -> T_contract (type_expression c)
    | T_ticket t              -> T_ticket (type_expression t)
    | T_sapling_state s       -> T_sapling_state s
    | T_sapling_transaction s -> T_sapling_transaction s
    | T_option o              -> T_option (type_expression o)
    in 
    o_type_expression t tc

and annot_type: I.type_expression I.annotated -> O.type_expression O.annotated = fun (a, b) ->
    a, type_expression b

and t_base: I.type_base -> O.type_base = fun tb ->
  match tb with 
  | I.TB_unit               -> O.TB_unit
  | TB_bool                 -> TB_bool
  | TB_string               -> TB_string
  | TB_bytes                -> TB_bytes
  | TB_nat                  -> TB_nat
  | TB_int                  -> TB_int
  | TB_mutez                -> TB_mutez
  | TB_operation            -> TB_operation
  | TB_address              -> TB_address
  | TB_key                  -> TB_key
  | TB_key_hash             -> TB_key_hash
  | TB_chain_id             -> TB_chain_id
  | TB_signature            -> TB_signature
  | TB_timestamp            -> TB_timestamp
  | TB_baker_hash           -> TB_baker_hash
  | TB_pvss_key             -> TB_pvss_key
  | TB_baker_operation      -> TB_baker_operation
  | TB_bls12_381_g1         -> TB_bls12_381_g1
  | TB_bls12_381_g2         -> TB_bls12_381_g2
  | TB_bls12_381_fr         -> TB_bls12_381_fr
  | TB_never                -> TB_never
  | TB_chest                -> TB_chest
  | TB_chest_key            -> TB_chest_key
  | TB_tx_rollup_l2_address -> TB_tx_rollup_l2_address


let o_expression: I.expression -> O.expression_content -> O.expression = 
  fun t l ->
    O.{content = l; location = t.location; type_expression = type_expression t.type_expression}

let rec expression: I.expression -> O.expression * O.data list = fun e ->
  let content = match e.content with 
  | E_literal Literal_unit                  -> 
    O.C_int 0l, ([]: O.data list)
  | E_literal (Literal_int z)               -> 
    let n = unique_name "literal_int" in
    C_symbol n, [[O.Cdefine_symbol n; Cbigint z]]
  | E_literal (Literal_nat z)               -> 
    let n = unique_name "literal_nat" in
    C_symbol n, [[Cdefine_symbol n; Cbigint z]]
  | E_literal (Literal_timestamp z)         -> 
    let n = unique_name "literal_timestamp" in
    C_symbol n, [[Cdefine_symbol n; Cbigint z]]
  | E_literal (Literal_mutez z)             -> 
    let n = unique_name "literal_mutez" in
    C_symbol n, [[Cdefine_symbol n; Cbigint z]]
  | E_literal (Literal_string (Standard s)) -> 
    let n = unique_name "literal_string" in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_string (Verbatim s)) -> 
    let n = unique_name "literal_verbatim" in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_bytes b)             -> 
    let n = unique_name "literal_bytes" in
    let s = Bytes.to_string b in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_address s)           -> 
    let n = unique_name "literal_address" in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_signature s)         -> 
    let n = unique_name "literal_signature" in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_key s)               -> 
    let n = unique_name "literal_key" in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_key_hash s)          -> 
    let n = unique_name "literal_key_hash" in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_chain_id s)          -> 
    let n = unique_name "literal_chain_id" in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_operation b)         -> 
    let n = unique_name "literal_operation" in
    let s = Bytes.to_string b in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_bls12_381_g1 b)      -> 
    let n = unique_name "literal_bls12_381_g1" in
    let s = Bytes.to_string b in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_bls12_381_g2 b)      -> 
    let n = unique_name "literal_bls12_381_g2" in
    let s = Bytes.to_string b in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_bls12_381_fr b)      -> 
    let n = unique_name "literal_bls12_381_fr" in
    let s = Bytes.to_string b in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_chest b)             -> 
    let n = unique_name "literal_chest" in
    let s = Bytes.to_string b in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_literal (Literal_chest_key b)         -> 
    let n = unique_name "literal_chest_key" in
    let s = Bytes.to_string b in
    C_symbol n, [[Cdefine_symbol n; Cstring s]]
  | E_closure c -> 
    let body, data = expression c.body in
    E_closure {
      binder = c.binder;
      body;
    }, data
  | E_constant c -> 
    let arguments, data = List.fold ~f:(fun (args, data) i -> let e, d = expression i in (e :: args, d @ data) ) ~init:([], []) c.arguments in
    let arguments = List.rev arguments in
    let data = List.rev data in
    E_constant {
      cons_name = c.cons_name;
      arguments = arguments
    }, data
  | E_application (a, b) -> 
    let a, a_data = expression a in
    let b, b_data = expression b in
    E_application (a, b), (a_data @ b_data)
  | E_variable a -> E_variable a, []
  | E_iterator (c, ((v, t), e1), e2) -> 
    let e1, e1_data = expression e1 in 
    let e2, e2_data = expression e2 in 
    E_iterator (c, ((v, type_expression t), e1), e2), (e1_data @ e2_data)
  | E_fold (((v, t), e1), e2, e3) -> 
    let e1, e1_data = expression e1 in
    let e2, e2_data = expression e2 in
    let e3, e3_data = expression e3 in
    E_fold (((v, type_expression t), e1), e2, e3), (e1_data @ e2_data @ e3_data)
  | E_fold_right (((v, t), e1), (e2, t2), e3) -> 
    let e1, e1_data = expression e1 in
    let e2, e2_data = expression e2 in
    let e3, e3_data = expression e3 in
    E_fold_right (((v, type_expression t), e1), (e2, type_expression t2), e3), (e1_data @ e2_data @ e3_data)
  | E_if_bool (e1, e2, e3) -> 
    let e1, e1_data = expression e1 in
    let e2, e2_data = expression e2 in
    let e3, e3_data = expression e3 in
    E_if_bool (e1, e2, e3), (e1_data @ e2_data @ e3_data)
  | E_if_none (e1, e2, ((v, t), e)) -> 
    let e1, e1_data = expression e1 in
    let e2, e2_data = expression e2 in
    let e, e_data = expression e in
    E_if_none (e1, e2, ((v, type_expression t), e)), (e1_data @ e2_data @ e_data)
  | E_if_cons (e1, e2, (((v1, t1), (v2,t2)), e3)) -> 
    let e1, e1_data = expression e1 in
    let e2, e2_data = expression e2 in
    let e3, e3_data = expression e3 in
    E_if_cons (e1, e2, (((v1, type_expression t1), (v2, type_expression t2)), e3)), (e1_data @ e2_data @ e3_data)
  | E_if_left (e1, ((v, t1), e2), ((v1, t2), e3)) -> 
    let e1, e1_data = expression e1 in
    let e2, e2_data = expression e2 in
    let e3, e3_data = expression e3 in
    E_if_left (e1, ((v, type_expression t1), e2), ((v1, type_expression t2), e3)), (e1_data @ e2_data @ e3_data)
  | E_let_in (e1, i, ((v, t1), e2)) -> 
    let e1, e1_data = expression e1 in
    let e2, e2_data = expression e2 in
    E_let_in (e1, i, ((v, type_expression t1), e2)), (e1_data @ e2_data)
  | E_tuple l -> 
    let l, data = List.fold ~f:(fun (l, data) i -> let e, d = expression i in (e :: l, d @ data) ) ~init:([], []) l in
    let l = List.rev l in
    let data = List.rev data in
    E_tuple l, data
  | E_let_tuple (e, (l, e2)) -> 
    let l = List.map ~f:(fun (v, t) -> (v, type_expression t)) l in
    let e, e_data = expression e in
    let e2, e2_data = expression e2 in

    E_let_tuple (e, (l, e2)), (e_data @ e2_data)
  | E_proj (e1, a, b) -> 
    let e1, e1_data = expression e1 in
    E_proj (e1, a, b), e1_data
  | E_update (e1, i1, e2, i2) -> 
    let e1, e1_data = expression e1 in
    let e2, e2_data = expression e2 in
    E_update (e1, i1, e2, i2), (e1_data @ e2_data)
  | E_raw_michelson l -> E_raw_michelson l, []
  | E_raw_wasm (sl, w) -> E_raw_wasm (sl, w), []
  | E_global_constant (s, l) -> 
    let l, data = List.fold ~f:(fun (l, data) i -> let e, d = expression i in (e :: l, d @ data) ) ~init:([], []) l in
    E_global_constant (s, l), data
  | E_create_contract (t1, t2, ((v, t3), e1), el) -> 
    let e1, e1_data = expression e1 in
    let el, el_data = List.fold ~f:(fun (l, data) i -> let e, d = expression i in (e :: l, d @ data) ) ~init:([], []) el in
    E_create_contract (type_expression t1, type_expression t2, ((v, type_expression t3), e1), el), (e1_data @ el_data)
  in
  let content, data = content in 
  let e = o_expression e content in
  e, data