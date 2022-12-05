(* use https://github.com/SanderSpies/ocaml/blob/manual_gc/asmcomp/wasm32/emit.mlp for inspiration *)

[@@@warning "-33-27-26-39"]

open Trace
open Errors
module I = Mini_c.Types
module W = WasmObjectFile
module A = W.Ast
module T = W.Types
module S = W.Source
module Z = Z
module Value_var = Ligo_prim.Value_var
module Location = Simple_utils.Location
open Helpers
open Validation

(**
 * Grouped instructions for partial calls.
 *)
module Partial_call = struct
  let create_memory_block env at ~f ~no_of_args ~current_args =
    let const = const at in
    let call_s = call_s at in
    let local_set_s = local_set_s at in
    let local_get_s = local_get_s at in
    let store = store at in
    let load = load at in
    let i32_add = i32_add at in
    let func_alloc_name = unique_name "partial_call" in
    let env = Env.add_local env (func_alloc_name, NumType I32Type) in
    ( env
    , func_alloc_name
    , [ const Int32.(12l + Int32.of_int_exn no_of_args)
      ; call_s "malloc"
      ; local_set_s func_alloc_name
      ; local_get_s func_alloc_name
      ]
      @ f
      @ [ store
        ; local_get_s func_alloc_name
        ; const 4l
        ; i32_add
        ; const (Int32.of_int_exn no_of_args)
        ; store
        ; local_get_s func_alloc_name
        ; const 8l
        ; i32_add
        ; const (Int32.of_int_exn (List.length current_args))
        ; store
        ] )


  let create_helper w at ~name ~no_of_args =
    let open A in
    let func_name = unique_name ("helper_" ^ name) in
    let const = const at in
    let call_s = call_s at in
    let local_set_s = local_set_s at in
    let local_get_s = local_get_s at in
    let store = store at in
    let load = load at in
    let i32_add = i32_add at in
    let exists =
      List.find
        ~f:(fun a ->
          match a.it with
          | FuncSymbol n -> String.equal n.name func_name
          | FuncNoSymbol _ -> false)
        w.funcs
    in
    match exists with
    | Some _ -> func_name, w
    | None ->
      let type_name = "i32_" ^ string_of_int no_of_args ^ "_type" in
      let t =
        A.TypeSymbol
          { tname = type_name
          ; tdetails = FuncType ([ NumType I32Type ], [ NumType I32Type ])
          }
      in
      let t = S.{ it = t; at } in
      let f : A.func' =
        FuncSymbol
          { name = func_name
          ; ftype = type_name
          ; locals = [ "arg", NumType I32Type ]
          ; body =
              List.fold_left
                ~f:(fun all i ->
                  all
                  @ [ local_get_s "arg"; const Int32.(of_int_exn i * 4l); i32_add; load ])
                ~init:[]
                (List.init no_of_args ~f:(fun i -> i))
              @ [ call_s name ]
          }
      in
      let f = S.{ it = f; at } in
      let s = A.{ name = func_name; details = Function } in
      let s = S.{ it = s; at } in
      ( func_name
      , { w with funcs = f :: w.funcs; types = t :: w.types; symbols = s :: w.symbols } )


  let memory_copy env at ~func_name ~total_args_length ~dest =
    let const = const at in
    let call_s = call_s at in
    let local_set_s = local_set_s at in
    let local_get_s = local_get_s at in
    let local_tee_s = local_tee_s at in
    let store = store at in
    let i32_add = i32_add at in
    let i32_mul = i32_mul at in
    let load = load at in
    let memory_copy = memory_copy at in
    [ const 12l
    ; local_get_s total_args_length
    ; const 4l
    ; i32_mul
    ; i32_add
    ; call_s "malloc"
    ; local_tee_s dest
    ; local_get_s func_name
    ; const 12l
    ; local_get_s total_args_length
    ; const 4l
    ; i32_mul
    ; i32_add
    ; memory_copy
    ]
end

(* The data offset. This indicates where a block of data should be placed in the linear memory. *)
(* TODO: move this to env *)
let global_offset = ref 0l

(**
 * Convert a Zarith number (used by LIGO and Tezos) to a wasm memory representation.
 *)
let convert_to_memory
    : S.region -> int -> string -> Z.t -> A.data_segment list * A.sym_info list
  =
 fun at header name z ->
  let z = Z.to_int32 z in
  let open Int32 in
  let data =
    A.[ data ~offset:!global_offset ~init:{ name; detail = [ Int8 header; Int32 z ] } ]
  in
  let symbols = A.[ symbol_data ~name ~index:0l ~size:5l ~offset:!global_offset ] in
  global_offset := !global_offset + 5l;
  data, symbols


type locals = (string * T.value_type) list

let func_symbol_type w symbol =
  match
    List.find
      ~f:(fun f ->
        match f.it with
        | FuncSymbol { name; _ } -> String.equal name symbol
        | _ -> false)
      w.A.funcs
  with
  | Some { it = FuncSymbol { ftype; _ } as fs; _ } ->
    let t =
      List.find
        ~f:(fun f ->
          match f.it with
          | TypeSymbol { tname; _ } -> String.equal tname ftype
          | _ -> false)
        w.A.types
    in
    (match t with
    | Some t -> Some (fs, t.it)
    | None -> None)
  | _ -> None


let rec expression ~raise
    : A.module_' -> Env.t -> I.expression -> A.module_' * Env.t * A.instr list
  =
 fun w env e ->
  let at = location_to_region e.location in
  let const = const at in
  let call_s = call_s at in
  let call_indirect_s = call_indirect_s at in
  let local_set_s = local_set_s at in
  let local_get_s = local_get_s at in
  let local_tee_s = local_tee_s at in
  let load = load at in
  let store = store at in
  let i32_add = i32_add at in
  let i32_mul = i32_mul at in
  let i32_ne = i32_ne at in
  let i32_eq = i32_eq at in
  let i32_lt = i32_lt at in
  let i32_gt = i32_gt at in
  let data_symbol = data_symbol at in
  let func_symbol = func_symbol at in
  let elem = elem at in
  let compare_eq = compare_eq at in
  let if_ = if_ at in
  let br_if = br_if at in
  let br = br at in
  let loop = loop at in
  let nop = nop at in
  let store8 = store8 at in
  let i32_eqz = i32_eqz at in
  let unreachable = unreachable at in
  let memory_copy = memory_copy at in
  let i32_and = i32_and at in
  let convert_to_memory = convert_to_memory at in
  let add_local = Env.add_local in
  let add_locals = Env.add_locals in
  let host_call
      :  fn:string -> response_size:int32 -> instructions:I.expression list
      -> A.module_' * Env.t * A.instr list
    =
   fun ~fn ~response_size ~instructions ->
    let new_value = var_to_string (Value_var.fresh ~name:fn ()) in
    let w, env, e =
      List.fold_left
        ~f:(fun all (a : I.expression) ->
          let w, env, e = all in
          let w, env, e2 = expression ~raise w env a in
          w, env, e @ e2)
        ~init:(w, env, [])
        instructions
    in
    ( w
    , add_local env (new_value, T.NumType I32Type)
    , S.[ const response_size; call_s "malloc"; local_tee_s new_value ]
      @ e
      @ S.[ call_s fn; store; local_get_s new_value ] )
  in
  let uni_op op w env e1 =
    let w, env, e1 = expression ~raise w env e1 in
    let env, e = op env e1 in
    w, env, e
  in
  let bin_op op w env e1 e2 =
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    let env, e = op env e1 e2 in
    w, env, e
  in
  let unique_name name =
    let unique_name = Value_var.fresh ~name () in
    let name = var_to_string unique_name in
    name
  in
  let int_like name header z =
    let name = unique_name name in
    let data, symbols = convert_to_memory header name z in
    let w = { w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    w, env, [ data_symbol name ]
  in
  let string_like name header s =
    let name = unique_name name in
    let s_len = Int32.of_int_exn (String.length s) in
    let a_len = String.length s + 5 in
    (* tag + size *)
    let a_len = Int32.of_int_exn a_len in
    let data =
      [ data
          ~offset:!global_offset
          ~init:{ name; detail = [ Int8 header; Int32 s_len; String s ] }
      ]
    in
    let symbols = [ symbol_data ~name ~index:0l ~size:a_len ~offset:!global_offset ] in
    (global_offset := Int32.(!global_offset + a_len));
    let w = { w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    w, env, [ data_symbol name ]
  in
  let bytes_like name b =
    let name = unique_name name in
    let data = [ data ~offset:!global_offset ~init:{ name; detail = [ Bytes b ] } ] in
    let symbols =
      [ symbol_data
          ~name
          ~index:0l
          ~size:(Int32.of_int_exn (Bytes.length b))
          ~offset:!global_offset
      ]
    in
    (global_offset := Int32.(!global_offset + Int32.of_int_exn (Bytes.length b)));
    let w = { w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    w, env, [ data_symbol name ]
  in
  (*
    TODO: change/add tags to:

    false     = 0
    true      = 1
    int       = 2
    string    = 4
    tuple     = 5
    list      = 6 
    set       = 7

    TODO: improve tuple handling, currently mostly a pair

    packaging format
    ===
    tag + info


    ---

    nat       = 3
    timestamp = 4
    mutez     = 5
    string    = 6
    bytes     = 7 
    address   = 8
    signature = 9
    key       = 10
    key_hash  = 11
    chain_id  = 12
    operation = 13
    bls12_381_g1 = 14
    bls12_381_g2 = 15
    bls12_381_fr = 16
    chest = 17
    chest_key = 18
  *)
  match e.content with
  | E_literal Literal_unit -> w, env, [ const 0l ]
  | E_literal (Literal_int z) -> int_like "Literal_int" 2 z
  | E_literal (Literal_nat z) -> int_like "Literal_nat" 2 z
  | E_literal (Literal_timestamp z) -> int_like "Literal_timestamp" 2 z
  | E_literal (Literal_mutez z) -> int_like "Literal_mutez" 2 z
  | E_literal (Literal_string (Standard s)) -> string_like "Literal_string" 4 s
  | E_literal (Literal_string (Verbatim s)) -> string_like "Literal_string" 4 s
  | E_literal (Literal_bytes b) -> bytes_like "Literal_bytes" b
  | E_literal (Literal_address s) -> string_like "Literal_address" 4 s
  | E_literal (Literal_signature s) -> string_like "Literal_signature" 4 s
  | E_literal (Literal_key s) -> string_like "Literal_key" 4 s
  | E_literal (Literal_key_hash s) -> string_like "Literal_key_hash" 4 s
  | E_literal (Literal_chain_id s) -> string_like "Literal_chain_id" 4 s
  | E_literal (Literal_operation b) -> bytes_like "Literal_operation" b
  | E_literal (Literal_bls12_381_g1 b) -> bytes_like "Literal_bls12_381_g1" b
  | E_literal (Literal_bls12_381_g2 b) -> bytes_like "Literal_bls12_381_g2" b
  | E_literal (Literal_bls12_381_fr b) -> bytes_like "Literal_bls12_381_fr" b
  | E_literal (Literal_chest b) -> bytes_like "Literal_chest" b
  | E_literal (Literal_chest_key b) -> bytes_like "Literal_chest_key" b
  | E_closure { binder; body } -> raise.error (not_supported e)
  (* Loops *)
  | E_constant { cons_name = C_ITER; arguments = [ func; iter ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_LOOP_LEFT; arguments = [ func; init ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_FOLD; arguments = [ func; list; init ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_FOLD_LEFT; arguments = [ func; init; col ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_FOLD_RIGHT; arguments = [ func; col; init ] } ->
    raise.error (not_supported e)
  (* MATH *)
  | E_constant { cons_name = C_NEG; arguments = [ e1 ] } ->
    uni_op Datatype.Int.neg w env e1
  | E_constant { cons_name = C_ADD; arguments = [ e1; e2 ] } ->
    bin_op Datatype.Int.add w env e1 e2
  | E_constant { cons_name = C_SUB; arguments = [ e1; e2 ] } ->
    bin_op Datatype.Int.sub w env e1 e2
  | E_constant { cons_name = C_MUL; arguments = [ e1; e2 ] } ->
    bin_op Datatype.Int.mul w env e1 e2
  | E_constant { cons_name = C_DIV; arguments = [ e1; e2 ] } ->
    bin_op Datatype.Int.div w env e1 e2
  | E_constant { cons_name = C_MOD; arguments = [ e1; e2 ] } ->
    raise.error (not_supported e)
  (* LOGIC *)
  | E_constant { cons_name = C_NOT; arguments = [ e1; e2 ] } ->
    (* TODO: CHECK: is this the same?? *)
    uni_op Datatype.Int.neg w env e1
  | E_constant { cons_name = C_AND; arguments = [ e1; e2 ] } ->
    bin_op Datatype.Int.and_ w env e1 e2
  | E_constant { cons_name = C_OR; arguments = [ e1; e2 ] } ->
    bin_op Datatype.Int.or_ w env e1 e2
  | E_constant { cons_name = C_XOR; arguments = [ e1; e2 ] } ->
    bin_op Datatype.Int.xor w env e1 e2
  | E_constant { cons_name = C_LSL; arguments = [ e1; e2 ] } ->
    bin_op Datatype.Int.lsl_ w env e1 e2
  | E_constant { cons_name = C_LSR; arguments = [ e1; e2 ] } ->
    bin_op Datatype.Int.lsr_ w env e1 e2
  (* COMPARATOR *)
  | E_constant { cons_name = C_EQ; arguments = [ e1; e2 ] } ->
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    w, env, e1 @ e2 @ [ call_s "compare"; i32_eqz ]
  | E_constant { cons_name = C_NEQ; arguments = [ e1; e2 ] } ->
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    w, env, e1 @ e2 @ [ call_s "compare"; i32_eqz; const 0l; i32_eq ]
  | E_constant { cons_name = C_LT; arguments = [ e1; e2 ] } ->
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    w, env, e1 @ e2 @ [ call_s "compare"; const (-1l); i32_eq ]
  | E_constant { cons_name = C_GT; arguments = [ e1; e2 ] } ->
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    w, env, e1 @ e2 @ [ call_s "compare"; const 1l; i32_eq ]
  | E_constant { cons_name = C_LE; arguments = [ e1; e2 ] } ->
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    w, env, e1 @ e2 @ [ call_s "compare"; const 1l; i32_lt ]
  | E_constant { cons_name = C_GE; arguments = [ e1; e2 ] } ->
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    w, env, e1 @ e2 @ [ call_s "compare"; const 0l; i32_gt ]
  (* Bytes/ String *)
  | E_constant { cons_name = C_CONCAT; arguments = [ e1; e2 ] } ->
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    w, env, e1 @ e2 @ [ call_s "__ligo_internal__string_concat" ]
  | E_constant { cons_name = C_SLICE; arguments = [ offset; len; str ] } ->
    let w, env, offset = expression ~raise w env offset in
    let w, env, len = expression ~raise w env len in
    let w, env, str = expression ~raise w env str in
    w, env, offset @ len @ str @ [ call_s "__ligo_internal__string_slice" ]
  | E_constant { cons_name = C_SIZE; arguments = [ s ] } ->
    let w, env, s = expression ~raise w env s in
    let size = unique_name "size" in
    let env = add_locals env [ size, T.NumType I32Type ] in
    ( w
    , env
    , [ const 5l
      ; call_s "malloc"
      ; local_tee_s size
      ; const 2l
      ; store8
      ; local_get_s size
      ; const 1l
      ; i32_add
      ]
      @ s
      @ [ const 1l; i32_add; load; store; local_get_s size ] )
  | E_constant { cons_name = C_CONS; arguments = [ l1; l2 ] } ->
    let cons = var_to_string (Value_var.fresh ~name:"C_CONS" ()) in
    let w, env, l1 = expression ~raise w env l1 in
    let w, env, l2 = expression ~raise w env l2 in
    ( w
    , add_local env (cons, T.NumType I32Type)
    , [ const 9l
      ; call_s "malloc"
      ; local_tee_s cons
      ; const 9l (* list tag *)
      ; store8
      ; local_get_s cons
      ; const 1l
      ; i32_add
      ]
      @ l1
      @ [ store; local_get_s cons; const 5l; i32_add ]
      @ l2
      @ [ store; local_get_s cons ] )
  (* Pair *)
  | E_constant { cons_name = C_PAIR; arguments = [ e1; e2 ] } ->
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    let pair = unique_name "c_pair" in
    let env = add_locals env [ pair, T.NumType I32Type ] in
    let e =
      [ const 9l
      ; call_s "malloc"
      ; local_set_s pair
      ; local_get_s pair
      ; const 5l (* pair tag *)
      ; store8
      ; local_get_s pair
      ; const 1l
      ; i32_add
      ]
      @ e1
      @ [ store; local_get_s pair; const 5l; i32_add ]
      @ e2
      @ [ store; local_get_s pair ]
    in
    w, env, e
  | E_constant { cons_name = C_CAR; arguments = [ cons ] } ->
    let w, env, cons = expression ~raise w env cons in
    w, env, cons @ [ const 4l; i32_add; load ]
  | E_constant { cons_name = C_CDR; arguments = [ cons ] } ->
    let w, env, cons = expression ~raise w env cons in
    w, env, cons @ [ const 8l; i32_add; load ]
  | E_constant { cons_name = C_TRUE; arguments = [] } -> w, env, [ const 1l ]
  | E_constant { cons_name = C_FALSE; arguments = [] } -> w, env, [ const 0l ]
  (* Set *)
  | E_constant { cons_name = C_SET_EMPTY; arguments = [] } ->
    w, env, [ const 12l (* C_SET_EMPTY *) ]
  | E_constant { cons_name = C_SET_LITERAL; arguments = [ e1 ] } ->
    host_call ~fn:"c_set_literal" ~response_size:4l ~instructions:[ e1 ]
  | E_constant { cons_name = C_SET_ADD; arguments = [ key; set ] } ->
    let w, env, key_e = expression ~raise w env key in
    let w, env, set_e = expression ~raise w env set in
    w, env, set_e @ key_e @ [ call_s "__ligo_internal__set_add" ]
  | E_constant { cons_name = C_SET_SIZE; arguments = [ s ] } ->
    let w, env, s = expression ~raise w env s in
    w, env, s @ [ call_s "__ligo_internal__set_size" ]
  | E_constant { cons_name = C_SET_REMOVE; arguments = [ item; set ] } ->
    let w, env, item_e = expression ~raise w env item in
    let w, env, set_e = expression ~raise w env set in
    w, env, set_e @ item_e @ [ const 20l; call_s "__ligo_internal__set_remove" ]
  | E_constant { cons_name = C_SET_ITER; arguments = [ func; set ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_SET_FOLD; arguments = [ func; set; init ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_SET_FOLD_DESC; arguments = [ func; set; init ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_SET_MEM; arguments = [ key; set ] } ->
    let w, env, key_e = expression ~raise w env key in
    let w, env, set_e = expression ~raise w env set in
    w, env, set_e @ key_e @ [ call_s "__ligo_internal__set_mem" ]
  | E_constant { cons_name = C_SET_UPDATE; arguments = [ key; boolean; set ] } ->
    let w, env, key_e = expression ~raise w env key in
    let w, env, set_e = expression ~raise w env set in
    let w, env, boolean_e = expression ~raise w env boolean in
    ( w
    , env
    , set_e @ boolean_e @ key_e @ [ const 20l; call_s "__ligo_internal__set_update" ] )
  (* List *)
  | E_constant { cons_name = C_LIST_EMPTY; arguments = [] } ->
    w, env, [ const 10l ] (* C_LIST_EMPTY *)
  | E_constant { cons_name = C_LIST_LITERAL; arguments = [ e1 ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_LIST_ITER; arguments = [ func; list ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_LIST_MAP; arguments = [ func; list ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_LIST_SIZE; arguments = [ list_ ] } ->
    let w, env, list_e = expression ~raise w env list_ in
    w, env, list_e @ [ call_s "__ligo_internal__list_size" ]
  | E_constant { cons_name = C_LIST_FOLD; arguments = [ func; list; init ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_LIST_FOLD_LEFT; arguments = [ func; init; list ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_LIST_FOLD_RIGHT; arguments = [ func; list; init ] } ->
    raise.error (not_supported e)
  (* Maps *)
  | E_constant { cons_name = C_MAP_EMPTY; arguments = [] } ->
    w, env, [ const 12l (* C_SET_EMPTY *) ]
  | E_constant { cons_name = C_MAP_LITERAL; arguments = [ e1 ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_MAP_ADD; arguments = [ key; value; map ] } ->
    let w, env, map_e = expression ~raise w env map in
    let w, env, key_e = expression ~raise w env key in
    let w, env, value_e = expression ~raise w env value in
    w, env, map_e @ key_e @ value_e @ [ call_s "__ligo_internal__map_add" ]
  | E_constant { cons_name = C_MAP_FIND_OPT; arguments = [ key; map ] } ->
    let w, env, key_e = expression ~raise w env key in
    let w, env, map_e = expression ~raise w env map in
    w, env, map_e @ key_e @ [ call_s "__ligo_internal__map_find_opt" ]
  | E_constant { cons_name = C_MAP_SIZE; arguments = [ m ] } ->
    let w, env, m = expression ~raise w env m in
    w, env, m @ [ call_s "__ligo_internal__set_size" ]
  | E_constant { cons_name = C_MAP_REMOVE; arguments = [ key; map ] } ->
    let w, env, key_e = expression ~raise w env key in
    let w, env, map_e = expression ~raise w env map in
    w, env, map_e @ key_e @ [ const 24l; call_s "__ligo_internal__set_remove" ]
  | E_constant { cons_name = C_MAP_MEM; arguments = [ key; set ] } ->
    let w, env, key_e = expression ~raise w env key in
    let w, env, set_e = expression ~raise w env set in
    w, env, set_e @ key_e @ [ call_s "__ligo_internal__set_mem" ]
  | E_constant { cons_name = C_MAP_UPDATE; arguments = [ key; value; map ] } ->
    let w, env, key_e = expression ~raise w env key in
    let w, env, value_e = expression ~raise w env value in
    let w, env, map_e = expression ~raise w env map in
    w, env, map_e @ key_e @ value_e @ [ const 24l; call_s "__ligo_internal__map_update" ]
  | E_constant { cons_name = C_MAP_GET_AND_UPDATE; arguments = [ key; value; map ] } ->
    let w, env, key_e = expression ~raise w env key in
    let w, env, value_e = expression ~raise w env value in
    let w, env, map_e = expression ~raise w env map in
    ( w
    , env
    , map_e @ key_e @ value_e @ [ const 24l; call_s "__ligo_internal__map_get_update" ] )
  | E_constant { cons_name = C_MAP_ITER; arguments = [ func; map ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_MAP_MAP; arguments = [ func; map ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_MAP_FOLD; arguments = [ func; map ] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_MAP_FIND; arguments = [ key; map ] } ->
    raise.error (not_supported e)
  (* Big Maps *)
  | E_constant { cons_name = C_BIG_MAP_EMPTY; arguments = [] } ->
    raise.error (not_supported e)
  | E_constant { cons_name = C_BIG_MAP_LITERAL; arguments = [ e1 ] } ->
    raise.error (not_supported e)
  | E_constant
      { cons_name = C_BIG_MAP_GET_AND_UPDATE; arguments = [ key; value; big_map ] } ->
    raise.error (not_supported e)
  (* Blockchain *)
  (* | E_constant {cons_name = C_CALL; arguments = [param; mutez; contract] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_CONTRACT; arguments = [address] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_CONTRACT_OPT; arguments = [address] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_CONTRACT_WITH_ERROR; arguments = [address; msg] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_CONTRACT_ENTRYPOINT; arguments = [entrypoint; address] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_CONTRACT_ENTRYPOINT_OPT; arguments = [entrypoint; address] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_ADDRESS; arguments = [address] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_SELF; arguments = [entrypoint] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_SELF_ADDRESS; arguments = [] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_IMPLICIT_ACCOUNT; arguments = [keyhash] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_SET_DELEGATE; arguments = [keyhash] } -> raise.error (not_supported e) *)
  | E_constant
      { cons_name = C_CREATE_CONTRACT
      ; arguments = [ operation_list_init; keyhash; mutez; init ]
      } -> raise.error (not_supported e)
  (* | E_constant {cons_name = C_OPEN_CHEST; arguments = [chest_key; chest; n] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_VIEW; arguments = [view_name; t; address] } -> raise.error (not_supported e) *)
  | E_constant { cons_name = C_SOME; arguments = [ arg ] } ->
    let w, env, arg = expression ~raise w env arg in
    let some = unique_name "c_some" in
    let env = Env.add_local env (some, T.NumType I32Type) in
    ( w
    , env
    , [ const 9l
      ; call_s "malloc"
      ; local_tee_s some
      ; const 6l (* option tag *)
      ; store8
      ; local_get_s some
      ; const 1l
      ; i32_add
      ; const 1l
      ; store
      ; local_get_s some
      ; const 5l
      ; i32_add
      ]
      @ arg
      @ [ store; local_get_s some ] )
  | E_constant { cons_name = C_NONE; arguments = [] } ->
    let none = unique_name "c_none" in
    let env = Env.add_local env (none, T.NumType I32Type) in
    ( w
    , env
    , [ const 5l
      ; call_s "malloc"
      ; local_tee_s none
      ; const 6l
      ; store8
      ; local_get_s none
      ; const 1l
      ; i32_add
      ; const 0l
      ; store
      ; local_get_s none
      ] )
  | E_constant { cons_name = C_LEFT; arguments = [ a ] } ->
    let c_left = unique_name "c_left" in
    let env = Env.add_local env (c_left, T.NumType I32Type) in
    let w, env, a = expression ~raise w env a in
    ( w
    , env
    , [ const 5l
      ; call_s "malloc"
      ; local_tee_s c_left
      ; const 7l
      ; store8
      ; local_get_s c_left
      ; const 1l
      ; i32_add
      ]
      @ a
      @ [ store; local_get_s c_left ] )
  | E_constant { cons_name = C_RIGHT; arguments = [ a ] } ->
    let c_right = unique_name "c_right" in
    let env = Env.add_local env (c_right, T.NumType I32Type) in
    let w, env, a = expression ~raise w env a in
    ( w
    , env
    , [ const 5l
      ; call_s "malloc"
      ; local_tee_s c_right
      ; const 8l
      ; store8
      ; local_get_s c_right
      ; const 1l
      ; i32_add
      ]
      @ a
      @ [ store; local_get_s c_right ] )
  | E_application _ ->
    let rec aux
        w
        env
        (result : A.instr list)
        (result_vars : string list)
        (expr : I.expression)
      =
      match expr.I.content with
      | E_application (func, e) ->
        let name = unique_name "e_application_" in
        let env = Env.add_local env (name, T.NumType I32Type) in
        let w, env, e = expression ~raise w env e in
        aux w env (result @ e @ [ local_set_s name ]) (name :: result_vars) func
      | E_variable v ->
        let name = var_to_string v in
        (match func_symbol_type w name with
        | Some (FuncSymbol fs, TypeSymbol { tdetails = FuncType (input, output); _ }) ->
          let no_of_args = List.length input in
          if no_of_args = List.length result_vars
          then
            ( w
            , env
            , result @ List.map ~f:(fun s -> local_get_s s) result_vars @ [ call_s name ]
            )
          else (
            let func_name, w = Partial_call.create_helper w at ~name ~no_of_args in
            let env, func_alloc_name, e =
              Partial_call.create_memory_block
                env
                at
                ~f:[ func_symbol func_name ]
                ~no_of_args
                ~current_args:result_vars
            in
            ( w
            , env
            , e
              @ result
              @ (let a, i =
                   List.fold_left
                     ~f:(fun (all, index) f ->
                       ( all
                         @ [ local_get_s func_alloc_name
                           ; const Int32.(index * 4l)
                           ; i32_add
                           ; local_get_s f
                           ; store
                           ]
                       , Int32.(index + 1l) ))
                     ~init:([], 3l)
                     result_vars
                 in
                 a)
              @ [ local_get_s func_alloc_name ] ))
        | _ ->
          let indirect_name = unique_name "call_indirect" in
          let total_args_length = unique_name "total_args_length" in
          let current_args_length = unique_name "current_args_length" in
          let counter = unique_name "counter" in
          let dest = unique_name "dest" in
          let env =
            add_locals
              env
              [ total_args_length, NumType I32Type
              ; current_args_length, NumType I32Type
              ; counter, NumType I32Type
              ; dest, NumType I32Type
              ]
          in
          ( { w with
              types =
                w.types
                @ [ type_
                      ~name:indirect_name
                      ~typedef:(FuncType ([ NumType I32Type ], [ NumType I32Type ]))
                  ]
                (* symbols = w.symbols @ 
              [
                symbol ~name:indirect_name ~details:Function;
              ]  *)
            }
          , env
          , [ local_get_s name
            ; const 4l
            ; i32_add
            ; load
            ; local_set_s total_args_length
            ; local_get_s name
            ; const 8l
            ; i32_add
            ; load
            ; local_set_s current_args_length
            ]
            @ Partial_call.memory_copy env at ~func_name:name ~total_args_length ~dest
            @ result
            @ (let a, i =
                 List.fold_left
                   ~f:(fun (all, index) f ->
                     ( all
                       @ [ local_get_s dest
                         ; local_get_s current_args_length
                         ; const index
                         ; i32_add
                         ; const 4l
                         ; i32_mul
                         ; i32_add
                         ; local_get_s f
                         ; store
                         ]
                     , Int32.(index + 1l) ))
                   ~init:([], 3l)
                   result_vars
               in
               a)
            @ [ (* update the current args *)
                local_get_s dest
              ; const 8l
              ; i32_add
              ; local_get_s current_args_length
              ; const (Int32.of_int_exn (List.length result_vars))
              ; i32_add
              ; store
              ; (* check to see if all the arguments are entered, if not: still a partial *)
                local_get_s current_args_length
              ; const (Int32.of_int_exn (List.length result_vars))
              ; i32_add
              ; local_get_s total_args_length
              ; i32_eq
              ; if_
                  (ValBlockType (Some (NumType I32Type)))
                  [ local_get_s dest
                  ; const 12l
                  ; i32_add
                  ; local_get_s dest
                  ; load
                  ; call_indirect_s indirect_name
                  ]
                  [ local_get_s dest ]
              ] ))
      | E_raw_wasm (local_symbols, code, args) ->
        let args =
          List.fold_left ~f:(fun e i -> e @ [ local_get_s i ]) ~init:[] result_vars
        in
        w, add_locals env local_symbols, result @ args @ code
      | E_constant _ -> raise.error (not_supported e)
      | _ -> raise.error (not_supported e)
    in
    let w, env, args = aux w env [] [] e in
    w, env, args
  | E_variable name ->
    let name = var_to_string name in
    (match List.find ~f:(fun (n, _) -> String.equal n name) env.locals with
    | Some _ -> w, env, [ local_get_s name ]
    | None ->
      (match func_symbol_type w name with
      | Some (FuncSymbol fs, TypeSymbol { tdetails = FuncType (input, output); _ }) ->
        let no_of_args = List.length input in
        let func_name, w = Partial_call.create_helper w at ~name ~no_of_args in
        let env, func_alloc_name, e =
          Partial_call.create_memory_block
            env
            at
            ~f:[ func_symbol func_name ]
            ~no_of_args
            ~current_args:[]
        in
        w, env, e @ [ local_get_s func_alloc_name ]
      | _ -> w, env, [ data_symbol name ]))
  | E_iterator
      ( C_MAP
      , ((item_name, item_type), body)
      , ({ type_expression = { type_content = T_list _; _ }; _ } as col) ) ->
    let env = add_locals env [ var_to_string item_name, T.NumType I32Type ] in
    let w, env, col = expression ~raise w env col in
    let w, env, iter_body = expression ~raise w env body in
    let iter_body_name = unique_name "iter_body" in
    let w, required_args = add_function w iter_body_name (fun _ -> iter_body) in
    w, env, col @ [ func_symbol iter_body_name; call_s "__ligo_internal__list_map" ]
  | E_iterator
      ( C_MAP
      , ((item_name, item_type), body)
      , ({ type_expression = { type_content = T_map _; _ }; _ } as col) ) ->
    let env = add_locals env [ var_to_string item_name, T.NumType I32Type ] in
    let w, env, col = expression ~raise w env col in
    let w, env, iter_body = expression ~raise w env body in
    let iter_body_name = unique_name "iter_body" in
    let w, required_args = add_function w iter_body_name (fun _ -> iter_body) in
    w, env, col @ [ func_symbol iter_body_name; call_s "__ligo_internal__map_map" ]
  | E_iterator
      ( C_ITER
      , ((item_name, item_type), body)
      , ({ type_expression = { type_content = T_list _; _ }; _ } as col) ) ->
    let env = add_locals env [ var_to_string item_name, T.NumType I32Type ] in
    let w, env, col = expression ~raise w env col in
    let w, env, iter_body = expression ~raise w env body in
    let iter_body_name = unique_name "iter_body" in
    let w, required_args = add_function w iter_body_name (fun _ -> iter_body) in
    w, env, col @ [ func_symbol iter_body_name; call_s "__ligo_internal__list_iter" ]
  | E_iterator
      ( C_ITER
      , ((item_name, item_type), body)
      , ({ type_expression = { type_content = T_set _; _ }; _ } as col) ) ->
    let env = add_locals env [ var_to_string item_name, T.NumType I32Type ] in
    let w, env, iter_body = expression ~raise w env body in
    let w, env, col = expression ~raise w env col in
    let iter_body_name = unique_name "iter_body" in
    let w, required_args = add_function w iter_body_name (fun _ -> iter_body) in
    w, env, col @ [ func_symbol iter_body_name; call_s "__ligo_internal__set_iter" ]
  | E_iterator
      ( C_ITER
      , ((item_name, item_type), body)
      , ({ type_expression = { type_content = T_map _; _ }; _ } as col) ) ->
    let env = add_locals env [ var_to_string item_name, T.NumType I32Type ] in
    let w, env, iter_body = expression ~raise w env body in
    let w, env, col = expression ~raise w env col in
    let iter_body_name = unique_name "iter_body" in
    let w, required_args = add_function w iter_body_name (fun _ -> iter_body) in
    w, env, col @ [ func_symbol iter_body_name; call_s "__ligo_internal__map_iter" ]
  | E_iterator (_, ((item_name, item_type), body), col) -> raise.error (not_supported e)
  | E_fold
      ( ((name, tv), body)
      , ({ type_expression = { type_content = T_set _; _ }; _ } as col)
      , initial ) ->
    let fold_body_name = unique_name "fold_body" in
    let env = add_locals env [ var_to_string name, T.NumType I32Type ] in
    let w, env, col = expression ~raise w env col in
    let w, env, init = expression ~raise w env initial in
    let w, env, iter_body = expression ~raise w env body in
    let w, required_args = add_function w fold_body_name (fun _ -> iter_body) in
    ( w
    , env
    , col @ init @ [ func_symbol fold_body_name; call_s "__ligo_internal__set_fold" ] )
  | E_fold
      ( ((name, tv), body)
      , ({ type_expression = { type_content = T_map _; _ }; _ } as col)
      , initial ) ->
    let fold_body_name = unique_name "fold_body" in
    let env = add_locals env [ var_to_string name, T.NumType I32Type ] in
    let w, env, col = expression ~raise w env col in
    let w, env, init = expression ~raise w env initial in
    let w, env, iter_body = expression ~raise w env body in
    let w, required_args = add_function w fold_body_name (fun _ -> iter_body) in
    ( w
    , env
    , col @ init @ [ func_symbol fold_body_name; call_s "__ligo_internal__map_fold" ] )
  | E_fold_right
      ( ((name, tv), body)
      , (({ type_expression = { type_content = T_set _; _ }; _ } as col), elem_tv)
      , initial ) ->
    let fold_body_name = unique_name "fold_body" in
    let env = add_locals env [ var_to_string name, T.NumType I32Type ] in
    let w, env, col = expression ~raise w env col in
    let w, env, init = expression ~raise w env initial in
    let w, env, iter_body = expression ~raise w env body in
    let w, required_args = add_function w fold_body_name (fun _ -> iter_body) in
    ( w
    , env
    , col
      @ init
      @ [ func_symbol fold_body_name; call_s "__ligo_internal__set_fold_right" ] )
  | E_fold
      ( ((name, tv), body)
      , ({ type_expression = { type_content = T_list _; _ }; _ } as col)
      , initial ) ->
    let fold_body_name = unique_name "fold_body" in
    let env = add_locals env [ var_to_string name, T.NumType I32Type ] in
    let w, env, col = expression ~raise w env col in
    let w, env, init = expression ~raise w env initial in
    let w, env, iter_body = expression ~raise w env body in
    let w, required_args = add_function w fold_body_name (fun _ -> iter_body) in
    ( w
    , env
    , col @ init @ [ func_symbol fold_body_name; call_s "__ligo_internal__list_fold" ] )
  | E_fold
      (((name, tv), body), ({ type_expression = { type_content = _; _ }; _ } as col), init)
    -> raise.error (not_supported e)
  | E_fold_right
      ( ((name, tv), body)
      , (({ type_expression = { type_content = T_list _; _ }; _ } as col), elem_tv)
      , initial ) ->
    let fold_body_name = unique_name "fold_body" in
    let env = add_locals env [ var_to_string name, T.NumType I32Type ] in
    let w, env, col = expression ~raise w env col in
    let w, env, init = expression ~raise w env initial in
    let w, env, iter_body = expression ~raise w env body in
    let w, required_args = add_function w fold_body_name (fun _ -> iter_body) in
    ( w
    , env
    , col
      @ init
      @ [ func_symbol fold_body_name; call_s "__ligo_internal__list_fold_right" ] )
  | E_fold_right _ -> raise.error (not_supported e)
  | E_if_bool (test, t, f) ->
    let w, env, test = expression ~raise w env test in
    let w, env, t = expression ~raise w env t in
    let w, env, f = expression ~raise w env f in
    w, env, test @ [ if_ (ValBlockType (Some (T.NumType I32Type))) t f ]
  | E_if_none (test, none_e, ((some_arg, some_arg_type), some_e)) ->
    let some_arg = var_to_string some_arg in
    let testing = unique_name "testing" in
    let env = Env.add_local env (some_arg, T.NumType I32Type) in
    let env = Env.add_local env (testing, T.NumType I32Type) in
    let w, env, test = expression ~raise w env test in
    let w, env, none_e = expression ~raise w env none_e in
    let w, env, some_e = expression ~raise w env some_e in
    let return_type = Some (T.NumType I32Type) in
    ( w
    , env
    , test
      @ [ const 1l
        ; i32_add
        ; local_set_s testing
        ; S.
            { it =
                A.Block
                  ( ValBlockType return_type
                  , [ { it =
                          A.Block
                            ( ValBlockType None
                            , [ local_get_s testing; load; br_if 0l ] @ none_e @ [ br 1l ]
                            )
                      ; at
                      }
                    ]
                    @ [ local_get_s testing
                      ; const 4l
                      ; i32_add
                      ; load
                      ; local_set_s some_arg
                      ]
                    @ some_e )
            ; at
            }
        ] )
  | E_if_cons (matchee, nil, (((hd, _), (tl, _)), cons)) ->
    let hd = var_to_string hd in
    let tl = var_to_string tl in
    let data = unique_name "data" in
    let env =
      add_locals env [ hd, NumType I32Type; tl, NumType I32Type; data, NumType I32Type ]
    in
    let w, env, matchee_e = expression ~raise w env matchee in
    let w, env, nil_e = expression ~raise w env nil in
    let w, env, cons_e = expression ~raise w env cons in
    ( w
    , env
    , matchee_e
      @ [ local_tee_s data
        ; const 1l
        ; i32_add
        ; load
        ; local_set_s hd
        ; local_get_s data
        ; const 5l
        ; i32_add
        ; load
        ; local_set_s tl
        ; S.
            { it =
                A.Block
                  ( ValBlockType (Some (T.NumType I32Type))
                  , [ { it =
                          A.Block
                            ( ValBlockType None
                            , [ local_get_s data; const 10l; i32_eq; br_if 0l ]
                              @ cons_e
                              @ [ br 1l ] )
                      ; at
                      }
                    ]
                    @ nil_e )
            ; at
            }
        ] )
  | E_if_left (matchee, ((name_l, _), left), ((name_r, _), right)) ->
    (* Variants *)
    let matchee_name = unique_name "matchee" in
    let name_l = var_to_string name_l in
    let name_r = var_to_string name_r in
    let env =
      add_locals
        env
        [ name_l, NumType I32Type
        ; name_r, NumType I32Type
        ; matchee_name, NumType I32Type
        ]
    in
    let w, env, matchee_e = expression ~raise w env matchee in
    let w, env, left_e = expression ~raise w env left in
    let w, env, right_e = expression ~raise w env right in
    ( w
    , env
    , matchee_e
      @ [ local_set_s matchee_name
        ; S.
            { it =
                A.Block
                  ( ValBlockType (Some (T.NumType I32Type))
                  , [ { it =
                          A.Block
                            ( ValBlockType None
                            , [ local_get_s matchee_name
                              ; load
                              ; br_if 0l
                              ; local_get_s matchee_name
                              ; const 1l
                              ; i32_add
                              ; load
                              ; local_set_s name_l
                              ]
                              @ left_e
                              @ [ br 1l ] )
                      ; at
                      }
                    ]
                    @ [ local_get_s matchee_name
                      ; const 1l
                      ; i32_add
                      ; load
                      ; local_set_s name_r
                      ]
                    @ right_e )
            ; at
            }
        ] )
  | E_let_in ({ content = E_closure _; _ }, _inline, ((name, _type), e2)) ->
    raise.error (not_supported e)
  | E_let_in (e1, _inline, ((name, typex), e2)) ->
    let name = var_to_string name in
    let w, env, e1 = expression ~raise w env e1 in
    let env = add_local env (name, T.NumType I32Type) in
    let w, env, e2 = expression ~raise w env e2 in
    w, env, e1 @ [ local_set_s name ] @ e2
  | E_tuple el ->
    let tuple_name = var_to_string (Value_var.fresh ~name:"let_tuple" ()) in
    let t =
      [ const (Int32.of_int_exn (List.length el))
      ; const 4l
      ; i32_mul
      ; call_s "malloc"
      ; local_set_s tuple_name
      ]
    in
    let env = add_local env (tuple_name, T.NumType I32Type) in
    let w, env, e =
      List.foldi
        ~f:(fun i (w, env, all) e ->
          let w, env, e = expression ~raise w env e in
          ( w
          , env
          , all
            @ [ local_get_s tuple_name
              ; const Int32.(4l * Int32.of_int_exn i)
              ; i32_add
              ; load
              ]
            @ e
            @ [ store ] ))
        ~init:(w, env, [])
        el
    in
    w, env, t @ e @ [ local_get_s tuple_name ]
  | E_let_tuple (tuple, (values, rhs)) ->
    let w, env, tuple = expression ~raise w env tuple in
    let tuple_name = var_to_string (Value_var.fresh ~name:"let_tuple" ()) in
    let t = tuple @ [ local_set_s tuple_name ] in
    let env = add_local env (tuple_name, T.NumType I32Type) in
    let env, e =
      List.foldi
        ~f:(fun i (env, all) (name, _) ->
          let name = var_to_string name in
          ( add_local env (name, T.NumType I32Type)
          , all
            @ [ local_get_s tuple_name
              ; const Int32.(1l (* tag *) + (4l * Int32.of_int_exn Int.(i)))
              ; i32_add
              ; load
              ; local_set_s name
              ] ))
        ~init:(env, [])
        values
    in
    let w, env, e2 = expression ~raise w env rhs in
    w, env, t @ e @ e2
  | E_proj (expr, i, count) ->
    let w, env, expr = expression ~raise w env expr in
    w, env, expr @ [ const Int32.(4l * Int32.of_int_exn i); load ]
  | E_update (_, _, _, _) -> raise.error (not_supported e)
  | E_raw_wasm (local_symbols, code, args) ->
    let w, env, args =
      List.fold_left
        ~f:(fun (w, env, e) i ->
          let w, env, expr = expression ~raise w env i in
          w, env, e @ expr)
        ~init:(w, env, [])
        args
    in
    w, add_locals env local_symbols, args @ code
  | E_create_contract (_, _, _, _) -> raise.error (not_supported e)
  (* Are these actually used? *)
  | E_constant { cons_name = C_LOOP_CONTINUE; arguments } -> raise.error (not_supported e)
  | E_constant { cons_name = C_LOOP_STOP; arguments } -> raise.error (not_supported e)
  | E_constant { cons_name = C_MAP; arguments } -> raise.error (not_supported e)
  | E_constant { cons_name = C_BIG_MAP; arguments } -> raise.error (not_supported e)
  | E_constant { cons_name = C_MAP_GET; arguments } -> raise.error (not_supported e)
  | E_constant { cons_name = C_MAP_GET_FORCE; arguments } -> raise.error (not_supported e)
  | E_global_constant (_, _) -> raise.error (not_supported e) (* is this actually used? *)
  (* Inline Michelson is not supported in wasm contracts *)
  | E_raw_michelson _ -> raise.error (michelson_insertion e.location)
  (* catch all *)
  | E_constant { cons_name; _ } -> raise.error (not_supported e)
  (* Mutability stuff *)
  | E_let_mut_in (_, _) -> raise.error (not_supported e)
  | E_deref _ -> raise.error (not_supported e)
  | E_assign (_, _) -> raise.error (not_supported e)
  | E_for_each (_, _, _) -> raise.error (not_supported e)
  | E_for (_, _, _, _) -> raise.error (not_supported e)
  | E_while (_, _) -> raise.error (not_supported e)


let func I.{ binder; body } =
  let rec aux arguments body =
    match body.I.content with
    | E_closure { binder; body } -> aux (binder :: arguments) body
    | _ -> List.rev arguments, body
  in
  aux [ binder ] body


let rec toplevel_bindings ~raise
    : I.expression -> string -> W.Ast.module_' -> W.Ast.module_'
  =
 fun e entrypoint w ->
  let at = location_to_region e.location in
  let drop = drop at in
  let const = const at in
  let call_s = call_s at in
  let call_indirect_s = call_indirect_s at in
  let local_set_s = local_set_s at in
  let local_get_s = local_get_s at in
  let local_tee_s = local_tee_s at in
  let load = load at in
  let store = store at in
  let i32_add = i32_add at in
  let i32_mul = i32_mul at in
  let data_symbol = data_symbol at in
  let func_symbol = func_symbol at in
  let elem = elem at in
  let compare_eq = compare_eq at in
  let if_ = if_ at in
  let br = br at in
  let br_if = br_if at in
  let loop = loop at in
  let nop = nop at in
  let convert_to_memory = convert_to_memory at in
  let add_local = Env.add_local in
  let add_locals = Env.add_locals in
  match e.content with
  | E_let_in
      ({ content = E_constant { cons_name = C_TRUE; _ }; _ }, _inline, ((name, type_), e2))
    ->
    let name = var_to_string name in
    let open Int32 in
    let data = A.[ data ~offset:!global_offset ~init:{ name; detail = [ Int32 1l ] } ] in
    let symbols = A.[ symbol_data ~name ~index:0l ~size:4l ~offset:!global_offset ] in
    global_offset := !global_offset + 4l;
    let w = { w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    toplevel_bindings ~raise e2 entrypoint w
  | E_let_in
      ( { content = E_constant { cons_name = C_FALSE; _ }; _ }
      , _inline
      , ((name, type_), e2) ) ->
    let name = var_to_string name in
    let open Int32 in
    let data = A.[ data ~offset:!global_offset ~init:{ name; detail = [ Int32 0l ] } ] in
    let symbols = A.[ symbol_data ~name ~index:0l ~size:4l ~offset:!global_offset ] in
    global_offset := !global_offset + 4l;
    let w = { w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    toplevel_bindings ~raise e2 entrypoint w
  | E_let_in ({ content = E_closure c; _ }, _inline, ((name, type_), e2)) ->
    let name = var_to_string name in
    let arguments, body = func c in
    let env =
      List.fold_left
        ~f:(fun env i -> add_local env (var_to_string i, T.NumType I32Type))
        ~init:(Env.make_env ())
        arguments
    in
    let w, env, body = expression ~raise w env body in
    let env = Validation.check w env body in
    let type_arg = List.map ~f:(fun _ -> T.NumType I32Type) arguments in
    assert (Poly.equal env.operand_stack (Some (T.NumType I32Type, Next None)));
    let return_type = [ T.NumType I32Type ] in
    let w =
      { w with
        symbols = w.symbols @ [ { it = { name; details = Function }; at } ]
      ; types =
          w.types
          @ [ { it =
                  TypeSymbol
                    { tname = name ^ "_type"
                    ; tdetails = FuncType (type_arg, return_type)
                    }
              ; at
              }
            ]
      ; funcs =
          w.funcs
          @ [ { it =
                  FuncSymbol { name; ftype = name ^ "_type"; locals = env.locals; body }
              ; at
              }
            ]
      }
    in
    toplevel_bindings ~raise e2 entrypoint w
  | E_let_in ({ content = E_literal (Literal_int z); _ }, _inline, ((name, _type), e2)) ->
    (* we convert these to in memory values *)
    let name = var_to_string name in
    let data, symbols = convert_to_memory 2 name z in
    toplevel_bindings
      ~raise
      e2
      entrypoint
      { w with datas = w.datas @ data; symbols = w.symbols @ symbols }
  | E_variable entrypoint ->
    let actual_name = var_to_string entrypoint in
    let name = "entrypoint" in
    let w =
      { w with
        symbols = w.symbols @ [ symbol ~name ~details:Function ]
      ; types =
          w.types
          @ [ type_
                ~name:(name ^ "_type")
                ~typedef:
                  (FuncType
                     ([ T.NumType I32Type; T.NumType I32Type ], [ T.NumType I32Type ]))
            ]
      ; funcs =
          w.funcs
          @ [ { it =
                  FuncSymbol
                    { name
                    ; ftype = name ^ "_type"
                    ; locals =
                        [ "parameter", T.NumType I32Type
                        ; "storage", T.NumType I32Type
                        ; "entrypoint_tuple", T.NumType I32Type
                        ; "result", T.NumType I32Type
                        ]
                    ; body =
                        [ const 8l
                        ; call_s "malloc"
                        ; local_set_s "entrypoint_tuple"
                        ; local_get_s "entrypoint_tuple"
                        ; local_get_s "parameter"
                        ; store
                        ; local_get_s "entrypoint_tuple"
                        ; const 4l
                        ; i32_add
                        ; local_get_s "storage"
                        ; store
                        ; local_get_s "entrypoint_tuple"
                        ; call_s actual_name
                        ; local_tee_s "result"
                        ; const 1l
                        ; i32_add
                        ; load
                        ; load
                        ; drop
                        ; local_get_s "result"
                        ; drop
                        ; const 0l
                        ]
                    }
              ; at
              }
            ]
      }
    in
    w
  | _ -> raise.error (not_supported e)


let compile ~raise : I.expression -> string -> string -> W.Ast.module_ =
 fun e filename entrypoint ->
  let w = Default_module.mod_ in
  let at = location_to_region e.location in
  global_offset := Default_module.offset;
  let w = toplevel_bindings ~raise e entrypoint w.it in
  let elems_i =
    List.mapi
      ~f:(fun i _ -> elem at i)
      (List.filter
         ~f:(fun f ->
           match f.it.idesc.it with
           | FuncImport _ | FuncImport_symbol _ -> true
           | _ -> false)
         w.imports)
  in
  let elems = List.mapi ~f:(fun i _ -> elem at (List.length elems_i + i)) w.funcs in
  let elems = List.mapi ~f:(fun i _ -> elem at i) w.types in
  (* let elems = List.mapi ~f:(fun i _ -> elem at (List.length elems_i + i)) w.types in *)
  let w =
    { w with
      elems
    ; imports =
        [ import
            ~item:"__indirect_function_table"
            ~desc:(TableImport (TableType ({ min = 1l; max = None }, FuncRefType)))
        ]
        @ w.imports
    }
  in
  S.{ it = w; at }
