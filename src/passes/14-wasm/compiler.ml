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
module ValueVar = Stage_common.Types.ValueVar
module Location = Simple_utils.Location

open Helpers

(**
 * Converts LIGO's location.t to WasmObjectFile's Source.region.
 *)
let location_to_region (l : Location.t) : S.region =
  match l with
  | File l ->
    {
      left = {file = l#file; line = l#start#line; column = l#start#column `Byte};
      right = {file = l#file; line = l#stop#line; column = l#stop#column `Byte};
    }
  | Virtual _ -> S.no_region

(** 
 * Convert a variable to a string which we can use for symbols 
 *)
let var_to_string name =
  let name, hash = ValueVar.internal_get_name_and_counter name in
  name ^ "#" ^ string_of_int hash

(* The data offset. This indicates where a block of data should be placed in the linear memory. *)
let global_offset = ref 0l

(**
 * Convert a Zarith number (used by LIGO and Tezos) to a num_bigint number (a Rust crate for big numbers).
 *)
let convert_to_memory :
    string -> S.region -> Z.t -> A.data_segment list * A.sym_info list =
 fun name at z ->
  let z = Z.to_int32 z in
  let open Int32 in
  let data = A.[data ~offset:!global_offset ~init:{name; detail = [Int32 z]}] in
  let symbols =
    A.[symbol_data ~name ~index:0l ~size:4l ~offset:!global_offset]
  in
  global_offset := !global_offset + 4l;
  (data, symbols)

type locals = (string * T.value_type) list

let name s =
  try W.Utf8.decode s with W.Utf8.Utf8 -> failwith "invalid UTF-8 encoding"

let func_symbol_type w symbol = 
  match List.find ~f:(fun f -> match f.it with FuncSymbol {name; _} -> String.equal name symbol | _ -> false ) w.A.funcs with 
  | Some {it = FuncSymbol {ftype; _} as fs; _} -> (
    let t = List.find ~f:(fun f -> match f.it with TypeSymbol {tname;_ } -> String.equal tname ftype | _ -> false ) w.A.types in
    match t with 
      Some t -> Some (fs, t.it)
    | None -> None
  )
  | _ -> None

let rec expression ~raise :
    A.module_' -> locals -> I.expression -> A.module_' * locals * A.instr list =
 fun w l e ->
  let open A in 
  let open T in 
  let at = location_to_region e.location in
  let host_call :
      fn:string ->
      response_size:int32 ->
      instructions:I.expression list ->
      A.module_' * locals * A.instr list =
   fun ~fn ~response_size ~instructions ->
    let new_value = var_to_string (ValueVar.fresh ~name:fn ()) in
    let w, l, e =
      List.fold_left
        ~f:(fun all (a : I.expression) ->
          let w, l, e = all in
          let w, l, e2 = expression ~raise w l a in
          (w, l, e @ e2))
        ~init:(w, l, []) instructions
    in
    ( w,
      l @ [(new_value, T.NumType I32Type)],
      S.
         [
           const response_size at;
           call_s "malloc" at;
           local_tee_s new_value at;
         ]
      @
      e 
      @ S.[ 
          call_s fn at; 
          store at; 
          local_get_s new_value at
        ] )
  in
  match e.content with
  | E_literal Literal_unit -> (w, l, [{it = Nop; at}])
  | E_literal (Literal_int z) ->
    let unique_name = ValueVar.fresh ~name:"Literal_int" () in
    let name = var_to_string unique_name in
    let data, symbols = convert_to_memory name at z in
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols} in
    (w, l, [data_symbol name at])
  | E_literal (Literal_nat z) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_nat" () in
    let name = var_to_string unique_name in
    let data, symbols = convert_to_memory name at z in
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols} in
    (w, l, [data_symbol name at])
  | E_literal (Literal_timestamp z) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_timestamp" () in
    let name = var_to_string unique_name in
    let data, symbols = convert_to_memory name at z in
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols} in
    (w, l, [data_symbol name at])
  | E_literal (Literal_mutez z) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_mutez" () in
    let name = var_to_string unique_name in
    let data, symbols = convert_to_memory name at z in
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols} in
    (w, l, [data_symbol name at])
  | E_literal (Literal_string (Standard s)) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_string" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [String s]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(String.length s)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (String.length s));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_string (Verbatim s)) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_string_verbatim" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [String s]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(String.length s)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (String.length s));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_bytes b) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_bytes" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [Bytes b]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(Bytes.length b)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (Bytes.length b));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_address s) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_address" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [String s]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(String.length s)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (String.length s));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_signature s) ->
    let unique_name = ValueVar.fresh ~name:"Literal_signature" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [String s]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(String.length s)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (String.length s));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_key s) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_key" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [String s]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(String.length s)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (String.length s));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_key_hash s) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_key_hash" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [String s]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(String.length s)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (String.length s));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_chain_id s) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_chain_id" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [String s]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(String.length s)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (String.length s));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_operation b) ->
    let unique_name = ValueVar.fresh ~name:"Literal_operation" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [Bytes b]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(Bytes.length b)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (Bytes.length b));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_bls12_381_g1 b) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_bls12_381_g1" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [Bytes b]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(Bytes.length b)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (Bytes.length b));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_bls12_381_g2 b) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_bls12_381_g2" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [Bytes b]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(Bytes.length b)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (Bytes.length b));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_bls12_381_fr b) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_bls12_381_fr" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [Bytes b]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(Bytes.length b)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (Bytes.length b));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_chest b) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_chest" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [Bytes b]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(Bytes.length b)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (Bytes.length b));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])
  | E_literal (Literal_chest_key b) -> 
    let unique_name = ValueVar.fresh ~name:"Literal_chest_key" () in
    let name = var_to_string unique_name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [Bytes b]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(Bytes.length b)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (Bytes.length b));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, l, [data_symbol name at])

  | E_closure {binder; body} -> (w, l, [S.{it = A.Nop; at}])
  
  (* Loops *)
  | E_constant {cons_name = C_ITER; arguments = [func; iter]} -> failwith "Not implemented: C_ITER"
  | E_constant {cons_name = C_LOOP_LEFT; arguments = [func; init]} -> failwith "Not implemented: C_LOOP_LEFT"
  
  | E_constant {cons_name = C_FOLD; arguments = [func; list; init]} -> failwith "Not implemented: C_FOLD"
  | E_constant {cons_name = C_FOLD_LEFT; arguments = [func; init; col]} -> failwith "Not implemented: C_FOLD_LEFT"
  | E_constant {cons_name = C_FOLD_RIGHT; arguments = [func; col; init]} -> failwith "Not implemented: C_FOLD_RIGHT"

  (* MATH *)
  | E_constant {cons_name = C_NEG; arguments} ->
    host_call ~fn:"c_neg" ~response_size:4l ~instructions:arguments
  | E_constant {cons_name = C_ADD; arguments = [e1; e2]} ->
    host_call ~fn:"c_add_i32" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_SUB; arguments = [e1; e2]} ->
    host_call ~fn:"c_sub" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_MUL; arguments = [e1; e2]} ->
    host_call ~fn:"c_mul" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_DIV; arguments = [e1; e2]} ->
    host_call ~fn:"c_div" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_MOD; arguments = [e1; e2]} ->
    host_call ~fn:"c_mod" ~response_size:4l ~instructions:[e1; e2]

  (* LOGIC *)
  | E_constant {cons_name = C_NOT; arguments = [e1; e2]} ->
    host_call ~fn:"c_not" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_AND; arguments = [e1; e2]} ->
    host_call ~fn:"c_and" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_OR; arguments = [e1; e2]} ->
    host_call ~fn:"c_or" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_XOR; arguments = [e1; e2]} ->
    host_call ~fn:"c_xor" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_LSL; arguments = [e1; e2]} ->
    host_call ~fn:"c_lsl" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_LSR; arguments = [e1; e2]} ->
    host_call ~fn:"c_lsr" ~response_size:4l ~instructions:[e1; e2]

  (* COMPARATOR *)
  | E_constant {cons_name = C_EQ;  arguments = [e1; e2]} -> 
    host_call ~fn:"c_eq" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_NEQ; arguments = [e1; e2]} -> 
    host_call ~fn:"c_neq" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_LT;  arguments = [e1; e2]} -> 
    host_call ~fn:"c_lt" ~response_size:4l ~instructions:[e1; e2] 
  | E_constant {cons_name = C_GT;  arguments = [e1; e2]} ->
    host_call ~fn:"c_gt" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_LE;  arguments = [e1; e2]} -> 
    host_call ~fn:"c_le" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_GE;  arguments = [e1; e2]} -> 
    host_call ~fn:"c_ge" ~response_size:4l ~instructions:[e1; e2]

  (* Bytes/ String *)
  | E_constant {cons_name = C_CONCAT; arguments = [e1; e2] } -> 
    host_call ~fn:"c_concat" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_BYTES_UNPACK; arguments = [e1] } -> 
    host_call ~fn:"c_bytes_unpack" ~response_size:4l ~instructions:[e1]
  | E_constant {cons_name = C_CONS; arguments = [l1; l2]} ->
    let cons = var_to_string (ValueVar.fresh ~name:"C_CONS" ()) in
    let w, l, l1 = expression ~raise w l l1 in
    let w, l, l2 = expression ~raise w l l2 in
    ( w,
      l @ [(cons, T.NumType I32Type)],
      [
        const 8l at;
        call_s "malloc" at;
        (* check if not 0 *)
        local_tee_s cons at;
      ]
      @ l1
      @ [
        store at;
        local_get_s cons at;
        const 4l at;
        i32_add at;
        ]
      @ l2
      @ [
        store at;
        local_get_s cons at;
        ] )

  (* Pair *)
  | E_constant {cons_name = C_PAIR; arguments = [e1; e2]} ->
    let pair = var_to_string (ValueVar.fresh ~name:"C_PAIR" ()) in
    let w, l, e1 = expression ~raise w l e1 in
    let w, l, e2 = expression ~raise w l e2 in
    let e =
      S.[const 8l at; call_s "malloc" at; local_set_s pair at; local_get_s pair at]
      @ e1
      @ S.[store at; local_get_s pair at; const 4l at; i32_add at]
      @ e2
      @ S.[store at; local_get_s pair at]
    in
    (w, l @ [(pair, T.NumType I32Type)], e)
  | E_constant {cons_name = C_CAR; arguments = [cons]} ->
    let w, l, cons = expression ~raise w l cons in
    ( w,
      l,
      cons @ [load at]
    )
  | E_constant {cons_name = C_CDR; arguments = [cons]} ->
    let w, l, cons = expression ~raise w l cons in
    ( w,
      l,
      cons
      @ [
          const 4l at;
          i32_add at;
          load at
        ] )
  | E_constant {cons_name = C_TRUE; arguments = [] } -> w, l, [const 1l at]
  | E_constant {cons_name = C_FALSE; arguments = [] } -> w, l, [const 0l at]
  
  (* Set *)
  | E_constant {cons_name = C_SET_EMPTY; arguments = [] } -> w, l, [data_symbol "C_SET_EMPTY" at]
  | E_constant {cons_name = C_SET_LITERAL; arguments = [e1] } -> 
    host_call ~fn:"c_set_literal" ~response_size:4l ~instructions:[e1]
  | E_constant {cons_name = C_SET_ADD; arguments = [item; set] } -> 
    host_call ~fn:"c_set_add" ~response_size:4l ~instructions:[item; set]
  | E_constant {cons_name = C_SET_REMOVE; arguments = [item; set] } -> 
    host_call ~fn:"c_set_remove" ~response_size:4l ~instructions:[item; set]
  | E_constant {cons_name = C_SET_ITER; arguments = [func; set] } -> failwith "Not supported: C_SET_ITER"
  | E_constant {cons_name = C_SET_FOLD; arguments = [func; set; init] } -> failwith "Not supported: C_SET_FOLD"
  | E_constant {cons_name = C_SET_FOLD_DESC; arguments = [func; set; init] } -> failwith "Not supported: C_SET_FOLD_DESC"
  | E_constant {cons_name = C_SET_MEM; arguments = [item; set] } -> 
    host_call ~fn:"c_set_mem" ~response_size:4l ~instructions:[item; set]
  | E_constant {cons_name = C_SET_UPDATE; arguments = [item; boolean; set] } -> 
    host_call ~fn:"c_set_update" ~response_size:4l ~instructions:[item; boolean; set]
    
  
  (* List *)
  | E_constant {cons_name = C_LIST_EMPTY; arguments = []} -> (w, l, [data_symbol "C_LIST_EMPTY" at])
  | E_constant {cons_name = C_LIST_LITERAL; arguments = [e1] } -> failwith "Not supported: C_LIST_LITERAL"
  | E_constant {cons_name = C_LIST_ITER; arguments = [func; list]  } -> failwith "Not supported: C_LIST_ITER"
  | E_constant {cons_name = C_LIST_MAP; arguments = [func; list] } -> failwith "Not supported: C_LIST_MAP"
  | E_constant {cons_name = C_LIST_FOLD; arguments = [func; list; init] } -> failwith "Not supported: C_LIST_FOLD"
  | E_constant {cons_name = C_LIST_FOLD_LEFT; arguments = [func; init; list]  } -> failwith "Not supported: C_LIST_FOLD_LEFT"
  | E_constant {cons_name = C_LIST_FOLD_RIGHT; arguments = [func; list; init] } -> failwith "Not supported: C_LIST_FOLD_RIGHT"

  (* Maps *)
  | E_constant {cons_name = C_MAP_EMPTY; arguments = [] } -> failwith "Not supported: C_MAP_EMPTY"
  | E_constant {cons_name = C_MAP_LITERAL; arguments = [e1] } -> failwith "Not supported: C_MAP_LITERAL"
  
  
  | E_constant {cons_name = C_MAP_ADD; arguments = [key; value; map] } -> failwith "Not supported: C_MAP_ADD"
  | E_constant {cons_name = C_MAP_REMOVE; arguments = [key; map] } -> failwith "Not supported: C_MAP_REMOVE"
  | E_constant {cons_name = C_MAP_UPDATE; arguments = [key; value; map] } -> failwith "Not supported: C_MAP_UPDATE"
  | E_constant {cons_name = C_MAP_ITER; arguments = [func; map] } -> failwith "Not supported: C_MAP_ITER"
  | E_constant {cons_name = C_MAP_MAP; arguments = [func; map] } -> failwith "Not supported: C_MAP_MAP"
  | E_constant {cons_name = C_MAP_FOLD; arguments = [func; map] } -> failwith "Not supported: C_MAP_FOLD"
  | E_constant {cons_name = C_MAP_FIND; arguments = [key; map] } -> failwith "Not supported: C_MAP_FIND"
  | E_constant {cons_name = C_MAP_FIND_OPT; arguments = [key; map] } -> failwith "Not supported: C_MAP_FIND_OPT"
  | E_constant {cons_name = C_MAP_GET_AND_UPDATE; arguments = [key; value; map] } -> failwith "Not supported: C_MAP_GET_AND_UPDATE"

  (* Big Maps *)
  | E_constant {cons_name = C_BIG_MAP_EMPTY; arguments = [] } -> failwith "Not supported: C_BIG_MAP_EMPTY"
  | E_constant {cons_name = C_BIG_MAP_LITERAL; arguments = [e1] } -> failwith "Not supported: C_BIG_MAP_LITERAL"
  | E_constant {cons_name = C_BIG_MAP_GET_AND_UPDATE; arguments = [key; value; big_map] } -> failwith "Not supported: C_BIG_MAP_GET_AND_UPDATE"

  (* Blockchain *)
  | E_constant {cons_name = C_CALL; arguments = [param; mutez; contract] } -> failwith "Not supported C_CALL"
  | E_constant {cons_name = C_CONTRACT; arguments = [address] } -> failwith "Not supported C_CONTRACT"
  | E_constant {cons_name = C_CONTRACT_OPT; arguments = [address] } -> failwith "Not supported C_CONTRACT_OPT"
  | E_constant {cons_name = C_CONTRACT_WITH_ERROR; arguments = [address; msg] } -> failwith "Not supported C_CONTRACT_WITH_ERROR"
  | E_constant {cons_name = C_CONTRACT_ENTRYPOINT; arguments = [entrypoint; address] } -> failwith "Not supported C_CONTRACT_ENTRYPOINT"
  | E_constant {cons_name = C_CONTRACT_ENTRYPOINT_OPT; arguments = [entrypoint; address] } -> failwith "Not supported C_CONTRACT_ENTRYPOINT_OPS"
  | E_constant {cons_name = C_ADDRESS; arguments = [address] } -> failwith "Not supported C_ADDRESS"
  | E_constant {cons_name = C_SELF; arguments = [entrypoint] } -> failwith "Not supported C_SELF"
  | E_constant {cons_name = C_SELF_ADDRESS; arguments = [] } -> failwith "Not supported C_SELF_ADDRESS"
  | E_constant {cons_name = C_IMPLICIT_ACCOUNT; arguments = [keyhash] } -> failwith "Not supported C_IMPLICIT_ACCOUNT"
  | E_constant {cons_name = C_SET_DELEGATE; arguments = [keyhash] } -> failwith "Not supported C_SET_DELEGATE"
  | E_constant {cons_name = C_CREATE_CONTRACT; arguments = [operation_list_init;keyhash;mutez;init] } -> failwith "Not supported C_CREATE_CONTRACT"
  | E_constant {cons_name = C_OPEN_CHEST; arguments = [chest_key; chest; n] } -> failwith "Not supported C_OPEN_CHESt"
  | E_constant {cons_name = C_VIEW; arguments = [view_name; t; address] } -> failwith "Not supported C_VIEW"

  | E_application _ ->
    let rec aux w l (result: A.instr list) (expr: I.expression) =
      match expr.I.content with
      | E_application (func, e) -> 
        let w, l, e = expression ~raise w l e in 
        aux w l (result @ e) func
      | E_variable v ->        
        let name = var_to_string v in
        (match (func_symbol_type w name) with 
        | Some (FuncSymbol fs, TypeSymbol {tdetails = FuncType (input, output); _}) -> 
          let no_of_args = List.length input in
          if no_of_args = List.length result then 
            (w, l, result @ [call_s name at])
          else (
            let unique_name = ValueVar.fresh ~name:"e_variable_partial" () in
            let func_alloc_name = var_to_string unique_name in
            w, l @ [(func_alloc_name, NumType I32Type)], 
            [
              const Int32.(12l + of_int_exn (List.length result)) at;
              call_s "malloc" at;
              
              local_tee_s func_alloc_name at;
              func_symbol name at;
              store at;

              local_get_s func_alloc_name at;
              const 4l at;
              i32_add at;
              const (Int32.of_int_exn no_of_args) at; 
              store at;

              local_get_s func_alloc_name at;
              const 8l at;
              i32_add at;
              const (Int32.of_int_exn (List.length result)) at;
              store at
            ]
            @
            (let a, i = List.fold_left ~f:(fun (all, index) f -> 
              (all @
              [
                local_get_s func_alloc_name at;
                const Int32.(index * 4l) at;
                i32_add at;
                f;
                store at
              ], Int32.(index + 1l)
              )
            ) ~init:([], 3l) result
            in a
            )
            @
            [
              local_get_s func_alloc_name at
            ]
          )
        | _ -> 
          let lt = expr.type_expression in
          let unique_name = ValueVar.fresh ~name:"Call_indirect" () in
          let indirect_name = var_to_string unique_name in
          
          let total_args_length = ValueVar.fresh ~name:"total_args_length" () in
          let total_args_length = var_to_string total_args_length in
          
          let current_args_length = ValueVar.fresh ~name:"current_args_length" () in
          let current_args_length = var_to_string current_args_length in

          let counter = ValueVar.fresh ~name:"counter" () in
          let counter = var_to_string counter in

          let unique_name = ValueVar.fresh ~name:"Call_indirect" () in
          let return_type = [NumType I32Type] in (* TODO properly get this *)
          ({w with 
            types = w.types
              @ 
              [type_ ~name:indirect_name ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]))]
          }, l @ [(total_args_length, NumType I32Type); (current_args_length, NumType I32Type); (counter, NumType I32Type)], [
            local_get_s name at; 
            const 4l at;
            i32_add at;
            load at; 
            local_set_s total_args_length at;

            local_get_s name at; 
            const 8l at;
            i32_add at;
            load at; 
            local_set_s current_args_length at;

          ]
          @
          (let a, i = List.fold_left ~f:(fun (all, index) f -> 
            (all @
            [
              local_get_s name at;
              
              local_get_s current_args_length at;
              const index at;
              i32_add at;
              const 4l at;
              i32_mul at;
              
              i32_add at;
              f;
              store at
            ], Int32.(index + 1l)
            )
          ) ~init:([], 3l) result
          in a
          )
          @
          [
            local_get_s current_args_length at;
            const (Int32.of_int_exn (List.length result)) at;
            i32_add at;
            local_get_s total_args_length at;
            compare_eq at;
            if_ 
              (ValBlockType (Some (NumType I32Type))) 
              [
                local_get_s name at;
                const 12l at;
                i32_add at;
                local_get_s name at;
                load at;
                call_indirect_s indirect_name at
              ]
              [
                local_get_s name at
              ]
              at
          ]) 
        )
      | E_raw_wasm (local_symbols, code) -> 
        (w, l @ local_symbols, result @ code)
      | _ -> failwith "E_application (..) not supported"
    in
    let w, l, args = aux w l [] e in
    (w, l, args)
  | E_variable name -> (
    let name = var_to_string name in
    match List.find ~f:(fun (n, _) -> String.equal n name) l with
    | Some _ -> (w, l, [local_get_s name at])
    | None -> (
      match func_symbol_type w name with 
      | Some (FuncSymbol fs, TypeSymbol {tdetails = FuncType (input, output); _}) -> 
        let unique_name = ValueVar.fresh ~name:"e_variable_func" () in
        let func_alloc_name = var_to_string unique_name in
        let no_of_args = List.length input in


        (* create the function here *)
        let func_name = "i32_" ^ (string_of_int no_of_args) in

        (* does it exist ? *)
        let exists = List.find ~f:(fun a -> match a.it with | FuncSymbol n -> (String.equal n.name func_name) | _ -> false) w.funcs  in
        let w = match exists with 
         Some _ -> 
          w
        | None -> (
          let type_name = "i32_" ^ (string_of_int no_of_args) ^ "_type" in
          let t = TypeSymbol {
            tname = type_name;
            tdetails = FuncType ([NumType I32Type], [NumType I32Type])
          } in
          let t = S.{it = t; at} in
          let f: A.func' = FuncSymbol {
            name   = func_name;
            ftype  = type_name;
            locals = [("foo", NumType I32Type)];
            body   = 
            
            (
              List.fold_left ~f:(fun all i -> 
                all @
                [
                local_get_s "foo" at;
                const Int32.(of_int_exn i * 4l) at;
                i32_add at;
                load at]
              )
              ~init: []
              (List.init no_of_args ~f:(fun i -> i)) 
            )
            @
            [
              call_s name at;
            ]
          } in
          let f = S.{ it = f; at } in
          let s = {
            name = func_name;
            details = Function;
          }
          in
          let s = S.{ it = s; at } in
          {w with funcs = f :: w.funcs; types = t :: w.types; symbols = s :: w.symbols}
        )
        in
      w, l @ [(func_alloc_name, NumType I32Type)], [
          const Int32.(12l + Int32.of_int_exn no_of_args) at;
          call_s "malloc" at;
          
          local_tee_s func_alloc_name at;
          func_symbol func_name at;
          store at;

          local_get_s func_alloc_name at;
          const 4l at;
          i32_add at;
          const (Int32.of_int_exn no_of_args) at; 
          store at;

          local_get_s func_alloc_name at;
          const 8l at;
          i32_add at;
          const 0l at;
          store at;

          local_get_s func_alloc_name at
      ] 
      | _ -> w,l, [data_symbol name at]

    )
        
    )
  | E_iterator (kind, ((item_name, item_type), body), collection) -> 
    (* todo: how do we get information on the collection? *)

    (* [
      (* what kind of collection ?! *) (* C_ITER, C_MAP, C_LOOP_LEFT *)
      Loop (
        [], ...
      )
    ] *)
    (* - collection
    - next next next *)

    failwith "E_iterator not supported"
  | E_fold (((name, tv), body), col, init) -> 
    
    failwith "E_fold not supported"
  | E_fold_right _ -> failwith "E_fold_right not supported"
  | E_if_bool (test, t, f) -> 
    let w, l, test = expression ~raise w l test in
    let w, l, t = expression ~raise w l t in
    let w, l, f = expression ~raise w l f in
    let return_type = Some (NumType I32Type) in (* TODO properly get this *)
    w, l, [
      S.{it = Block (ValBlockType return_type, 
        [
          {it = Block (ValBlockType None,
            test 
            @
            [
              {it = BrIf {it = 0l; at}; at };
            ]
            @ 
            f
            @ 
            [
              {it = Br {it = 1l; at}; at };
            ]
          );
          at
          }
        ]
        @ 
        t
      );
      at
      }
    ]
  | E_if_none (test, none_e, (_, some_e)) -> 
    (* TODO: check handling of locals *)
    let w, l, test = expression ~raise w l test in
    let w, l, none_e = expression ~raise w l none_e in
    let w, l, some_e = expression ~raise w l some_e in
    let return_type = Some (NumType I32Type) in (* TODO properly get this *)
    w, l, [
      S.{it = Block (ValBlockType return_type, 
        [
          {it = Block (ValBlockType None,
            test 
            @
            [
              {it = BrIf {it = 0l; at}; at };
            ]
            @ 
            none_e
            @ 
            [
              {it = Br {it = 1l; at}; at };
            ]
          );
          at
          }
        ]
        @ 
        some_e
      );
      at
      }
    ]
  | E_if_cons (matchee, nil, (((hd, _), (tl, _)), cons)) -> 
    let hd = var_to_string hd in
    let tl = var_to_string tl in
    let l = l @ [(hd, NumType I32Type); (tl, NumType I32Type)] in
    let w, l, matchee_e = expression ~raise w l matchee in
    let w, l, nil_e = expression ~raise w l nil in
    let w, l, cons_e = expression ~raise w l cons in
    let return_type = Some (NumType I32Type) in (* TODO properly get this *)
    w, l, [
      S.{it = Block (ValBlockType return_type, 
        [
          {it = Block (ValBlockType None,
          matchee_e
            @
            [
              data_symbol "C_LIST_EMPTY" at;
              {it = Compare (I32 I32Op.Ne); at };
              {it = BrIf {it = 0l; at}; at };
            ]
            @ 
            nil_e
            @ 
            [
              {it = Br {it = 1l; at}; at };
            ]
          );
          at
          }
        ]
        @ 
        cons_e
      );
      at
      }]
  | E_if_left (matchee, ((name_l, _), left), ((name_r, _), right)) -> 
    (* Variants *)
    let name_l = var_to_string name_l in
    let name_r = var_to_string name_r in
    let l = l @ [(name_l, NumType I32Type); (name_r, NumType I32Type)] in
    let w, l, matchee_e = expression ~raise w l matchee in
    let w, l, left_e = expression ~raise w l left in
    let w, l, right_e = expression ~raise w l right in
    let return_type = Some (NumType I32Type) in (* TODO properly get this *)
    w, l, [
      S.{it = Block (ValBlockType return_type, 
        [
          {it = Block (ValBlockType None,
          matchee_e
            @
            [
              {it = BrIf {it = 0l; at}; at };
            ]
            @ 
            left_e
            @ 
            [
              {it = Br {it = 1l; at}; at };
            ]
          );
          at
          }
        ]
        @ 
        right_e
      );
      at
      }]
  | E_let_in
      ( {content = E_closure {binder; body}},
        _inline,
        ((name, _type), e2) ) ->
    failwith "should not happen..."
  | E_let_in (e1, _inline, ((name, typex), e2)) ->
    let name = var_to_string name in
    let w, l, e1 = expression ~raise w l e1 in
    let l = l @ [(name, T.NumType I32Type)] in
    let w, l, e2 = expression ~raise w l e2 in
    (w, l, e1 @ [local_set_s name at] @ e2)
  | E_tuple _ -> failwith "E_tuple not supported"
  | E_let_tuple (tuple, (values, rhs)) ->
    let w, l, tuple = expression ~raise w l tuple in
    let tuple_name = var_to_string (ValueVar.fresh ~name:"let_tuple" ()) in
    let t = tuple @ [local_set_s tuple_name at] in
    let l = l @ [(tuple_name, T.NumType I32Type)] in
    let l, e =
      List.foldi
        ~f:(fun i (l, all) (name, _) ->
          let name = var_to_string name in
          ( l @ [(name, T.NumType I32Type)],
            all
            @ [
              local_get_s tuple_name at;
              const Int32.(4l * Int32.of_int_exn i) at;
              i32_add at;
              load at;
              local_set_s name at;
            ]
          )
        )
        ~init:(l, []) values
    in
    let w, l, e2 = expression ~raise w l rhs in
    (w, l, t @ e @ e2)
  | E_proj (expr, i, count) -> 
    let w, l, expr = expression ~raise w l expr in
    w, l, 
    expr 
    @
    [
      const Int32.(4l * Int32.of_int_exn i) at;
      load at
    ]
  | E_update (_,_,_,_) -> failwith "E_update not supported"
  | E_raw_wasm (local_symbols, code) -> (w, l @ local_symbols, code)
  | E_create_contract (_,_,_,_) -> failwith "E_create_contract not supported"

  (* Are these actually used? *)
  | E_constant {cons_name = C_LOOP_CONTINUE; arguments} -> failwith "Not implemented: C_LOOP_CONTINUE"
  | E_constant {cons_name = C_LOOP_STOP; arguments} -> failwith "Not implemented: C_LOOP_STOP"
  | E_constant {cons_name = C_MAP; arguments } -> failwith "Not supported: C_MAP"
  | E_constant {cons_name = C_BIG_MAP; arguments } -> failwith "Not supported: C_BIG_MAP"
  | E_constant {cons_name = C_LEFT; arguments = [a; b] } -> failwith "Not supported: C_LEFT"
  | E_constant {cons_name = C_RIGHT; arguments = [a; b]} -> failwith "Not supported: C_RIGHT"
  | E_constant {cons_name = C_MAP_GET; arguments } -> failwith "Not supported: C_MAP_GET"
  | E_constant {cons_name = C_MAP_GET_FORCE; arguments } -> failwith "Not supported: C_MAP_GET_FORCE"  
  | E_global_constant (_,_) -> failwith "E_global_constant not supported" (* is this actually used? *)

  (* Inline Michelson is not supported in wasm contracts *)
  | E_raw_michelson _ -> failwith "E_raw_michelson not supported"

  (* catch all *)
  | E_constant {cons_name; _} -> failwith ("E_constant with these arguments is not supported.")
  

let func I.{binder; body} =
  let rec aux arguments body =
    match body.I.content with
    | E_closure {binder; body} -> aux (binder :: arguments) body
    | _ -> (List.rev arguments, body)
  in
  aux [binder] body

let rec toplevel_bindings ~raise :
    I.expression -> W.Ast.module_' -> W.Ast.module_' =
 fun e w ->
  let at = location_to_region e.location in
  match e.content with
  | E_let_in ({content = E_closure c; _}, _inline, ((name, type_), e2))
    ->
    let name = var_to_string name in
    let arguments, body = func c in
    let locals =
      List.map ~f:(fun a -> (var_to_string a, T.NumType I32Type)) arguments
    in
    let w, locals, body = expression ~raise w locals body in
    let type_arg = List.map ~f:(fun _ -> T.NumType I32Type) arguments in
    let return_type =
      match type_.type_content with
      | I.T_function (_, {type_content = I.T_base TB_unit; _}) -> []
      | _ -> [T.NumType I32Type]
    in
    let w =
      {
        w with
        symbols = w.symbols @ [{it = {name; details = Function}; at}];
        types =
          w.types
          @ [
              {
                it =
                  TypeSymbol {
                    tname = name ^ "_type";
                    tdetails = FuncType (type_arg, return_type);
                  };
                at;
              };
            ];
        funcs =
          w.funcs @ [{it = FuncSymbol {name; ftype = name ^ "_type"; locals; body}; at}];
      }
    in
    toplevel_bindings ~raise e2 w
  | E_let_in
      ( {content = E_literal (Literal_int z); _},
        _inline,
        ((name, _type), e2) ) ->
    (* we convert these to in memory values *)
    let name = var_to_string name in
    let data, symbols = convert_to_memory name at z in
    toplevel_bindings ~raise e2
      {w with datas = w.datas @ data; symbols = w.symbols @ symbols}
  | E_variable entrypoint ->
    let actual_name = var_to_string entrypoint in
    let name = "entrypoint" in
    let w =
      {
        w with
        symbols = w.symbols @ [
          symbol ~name ~details:Function   
        ];
        types =
          w.types
          @ [
            type_ ~name:(name ^ "_type") ~typedef:(FuncType ([T.NumType I32Type; T.NumType I32Type], [T.NumType I32Type]));
            ];
        funcs =
          w.funcs
          @ [
              {
                it =
                  FuncSymbol {
                    name;
                    ftype = name ^ "_type";
                    locals = [("parameter", T.NumType I32Type); ("storage", T.NumType I32Type); ("entrypoint_tuple", T.NumType I32Type)];
                    body =
                      [
                        const 8l at;
                        call_s "malloc" at;
                        local_set_s "entrypoint_tuple" at;
                        local_get_s "entrypoint_tuple" at;
                        local_get_s "parameter" at;
                        store at;

                        local_get_s "entrypoint_tuple" at;
                        const 4l at;
                        i32_add at;
                        local_get_s "storage" at;
                        store at;

                        local_get_s "entrypoint_tuple" at;
                        call_s actual_name at;
                      ];
                  };
                at;
              };
            ];
      }
    in
    w
  | _ -> failwith "Instruction not supported at the toplevel."

let compile ~raise : I.expression -> string -> string -> W.Ast.module_ =
 fun e filename entrypoint ->
  let w = Default_env.env in
  let at = location_to_region e.location in
  global_offset := Default_env.offset;
  let w = toplevel_bindings ~raise e w.it in
  let elems_i = List.mapi ~f:(fun i _ -> elem i at) (List.filter ~f:(fun f -> match f.it.idesc.it with FuncImport _ | FuncImport_symbol _ -> true | _ -> false) w.imports) in
  let elems = List.mapi ~f:(fun i _ -> elem (List.length elems_i + i) at) w.funcs in
  let w = {w with 
    elems = elems_i @ elems;
    symbols = w.symbols @ [
      {it = {
        name = "ttt";
        details = Table
      };
      at
      }

    ]
  } in
  S.{it = w; at}
