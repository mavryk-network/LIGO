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
    env, func_alloc_name, [
      const Int32.(12l + Int32.of_int_exn no_of_args);
      call_s "malloc";
      local_set_s func_alloc_name;

      local_get_s func_alloc_name;
    ]
    @
    f
    @  
    [
      store;

      local_get_s func_alloc_name;
      const 4l;
      i32_add;
      const (Int32.of_int_exn no_of_args); 
      store;

      local_get_s func_alloc_name;
      const 8l;
      i32_add;
      const (Int32.of_int_exn (List.length current_args));
      store;

    ]

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
    let exists = List.find ~f:(fun a -> match a.it with | FuncSymbol n -> (String.equal n.name func_name) | FuncNoSymbol _ -> false) w.funcs  in
    match exists with 
      Some _ -> 
        func_name, w
    | None -> (
      let type_name = "i32_" ^ (string_of_int no_of_args) ^ "_type" in
      let t = A.TypeSymbol {
        tname = type_name;
        tdetails = FuncType ([NumType I32Type], [NumType I32Type])
      } in
      let t = S.{it = t; at} in
      let f: A.func' = FuncSymbol {
        name   = func_name;
        ftype  = type_name;
        locals = [("arg", NumType I32Type)];
        body   = 
        
        (
          List.fold_left ~f:(fun all i -> 
            all @
            [
            local_get_s "arg";
            const Int32.(of_int_exn i * 4l);
            i32_add;
            load]
          )
          ~init: []
          (List.init no_of_args ~f:(fun i -> i)) 
        )
        @
        [
          call_s name;
        ]
      } in
      let f = S.{ it = f; at } in
      let s = A.{
        name = func_name;
        details = Function;
      }
      in
      let s = S.{ it = s; at } in
      func_name, {w with funcs = f :: w.funcs; types = t :: w.types; symbols = s :: w.symbols}
    )

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

    [
      const 12l; 
      local_get_s total_args_length;
      const 4l;
      i32_mul;
      i32_add;
      call_s "malloc";
      local_tee_s dest;

      local_get_s func_name;

      const 12l; 
      local_get_s total_args_length;
      const 4l;
      i32_mul;
      i32_add;

      { it = MemoryCopy; at };
    ]

end


(* The data offset. This indicates where a block of data should be placed in the linear memory. *)
let global_offset = ref 0l

(**
 * Convert a Zarith number (used by LIGO and Tezos) to a wasm memory representation.
 *)
let convert_to_memory :
  S.region -> string -> Z.t -> A.data_segment list * A.sym_info list =
 fun at name z ->
  let z = Z.to_int32 z in
  let open Int32 in
  let data = A.[data ~offset:!global_offset ~init:{name; detail = [Int32 z]}] in
  let symbols =
    A.[symbol_data ~name ~index:0l ~size:4l ~offset:!global_offset]
  in
  global_offset := !global_offset + 4l;
  (data, symbols)

type locals = (string * T.value_type) list

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
    A.module_' -> Env.t -> I.expression -> A.module_' * Env.t * A.instr list =
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
  let i32_ne  = i32_ne at in
  let i32_eq  = i32_eq at in
  let data_symbol = data_symbol at in
  let func_symbol = func_symbol at in
  let elem = elem at in
  let compare_eq = compare_eq at in
  let if_ = if_ at in 
  let br_if = br_if at in
  let br = br at in
  let loop = loop at in 
  let nop = nop at in
  let convert_to_memory = convert_to_memory at in
  let add_local = Env.add_local in
  let add_locals = Env.add_locals in
  let host_call :
      fn:string ->
      response_size:int32 ->
      instructions:I.expression list ->
      A.module_' * Env.t * A.instr list =
   fun ~fn ~response_size ~instructions ->
    let new_value = var_to_string (Value_var.fresh ~name:fn ()) in
    let w, env, e =
      List.fold_left
        ~f:(fun all (a : I.expression) ->
          let w, env, e = all in
          let w, env, e2 = expression ~raise w env a in
          (w, env, e @ e2))
        ~init:(w, env, []) instructions
    in  
    w,
    add_local env (new_value, T.NumType I32Type),
    S.
        [
          const response_size;
          call_s "malloc";
          local_tee_s new_value;
        ]
    @
    e 
    @ S.[ 
        call_s fn; 
        store; 
        local_get_s new_value
      ] 
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
  let int_like name z = 
    let name = unique_name name in
    let data, symbols = convert_to_memory name z in
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols} in
    (w, env, [data_symbol name])
  in
  let string_like name s = 
    let name = unique_name name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [String s]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(String.length s)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (String.length s));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, env, [data_symbol name])
  in
  let bytes_like name b = 
    let name = unique_name name in
    let data = [data ~offset:!global_offset ~init:{name; detail = [Bytes b]}] in
    let symbols =
      [symbol_data ~name ~index:0l ~size:(Int32.of_int_exn(Bytes.length b)) ~offset:!global_offset]
    in
    global_offset := Int32.(!global_offset + Int32.of_int_exn (Bytes.length b));
    let w = {w with datas = w.datas @ data; symbols = w.symbols @ symbols } in
    (w, env, [data_symbol name])
  in
  match e.content with
  | E_literal Literal_unit                  -> (w, env, [const 0l])
  | E_literal (Literal_int z)               -> int_like "Literal_int" z
  | E_literal (Literal_nat z)               -> int_like "Literal_nat" z
  | E_literal (Literal_timestamp z)         -> int_like "Literal_timestamp" z
  | E_literal (Literal_mutez z)             -> int_like "Literal_mutez" z
  | E_literal (Literal_string (Standard s)) -> string_like "Literal_string" s
  | E_literal (Literal_string (Verbatim s)) -> string_like "Literal_string" s
  | E_literal (Literal_bytes b)             -> bytes_like "Literal_bytes" b
  | E_literal (Literal_address s)           -> string_like "Literal_address" s
  | E_literal (Literal_signature s)         -> string_like "Literal_signature" s
  | E_literal (Literal_key s)               -> string_like "Literal_key" s
  | E_literal (Literal_key_hash s)          -> string_like "Literal_key_hash" s
  | E_literal (Literal_chain_id s)          -> string_like "Literal_chain_id" s
  | E_literal (Literal_operation b)         -> bytes_like "Literal_operation" b
  | E_literal (Literal_bls12_381_g1 b)      -> bytes_like "Literal_bls12_381_g1" b
  | E_literal (Literal_bls12_381_g2 b)      -> bytes_like "Literal_bls12_381_g2" b
  | E_literal (Literal_bls12_381_fr b)      -> bytes_like "Literal_bls12_381_fr" b
  | E_literal (Literal_chest b)             -> bytes_like "Literal_chest" b
  | E_literal (Literal_chest_key b)         -> bytes_like "Literal_chest_key" b
    
  | E_closure {binder; body} -> raise.error (not_supported e)
  
  (* Loops *)
  | E_constant {cons_name = C_ITER; arguments = [func; iter]} -> raise.error (not_supported e)
  | E_constant {cons_name = C_LOOP_LEFT; arguments = [func; init]} -> raise.error (not_supported e)
  | E_constant {cons_name = C_FOLD; arguments = [func; list; init]} -> raise.error (not_supported e)
  | E_constant {cons_name = C_FOLD_LEFT; arguments = [func; init; col]} -> raise.error (not_supported e)
  | E_constant {cons_name = C_FOLD_RIGHT; arguments = [func; col; init]} -> raise.error (not_supported e)

  (* MATH *)
  | E_constant {cons_name = C_NEG; arguments = [e1]} ->
    uni_op Datatype.Int.neg w env e1
  | E_constant {cons_name = C_ADD; arguments = [e1; e2]} ->
    bin_op Datatype.Int.add w env e1 e2          
  | E_constant {cons_name = C_SUB; arguments = [e1; e2]} ->
    bin_op Datatype.Int.sub w env e1 e2
  | E_constant {cons_name = C_MUL; arguments = [e1; e2]} ->
    bin_op Datatype.Int.mul w env e1 e2
  | E_constant {cons_name = C_DIV; arguments = [e1; e2]} ->
    bin_op Datatype.Int.div w env e1 e2
  | E_constant {cons_name = C_MOD; arguments = [e1; e2]} ->
    raise.error (not_supported e)

  (* LOGIC *)
  | E_constant {cons_name = C_NOT; arguments = [e1; e2]} ->
    (* TODO: CHECK: is this the same?? *)
    uni_op Datatype.Int.neg w env e1
  | E_constant {cons_name = C_AND; arguments = [e1; e2]} ->
    bin_op Datatype.Int.and_ w env e1 e2
  | E_constant {cons_name = C_OR; arguments = [e1; e2]} ->
    bin_op Datatype.Int.or_ w env e1 e2
  | E_constant {cons_name = C_XOR; arguments = [e1; e2]} ->
    bin_op Datatype.Int.xor w env e1 e2
  | E_constant {cons_name = C_LSL; arguments = [e1; e2]} ->
    bin_op Datatype.Int.lsl_ w env e1 e2
  | E_constant {cons_name = C_LSR; arguments = [e1; e2]} ->
    bin_op Datatype.Int.lsr_ w env e1 e2

  (* COMPARATOR *)
  | E_constant {cons_name = C_EQ;  arguments = [e1; e2]} -> 
    bin_op Datatype.Int.eq w env e1 e2
  | E_constant {cons_name = C_NEQ; arguments = [e1; e2]} -> 
    bin_op Datatype.Int.ne w env e1 e2
  | E_constant {cons_name = C_LT;  arguments = [e1; e2]} -> 
    bin_op Datatype.Int.lt w env e1 e2
  | E_constant {cons_name = C_GT;  arguments = [e1; e2]} ->
    bin_op Datatype.Int.gt w env e1 e2
  | E_constant {cons_name = C_LE;  arguments = [e1; e2]} -> 
    bin_op Datatype.Int.le w env e1 e2
  | E_constant {cons_name = C_GE;  arguments = [e1; e2]} -> 
    bin_op Datatype.Int.ge w env e1 e2

  (* Bytes/ String *)
  | E_constant {cons_name = C_CONCAT; arguments = [e1; e2] } -> 
    host_call ~fn:"c_concat" ~response_size:4l ~instructions:[e1; e2]
  | E_constant {cons_name = C_CONS; arguments = [l1; l2]} ->
    let cons = var_to_string (Value_var.fresh ~name:"C_CONS" ()) in
    let w, env, l1 = expression ~raise w env l1 in
    let w, env, l2 = expression ~raise w env l2 in
    ( w,
      add_local env (cons, T.NumType I32Type),
      [
        const 8l;
        call_s "malloc";

        (* check if not 0 *)
        local_tee_s cons;
      ]
      @ 
      l1
      @ 
      [
        store;
        local_get_s cons;
        const 4l;
        i32_add;
      ]
      @ 
      l2
      @ 
      [
        store;
        local_get_s cons;
      ] 
    )

  (* Pair *)
  | E_constant {cons_name = C_PAIR; arguments = [e1; e2]} ->
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    Datatype.Pair.create w env e1 e2
  | E_constant {cons_name = C_CAR; arguments = [cons]} ->
    (* TODO: move to Datatype.ml *)
    let w, env, cons = expression ~raise w env cons in
    ( w,
      env,
      cons @ [load]
    )
  | E_constant {cons_name = C_CDR; arguments = [cons]} ->
    (* TODO: move to Datatype.ml *)
    let w, env, cons = expression ~raise w env cons in
    ( w,
      env,
      cons
      @ [
          const 4l;
          i32_add;
          load
        ] )
  | E_constant {cons_name = C_TRUE; arguments = [] }  -> w, env, [const 1l]
  | E_constant {cons_name = C_FALSE; arguments = [] } -> w, env, [const 0l]
  
  (* Set *)
  | E_constant {cons_name = C_SET_EMPTY; arguments = [] } -> w, env, [data_symbol "C_SET_EMPTY"]
  | E_constant {cons_name = C_SET_LITERAL; arguments = [e1] } -> 
    host_call ~fn:"c_set_literal" ~response_size:4l ~instructions:[e1]
  | E_constant {cons_name = C_SET_ADD; arguments = [item; set] } -> 
    host_call ~fn:"c_set_add" ~response_size:4l ~instructions:[item; set]
  | E_constant {cons_name = C_SET_REMOVE; arguments = [item; set] } -> 
    host_call ~fn:"c_set_remove" ~response_size:4l ~instructions:[item; set]
  | E_constant {cons_name = C_SET_ITER; arguments = [func; set] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_SET_FOLD; arguments = [func; set; init] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_SET_FOLD_DESC; arguments = [func; set; init] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_SET_MEM; arguments = [item; set] } -> 
    host_call ~fn:"c_set_mem" ~response_size:4l ~instructions:[item; set]
  | E_constant {cons_name = C_SET_UPDATE; arguments = [item; boolean; set] } -> 
    host_call ~fn:"c_set_update" ~response_size:4l ~instructions:[item; boolean; set]
    
  
  (* List *)
  | E_constant {cons_name = C_LIST_EMPTY; arguments = []} -> (w, env, [data_symbol "C_LIST_EMPTY"])
  | E_constant {cons_name = C_LIST_LITERAL; arguments = [e1] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_LIST_ITER; arguments = [func; list]  } -> raise.error (not_supported e)
  | E_constant {cons_name = C_LIST_MAP; arguments = [func; list] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_LIST_FOLD; arguments = [func; list; init] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_LIST_FOLD_LEFT; arguments = [func; init; list]  } -> raise.error (not_supported e)
  | E_constant {cons_name = C_LIST_FOLD_RIGHT; arguments = [func; list; init] } -> raise.error (not_supported e)

  (* Maps *)
  | E_constant {cons_name = C_MAP_EMPTY; arguments = [] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_LITERAL; arguments = [e1] } -> raise.error (not_supported e)
  
  
  | E_constant {cons_name = C_MAP_ADD; arguments = [key; value; map] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_REMOVE; arguments = [key; map] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_UPDATE; arguments = [key; value; map] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_ITER; arguments = [func; map] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_MAP; arguments = [func; map] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_FOLD; arguments = [func; map] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_FIND; arguments = [key; map] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_FIND_OPT; arguments = [key; map] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_GET_AND_UPDATE; arguments = [key; value; map] } -> raise.error (not_supported e)

  (* Big Maps *)
  | E_constant {cons_name = C_BIG_MAP_EMPTY; arguments = [] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_BIG_MAP_LITERAL; arguments = [e1] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_BIG_MAP_GET_AND_UPDATE; arguments = [key; value; big_map] } -> raise.error (not_supported e)

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
  | E_constant {cons_name = C_CREATE_CONTRACT; arguments = [operation_list_init;keyhash;mutez;init] } -> raise.error (not_supported e)
  (* | E_constant {cons_name = C_OPEN_CHEST; arguments = [chest_key; chest; n] } -> raise.error (not_supported e) *)
  (* | E_constant {cons_name = C_VIEW; arguments = [view_name; t; address] } -> raise.error (not_supported e) *)
  | E_constant {cons_name = C_SOME; arguments = [arg]} ->
    let w, l, arg = expression ~raise w env arg in
    let some = unique_name "c_some" in  
    let env = Env.add_local env (some, T.NumType I32Type) in
    w, env, [ 
      const 8l; 
      call_s "malloc";
      local_tee_s some;
      const 1l;
      store;
      local_get_s some;
      const 4l;
      i32_add
    ]
    @ 
    arg
    @
    [
      store;
      local_get_s some
    ] 
  | E_constant {cons_name = C_NONE; arguments = []} ->  
    w, env, [
      const 0l;
    ]
  | E_constant {cons_name = C_LEFT; arguments = [a]} -> 
    let c_left = unique_name "c_left" in
    let env = Env.add_local env (c_left, T.NumType I32Type) in
    let w, env, a = expression ~raise w env a in
    w, env, 
   [
      const 8l;
      call_s "malloc";
      local_tee_s c_left;
      const 0l;
      store;
      local_get_s c_left;
      const 4l;
      i32_add;
    ]
    @
    a
    @
    [
      store;
      local_get_s c_left
    ]
  | E_constant {cons_name = C_RIGHT; arguments = [a]} -> 
    let c_right = unique_name "c_right" in
    let env = Env.add_local env (c_right, T.NumType I32Type) in
    let w, env, a = expression ~raise w env a in
    w, env, 
   [
      const 8l;
      call_s "malloc";
      local_tee_s c_right;
      const 1l;
      store;
      local_get_s c_right;
      const 4l;
      i32_add;
    ]
    @
    a
    @
    [
      store;
      local_get_s c_right
    ]

  | E_application _ ->
    let rec aux w env (result: A.instr list) (result_vars: string list) (expr: I.expression) =
      match expr.I.content with
      | E_application (func, e) -> 
        let name = unique_name "e_application_" in
        let env = Env.add_local env (name, T.NumType I32Type) in
        let w, env, e = expression ~raise w env e in 
        aux w env (result @ e @ [local_set_s name]) (name :: result_vars) func
      | E_variable v ->        
        let name = var_to_string v in
        (match (func_symbol_type w name) with 
        | Some (FuncSymbol fs, TypeSymbol {tdetails = FuncType (input, output); _}) -> 
          let no_of_args = List.length input in
          if no_of_args = List.length result_vars then (
            (w, env, result @ (List.map ~f:(fun s -> local_get_s s) result_vars) @ [call_s name])
          )
          else (
            let func_name, w = Partial_call.create_helper w at ~name ~no_of_args in 
            let env, func_alloc_name, e = Partial_call.create_memory_block env at ~f:[func_symbol func_name] ~no_of_args ~current_args:result_vars in
            w, env,
            e
            @
            result
            @
            (let a, i = List.fold_left ~f:(fun (all, index) f -> 
              (all @
              [
                local_get_s func_alloc_name;
                const Int32.(index * 4l);
                i32_add;
                local_get_s f;
                store
              ], Int32.(index + 1l)
              )
            ) ~init:([], 3l) result_vars
            in a
            )
            @
            [
              local_get_s func_alloc_name
            ]
          )
        | _ -> 
          let indirect_name = unique_name "call_indirect" in
          let total_args_length = unique_name "total_args_length"in
          let current_args_length = unique_name "current_args_length" in
          let counter = unique_name "counter" in
          let dest = unique_name "dest" in

          let env = add_locals env [
            (total_args_length, NumType I32Type); 
            (current_args_length, NumType I32Type); 
            (counter, NumType I32Type);
            (dest, NumType I32Type)
            ]
          in

          ({w with 
            types = w.types @ 
              [type_ ~name:indirect_name ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]))]
          }, 
          env, 
          [
            
            local_get_s name; 
            const 4l;
            i32_add;
            load; 
            local_set_s total_args_length;

            local_get_s name; 
            const 8l;
            i32_add;
            load; 
            local_set_s current_args_length;
          ]
          @ 
          Partial_call.memory_copy env at ~func_name:name ~total_args_length ~dest
          @
          result
          @
          (let a, i = List.fold_left ~f:(fun (all, index) f -> 
            (all @
            [
              local_get_s dest;
              
              local_get_s current_args_length;
              const index;
              i32_add;
              const 4l;
              i32_mul;
              i32_add;
              local_get_s f;
              store
            ], Int32.(index + 1l)
            )
          ) ~init:([], 3l) result_vars
          in a
          )
          @
          [
            (* update the current args *)
            local_get_s dest; 
            const 8l;
            i32_add;
            local_get_s current_args_length;
            const (Int32.of_int_exn (List.length result_vars));
            i32_add;
            store; 

            (* check to see if all the arguments are entered, if not: still a partial *)
            local_get_s current_args_length;
            const (Int32.of_int_exn (List.length result_vars));
            i32_add;
            local_get_s total_args_length;
            i32_eq;
            if_ 
              (ValBlockType (Some (NumType I32Type))) 
              [
                local_get_s dest;
                const 12l;
                i32_add;

                local_get_s dest;
                load;
                call_indirect_s indirect_name;
              ]
              [
                local_get_s dest
              ]
             
          ])
        )
      | E_raw_wasm (local_symbols, code, args) -> 
        let w, env, args = List.fold_left ~f:(fun (w, env, e) i -> 
          let w, env, expr = expression ~raise w env i in
          w, env, e @ expr
        ) ~init:(w, env, []) args 
        in
        (w, add_locals env local_symbols, result @ args @ code)
      | _ ->
        raise.error (not_supported e)
    in
    let w, env, args = aux w env [] [] e in
    (w, env, args)
  | E_variable name -> (
    let name = var_to_string name in
    match List.find ~f:(fun (n, _) -> String.equal n name) env.locals with
    | Some _ -> (w, env, [local_get_s name])
    | None -> (
      match func_symbol_type w name with 
      | Some (FuncSymbol fs, TypeSymbol {tdetails = FuncType (input, output); _}) ->
        let no_of_args = List.length input in
        let func_name, w = Partial_call.create_helper w at ~name ~no_of_args in 
        let env, func_alloc_name, e = Partial_call.create_memory_block env at ~f:[func_symbol func_name] ~no_of_args ~current_args:[] in
        w, env, e @ [local_get_s func_alloc_name]
        
      | _ -> 
        w, env, [data_symbol name]
      )
    )
  | E_iterator (C_MAP, ((item_name, item_type), body), ({type_expression = {type_content = T_list _; _}; _} as col)) -> 
    let item = var_to_string item_name in
    let next_item = unique_name "next_item" in
    let result = unique_name "result" in
    let result_iter = unique_name "result_iter" in
    let result_iter_next = unique_name "result_iter_next" in

    let env = add_locals env [(item, T.NumType I32Type); (result, T.NumType I32Type); (result_iter, T.NumType I32Type); (result_iter_next, T.NumType I32Type); (next_item, T.NumType I32Type)] in
    let w, env, col = expression ~raise w env col in
    let w, env, body = expression ~raise w env body in
    
    w, env, 
      col 
      @
      [
      local_set_s item;

      local_get_s item;
      if_ 
        (ValBlockType (Some (T.NumType I32Type)))
        ([ 

          (* allocate memory for the first block *)
          const 8l;
          call_s "malloc";
          local_tee_s result_iter;
          local_set_s result;
          
          loop (ValBlockType (Some (T.NumType I32Type))) 
          (
            [
              
              (* get the next item *)
              local_get_s item;
              const 4l;
              i32_add;
              load;
              local_set_s next_item;


              (* allocate memory for the next item in the list if necessary *)
              local_get_s next_item;
              load;
              if_ 
                (ValBlockType None)
                [
                  const 8l;
                  call_s "malloc";
                  local_set_s result_iter_next;
                ]
                [
                  data_symbol "C_LIST_EMPTY";
                  local_set_s result_iter_next;
                ];

              (* store the item of the previous list in the new one *)  
              local_get_s result_iter;
              local_get_s item;
              load;
              load;
              store;

              local_get_s result_iter;
              local_set_s item;
              
              local_get_s result_iter;
              
            ]
            @
            body
            @
            [
              store;
              
              local_get_s result_iter;
              const 4l; 
              i32_add;
              local_get_s result_iter_next;
              store;
              
              local_get_s result_iter_next;
              local_set_s result_iter;


              

              (* check to see if the loop needs to continue *)
              local_get_s next_item;
              local_set_s item;
      
              local_get_s result;
      (* load; *)
              local_get_s next_item;
              load;
              const 0l;
              i32_ne;
              br_if 0l;
            ]
          );
          

          
         
        ]
        
        )
        [
          data_symbol "C_LIST_EMPTY"
        ];
      ]
      
  | E_iterator (kind, ((item_name, item_type), body), col) -> 
    raise.error (not_supported e)
  | E_fold (((name, tv), body), ({type_expression = {type_content = T_list _; _}; _} as col), initial) -> 
    let item = unique_name "item" in
    let init = unique_name "init" in
    let next_item = unique_name "next_item" in
    let result = unique_name "result" in
    let name = var_to_string name in
    let env = add_locals env [
      (name, T.NumType I32Type);  
      (init, T.NumType I32Type); 
      (item, T.NumType I32Type); 
      (next_item, T.NumType I32Type);
      (result, T.NumType I32Type);
    ]
    in
    let w, env, initial = expression ~raise w env initial in
    let w, env, col = expression ~raise w env col in
    let w, env, body = expression ~raise w env body in
    let w, env, tuple = Datatype.Pair.create w env [local_get_s init] [local_get_s item; load]  in
    w, env, 
    initial 
    @
    [
      local_set_s init;
    ]
    @
    col 
    @
    [
      local_set_s item;

      loop (ValBlockType (Some (T.NumType I32Type))) 
      (
      tuple
      @ 
      [
        local_set_s name;
      ]
      @
      body 
      @
      [        
        local_set_s result;
        
        local_get_s result;
        local_set_s init;

        (* check to see if the loop needs to continue *)
        local_get_s item;
        const 4l;
        i32_add;
        load;
        local_set_s next_item;

        local_get_s next_item;
        local_set_s item;

        local_get_s result;

        local_get_s next_item;
        load;
        const 0l;
        i32_ne;
        br_if 0l;
      ]);
    ]
  | E_fold (((name, tv), body), ({type_expression = {type_content = _; _}; _} as col), init) -> 
    raise.error (not_supported e)
  | E_fold_right (((name , tv) , body) , (({type_expression = {type_content = T_list _; _}; _} as collection), elem_tv) , initial) -> 
    let col_item = unique_name "item" in
    let init = unique_name "init" in
    let helper_fn_name = unique_name "helper_fn" in
    let helper_fn_name = unique_name "tl" in
    let name = var_to_string name in
    
    let result = unique_name "result" in
    let env = add_locals env [
      (init, T.NumType I32Type);   
      (col_item, T.NumType I32Type);         
      (name, T.NumType I32Type);  
     
    ]
    in

    let w, env, initial = expression ~raise w env initial in    
    let w, env, col = expression ~raise w env collection in
    let w, env, body = expression ~raise w env body in

    (* create a helper function here *)
    let s = A.{
      name    = helper_fn_name;
      details = Function;
    }
    in
    let s = S.{ it = s; at } in
    let w = {w with symbols = s :: w.symbols } in
    let hd  = unique_name "hd" in
    let tl = unique_name "tl" in
    let w, env, tuple = Datatype.Pair.create w env [local_get_s hd; load] [local_get_s init] in

    
    let f_body args = [
      (* check if the item points to an empty list *)
      local_get_s col_item;
      data_symbol "C_LIST_EMPTY";
      compare_eq;
      if_ (ValBlockType (Some (NumType I32Type)))
        [
          local_get_s init
        ]
        (
          [

            local_get_s col_item;
            local_set_s hd;

            (* iterate through the items in the list from left to right *)
            local_get_s col_item;
            const 4l;
            i32_add;
            load;
            local_set_s col_item;
          ]
          @
          args 
          @
         [
            call_s helper_fn_name;
            local_set_s init;

            local_get_s hd;
            local_set_s col_item;            

        ]
        @
        tuple
        @ 
        [
          local_set_s name;
        ]
        @
        body 
        @
        [
          local_set_s result;     
          local_get_s result;
        ])
    ]
    in
    let required = find_missing (f_body []) in
    let required_arguments = (
      List.fold_left 
        ~f:(fun a s -> 
            a @ 
            [
              local_get_s s              
            ]
          ) 
        ~init:[] 
        required.missing_arguments
    )
    in
    let f_body = f_body required_arguments in
    let required_locals = List.map ~f:(fun f -> (f, T.NumType I32Type)) required.missing_arguments @ List.map ~f:(fun f -> (f, T.NumType I32Type)) required.missing_locals in
    let f = A.FuncSymbol {
      name   = helper_fn_name;
      ftype  = helper_fn_name ^ "_type";
      locals = required_locals;
      body = f_body
    } 
    in

    let f = S.{it = f; at} in
    
    let t = A.TypeSymbol {
      tname = helper_fn_name ^ "_type";
      tdetails = FuncType (List.map ~f:(fun f -> T.NumType I32Type) required.missing_arguments, [NumType I32Type])
    } 
    in
    let t = S.{ it = t; at } in

    (* call the helper function here *)
    let w = {
      w with
        types   = t :: w.types;
        funcs   = f :: w.funcs;
    } in 
    w, env, 
    initial 
    @
    [
      local_set_s init;
    ]
    @
    col 
    @
    [
      local_set_s col_item;

    ]
    @
    required_arguments
    @
    [
      call_s helper_fn_name;
    ]
  | E_fold_right _ ->
    raise.error (not_supported e)
  | E_if_bool (test, t, f) -> 
    let w, env, test = expression ~raise w env test in
    let w, env, t = expression ~raise w env t in
    let w, env, f = expression ~raise w env f in
    let return_type = Some (T.NumType I32Type) in (* TODO properly get this *)
    w, env, [
      S.{it = A.Block (ValBlockType return_type, 
        [
          {it = A.Block (ValBlockType return_type,
            test 
            @
            [
              br_if 0l
            ]
            @ 
            f
            @ 
            [
              br_if 1l
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
  | E_if_none (test, none_e, ((some_arg, some_arg_type), some_e)) -> 
    (* TODO: check handling of locals *)
    let some_arg = var_to_string some_arg in
    let testing = unique_name "testing" in
    let env = Env.add_local env (some_arg, T.NumType I32Type) in
    let env = Env.add_local env (testing, T.NumType I32Type) in
    let w, env, test = expression ~raise w env test in
    let w, env, none_e = expression ~raise w env none_e in
    let w, env, some_e = expression ~raise w env some_e in
    
    let return_type = Some (T.NumType I32Type) in (* TODO properly get this *)
    w, env, 
      test 
      @
      [
        local_set_s testing;
        S.{it = A.Block (ValBlockType return_type, 
          [
            {it = A.Block (ValBlockType None,
              [
                local_get_s testing;
                br_if 0l
              ]
              @ 
              none_e
              @ 
              [
                br 1l
              ]
            );
            at
            }
          ]
          @ 
          [
            local_get_s testing;
            const 4l;
            i32_add;
            load;
            local_set_s some_arg;
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
    let data = unique_name "data" in
    let env = add_locals env [(hd, NumType I32Type); (tl, NumType I32Type); (data, NumType I32Type)] in
    let w, env, matchee_e = expression ~raise w env matchee in
    let w, env, nil_e = expression ~raise w env nil in
    let w, env, cons_e = expression ~raise w env cons in
    let return_type = Some (T.NumType I32Type) in (* TODO properly get this *)
    w, env,
      matchee_e
      @
      [
        local_tee_s data;
        load;
        local_set_s hd;
        local_get_s data;
        const 4l;
        i32_add;
        load;
        local_set_s tl;

        S.{it = A.Block (ValBlockType return_type, 
          [
            {it = A.Block (ValBlockType None,
              [ local_get_s data;
              
                data_symbol "C_LIST_EMPTY";
                i32_eq;
                
                br_if 0l
              ]
              @ 
              cons_e
              @ 
              [
                br 1l
              ]
            );
            at
            }
          ]
          @ 
          nil_e  
        );
        at
      };  
    ]
  | E_if_left (matchee, ((name_l, _), left), ((name_r, _), right)) -> 
    (* Variants *)
    let matchee_name = unique_name "matchee" in
    let name_l = var_to_string name_l in
    let name_r = var_to_string name_r in
    let env = add_locals env [(name_l, NumType I32Type); (name_r, NumType I32Type); (matchee_name, NumType I32Type)] in
    let w, env, matchee_e = expression ~raise w env matchee in
    let w, env, left_e = expression ~raise w env left in
    let w, env, right_e = expression ~raise w env right in
    let return_type = Some (T.NumType I32Type) in (* TODO properly get this *)
    w, env, 
    
    
    matchee_e 
    @
    [
      local_set_s matchee_name; 
      S.{it = A.Block (ValBlockType return_type, 
        [
          {it = A.Block (ValBlockType None,
            [
              local_get_s matchee_name;
              load;
              br_if 0l;
              local_get_s matchee_name;
              const 4l;
              i32_add;
              load;
              local_set_s name_l;
            ]
            @ 
            left_e
            @ 
            [
              br 1l
            ]
          );
          at
          }
        ]
        @ 
        [
          local_get_s matchee_name;
          const 4l;
          i32_add;
          load;
          local_set_s name_r;
        ]
        @
        right_e
      );
      at
      }]
  | E_let_in
      ( {content = E_closure _; _ },
        _inline,
        ((name, _type), e2) ) ->
    raise.error (not_supported e)
  | E_let_in (e1, _inline, ((name, typex), e2)) ->
    let name = var_to_string name in
    let w, env, e1 = expression ~raise w env e1 in
    let env = add_local env (name, T.NumType I32Type) in
    let w, env, e2 = expression ~raise w env e2 in
    (w, env, e1 @ [local_set_s name] @ e2)
  | E_tuple _ -> raise.error (not_supported e)
  | E_let_tuple (tuple, (values, rhs)) ->
    let w, env, tuple = expression ~raise w env tuple in
    let tuple_name = var_to_string (Value_var.fresh ~name:"let_tuple" ()) in
    let t = tuple @ [local_set_s tuple_name] in
    let env = add_local env (tuple_name, T.NumType I32Type) in
    let env, e =
      List.foldi
        ~f:(fun i (env, all) (name, _) ->
          let name = var_to_string name in
          ( add_local env (name, T.NumType I32Type),
            all
            @ [
              local_get_s tuple_name;
              const Int32.(4l * Int32.of_int_exn i);
              i32_add;
              load;
              local_set_s name;
            ]
          )
        )
        ~init:(env, []) values
    in
    let w, env, e2 = expression ~raise w env rhs in
    (w, env, t @ e @ e2)
  | E_proj (expr, i, count) -> 
    let w, env, expr = expression ~raise w env expr in
    w, env, 
    expr 
    @
    [
      const Int32.(4l * Int32.of_int_exn i);
      load
    ]
  | E_update (_,_,_,_) -> raise.error (not_supported e)
  | E_raw_wasm (local_symbols, code, args) -> 
    
    let w, env, args = List.fold_left ~f:(fun (w, env, e) i -> 
      let w, env, expr = expression ~raise w env i in
      w, env, e @ expr
    ) ~init:(w, env, []) args 
    in
    (w, add_locals env local_symbols, args @ code)
  | E_create_contract (_,_,_,_) -> raise.error (not_supported e)

  (* Are these actually used? *)
  | E_constant {cons_name = C_LOOP_CONTINUE; arguments} -> raise.error (not_supported e)
  | E_constant {cons_name = C_LOOP_STOP; arguments} -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP; arguments } -> raise.error (not_supported e)
  | E_constant {cons_name = C_BIG_MAP; arguments } -> raise.error (not_supported e)
  
  | E_constant {cons_name = C_MAP_GET; arguments } -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP_GET_FORCE; arguments } -> raise.error (not_supported e)  
  | E_global_constant (_,_) -> raise.error (not_supported e) (* is this actually used? *)

  (* Inline Michelson is not supported in wasm contracts *)
  | E_raw_michelson _ -> raise.error (michelson_insertion e.location)

  (* catch all *)
  | E_constant {cons_name; _} -> raise.error (not_supported e)  

  (* Mutability stuff *)
  | E_let_mut_in (_, _)       -> raise.error (not_supported e)
  | E_deref       _           -> raise.error (not_supported e)
  | E_assign     (_, _)       -> raise.error (not_supported e)
  | E_for_each   (_, _,_)     -> raise.error (not_supported e)
  | E_for        (_, _, _, _) -> raise.error (not_supported e)
  | E_while      (_, _)       -> raise.error (not_supported e)

  

let func I.{binder; body} =
  let rec aux arguments body =
    match body.I.content with
    | E_closure {binder; body} -> aux (binder :: arguments) body
    | _ -> (List.rev arguments, body)
  in
  aux [binder] body

let rec toplevel_bindings ~raise :
    I.expression -> string -> W.Ast.module_' -> W.Ast.module_' =
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
  | E_let_in ({content = E_closure c; _}, _inline, ((name, type_), e2))
    ->
    let name = var_to_string name in
    let arguments, body = func c in
    let env =
      List.fold_left ~f:(fun env i -> add_local env (var_to_string i, T.NumType I32Type)) ~init:(Env.make_env()) arguments 
    in
    let w, env, body = expression ~raise w env body in
    let env = Validation.check w env body in
    let type_arg = List.map ~f:(fun _ -> T.NumType I32Type) arguments in
    let return_type =
      match type_.type_content with
      | I.T_function (_, {type_content = I.T_base TB_unit; _}) -> 
        assert(Poly.equal env.operand_stack None);
        []
      | _ -> 
        assert(Poly.equal env.operand_stack (Some (T.NumType I32Type, Next None)));
        [T.NumType I32Type]
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
          w.funcs @ [{it = FuncSymbol {name; ftype = name ^ "_type"; locals = env.locals; body}; at}];
      }
    in
    toplevel_bindings ~raise e2 entrypoint w
  | E_let_in
      ( {content = E_literal (Literal_int z); _},
        _inline,
        ((name, _type), e2) ) ->
    (* we convert these to in memory values *)
    let name = var_to_string name in
    let data, symbols = convert_to_memory name z in
    toplevel_bindings ~raise e2 entrypoint
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
                    locals = [
                      ("parameter", T.NumType I32Type); 
                      ("storage", T.NumType I32Type); 
                      ("entrypoint_tuple", T.NumType I32Type);
                      ("result", T.NumType I32Type); 
                    ];
                    body =
                      [
                        const 8l ;
                        call_s "malloc" ;
                        local_set_s "entrypoint_tuple" ;
                        local_get_s "entrypoint_tuple" ;
                        local_get_s "parameter" ;
                        store ;

                        local_get_s "entrypoint_tuple" ;
                        const 4l ;
                        i32_add ;
                        local_get_s "storage" ;
                        store ;

                        local_get_s "entrypoint_tuple" ;
                        call_s actual_name;
                        local_tee_s "result";
                        const 4l;
                        i32_add;
                        load;
                        load;
                        
                        call_s "print";
                        
                        local_get_s "result";
                      ];
                  };
                at;
              };
            ];
      }
    in
    w
  | _ -> raise.error(not_supported e)

let compile ~raise : I.expression -> string -> string -> W.Ast.module_ =
 fun e filename entrypoint ->
  let w = Default_module.mod_ in
  let at = location_to_region e.location in
  global_offset := Default_module.offset;
  let w = toplevel_bindings ~raise e entrypoint w.it in
  let elems_i = List.mapi ~f:(fun i _ -> elem at i) (List.filter ~f:(fun f -> match f.it.idesc.it with FuncImport _ | FuncImport_symbol _ -> true | _ -> false) w.imports) in
  let elems = List.mapi ~f:(fun i _ -> elem at (List.length elems_i + i)) w.funcs in
  let w = {w with 
    elems = elems_i @ elems;
    symbols = w.symbols @ [
      {it = {
        name = "table";
        details = Table
      };
      at
      }

    ]
  } in
  S.{it = w; at}
 