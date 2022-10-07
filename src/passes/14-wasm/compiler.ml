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
  let name, hash = Value_var.internal_get_name_and_counter name in
  name ^ "#" ^ string_of_int hash

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
  let data_symbol = data_symbol at in
  let func_symbol = func_symbol at in
  let elem = elem at in
  let compare_eq = compare_eq at in
  let if_ = if_ at in 
  let br_if = br_if at in
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
      @ l1
      @ [
        store;
        local_get_s cons;
        const 4l;
        i32_add;
        ]
      @ l2
      @ [
        store;
        local_get_s cons;
        ] )

  (* Pair *)
  | E_constant {cons_name = C_PAIR; arguments = [e1; e2]} ->
    let pair = var_to_string (Value_var.fresh ~name:"C_PAIR" ()) in
    let w, env, e1 = expression ~raise w env e1 in
    let w, env, e2 = expression ~raise w env e2 in
    let e =
      S.[const 8l; call_s "malloc"; local_set_s pair; local_get_s pair]
      @ e1
      @ S.[store; local_get_s pair; const 4l; i32_add]
      @ e2
      @ S.[store; local_get_s pair]
    in
    (w, add_local env (pair, T.NumType I32Type), e)
  | E_constant {cons_name = C_CAR; arguments = [cons]} ->
    let w, env, cons = expression ~raise w env cons in
    ( w,
      env,
      cons @ [load]
    )
  | E_constant {cons_name = C_CDR; arguments = [cons]} ->
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
  | E_constant {cons_name = C_CALL; arguments = [param; mutez; contract] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_CONTRACT; arguments = [address] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_CONTRACT_OPT; arguments = [address] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_CONTRACT_WITH_ERROR; arguments = [address; msg] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_CONTRACT_ENTRYPOINT; arguments = [entrypoint; address] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_CONTRACT_ENTRYPOINT_OPT; arguments = [entrypoint; address] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_ADDRESS; arguments = [address] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_SELF; arguments = [entrypoint] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_SELF_ADDRESS; arguments = [] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_IMPLICIT_ACCOUNT; arguments = [keyhash] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_SET_DELEGATE; arguments = [keyhash] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_CREATE_CONTRACT; arguments = [operation_list_init;keyhash;mutez;init] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_OPEN_CHEST; arguments = [chest_key; chest; n] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_VIEW; arguments = [view_name; t; address] } -> raise.error (not_supported e)

  | E_application _ ->
    let rec aux w env (result: A.instr list) (expr: I.expression) =
      match expr.I.content with
      | E_application (func, e) -> 
        let w, env, e = expression ~raise w env e in 
        aux w env (result @ e) func
      | E_variable v ->        
        let name = var_to_string v in
        (match (func_symbol_type w name) with 
        | Some (FuncSymbol fs, TypeSymbol {tdetails = FuncType (input, output); _}) -> 
          let no_of_args = List.length input in
          if no_of_args = List.length result then 
            (w, env, result @ [call_s name])
          else (
            let unique_name = Value_var.fresh ~name:"e_variable_partial" () in
            let func_alloc_name = var_to_string unique_name in
            w, add_local env (func_alloc_name, NumType I32Type), 
            [
              const Int32.(12l + of_int_exn (List.length result));
              call_s "malloc";
              
              local_tee_s func_alloc_name;
              func_symbol name;
              store;

              local_get_s func_alloc_name;
              const 4l;
              i32_add;
              const (Int32.of_int_exn no_of_args); 
              store;

              local_get_s func_alloc_name;
              const 8l;
              i32_add;
              const (Int32.of_int_exn (List.length result));
              store
            ]
            @
            (let a, i = List.fold_left ~f:(fun (all, index) f -> 
              (all @
              [
                local_get_s func_alloc_name;
                const Int32.(index * 4l);
                i32_add;
                f;
                store
              ], Int32.(index + 1l)
              )
            ) ~init:([], 3l) result
            in a
            )
            @
            [
              local_get_s func_alloc_name
            ]
          )
        | _ -> 
          let lt = expr.type_expression in
          let unique_name = Value_var.fresh ~name:"Call_indirect" () in
          let indirect_name = var_to_string unique_name in
          
          let total_args_length = Value_var.fresh ~name:"total_args_length" () in
          let total_args_length = var_to_string total_args_length in
          
          let current_args_length = Value_var.fresh ~name:"current_args_length" () in
          let current_args_length = var_to_string current_args_length in

          let counter = Value_var.fresh ~name:"counter" () in
          let counter = var_to_string counter in

          let unique_name = Value_var.fresh ~name:"Call_indirect" () in
          ({w with 
            types = w.types
              @ 
              [type_ ~name:indirect_name ~typedef:(FuncType ([NumType I32Type], [NumType I32Type]))]
          }, add_locals env [(total_args_length, NumType I32Type); (current_args_length, NumType I32Type); (counter, NumType I32Type)], [
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
          (let a, i = List.fold_left ~f:(fun (all, index) f -> 
            (all @
            [
              local_get_s name;

              local_get_s current_args_length;
              const index;
              i32_add;
              const 4l;
              i32_mul;
              
              i32_add;
              f;
              store
            ], Int32.(index + 1l)
            )
          ) ~init:([], 3l) result
          in a
          )
          @
          [
            local_get_s current_args_length;
            const (Int32.of_int_exn (List.length result));
            i32_add;
            local_get_s total_args_length;
            compare_eq;
            if_ 
              (ValBlockType (Some (NumType I32Type))) 
              [
                local_get_s name;
                const 12l;
                i32_add;
                local_get_s name;
                load;
                call_indirect_s indirect_name
              ]
              [
                local_get_s name
              ]
             
          ]) 
        )
      | E_raw_wasm (local_symbols, code) -> 
        (w, add_locals env local_symbols, result @ code)
      | _ -> raise.error (not_supported e)
    in
    let w, env, args = aux w env [] e in
    (w, env, args)
  | E_variable name -> (
    let name = var_to_string name in
    match List.find ~f:(fun (n, _) -> String.equal n name) env.locals with
    | Some _ -> (w, env, [local_get_s name])
    | None -> (
      match func_symbol_type w name with 
      | Some (FuncSymbol fs, TypeSymbol {tdetails = FuncType (input, output); _}) -> 
        let unique_name = Value_var.fresh ~name:"e_variable_func" () in
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
          let t = A.TypeSymbol {
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
                local_get_s "foo";
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
          {w with funcs = f :: w.funcs; types = t :: w.types; symbols = s :: w.symbols}
        )
        in
        let env = add_local env (func_alloc_name, NumType I32Type) in
        w, env, [
          const Int32.(12l + Int32.of_int_exn no_of_args);
          call_s "malloc";
          
          local_tee_s func_alloc_name;
          func_symbol func_name;
          store;

          local_get_s func_alloc_name;
          const 4l;
          i32_add;
          const (Int32.of_int_exn no_of_args); 
          store;

          local_get_s func_alloc_name;
          const 8l;
          i32_add;
          const 0l;
          store;

          local_get_s func_alloc_name
      ] 
      | _ -> 
        w, env, [data_symbol name]
      )
    )
  | E_iterator (kind, ((item_name, item_type), body), collection) -> raise.error (not_supported e)
  | E_fold (((name, tv), body), col, init) -> raise.error (not_supported e)
  | E_fold_right _ -> raise.error (not_supported e)
  | E_if_bool (test, t, f) -> 
    let w, env, test = expression ~raise w env test in
    let w, env, t = expression ~raise w env t in
    let w, env, f = expression ~raise w env f in
    let return_type = Some (T.NumType I32Type) in (* TODO properly get this *)
    w, env, [
      S.{it = A.Block (ValBlockType return_type, 
        [
          {it = A.Block (ValBlockType None,
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
  | E_if_none (test, none_e, (_, some_e)) -> 
    (* TODO: check handling of locals *)
    let w, env, test = expression ~raise w env test in
    let w, env, none_e = expression ~raise w env none_e in
    let w, env, some_e = expression ~raise w env some_e in
    let return_type = Some (T.NumType I32Type) in (* TODO properly get this *)
    w, env, [
      S.{it = A.Block (ValBlockType return_type, 
        [
          {it = A.Block (ValBlockType None,
            test 
            @
            [
              br_if 0l
            ]
            @ 
            none_e
            @ 
            [
              br_if 1l
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
    let env = add_locals env [(hd, NumType I32Type); (tl, NumType I32Type)] in
    let w, env, matchee_e = expression ~raise w env matchee in
    let w, env, nil_e = expression ~raise w env nil in
    let w, env, cons_e = expression ~raise w env cons in
    let return_type = Some (T.NumType I32Type) in (* TODO properly get this *)
    w, env, [
      S.{it = A.Block (ValBlockType return_type, 
        [
          {it = A.Block (ValBlockType None,
          matchee_e
            @
            [
              data_symbol "C_LIST_EMPTY";
              {it = A.Compare (I32 A.I32Op.Ne); at };
              br_if 0l
            ]
            @ 
            nil_e
            @ 
            [
              br_if 1l
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
    let env = add_locals env [(name_l, NumType I32Type); (name_r, NumType I32Type)] in
    let w, env, matchee_e = expression ~raise w env matchee in
    let w, env, left_e = expression ~raise w env left in
    let w, env, right_e = expression ~raise w env right in
    let return_type = Some (T.NumType I32Type) in (* TODO properly get this *)
    w, env, [
      S.{it = A.Block (ValBlockType return_type, 
        [
          {it = A.Block (ValBlockType None,
          matchee_e
            @
            [
              br_if 0l
            ]
            @ 
            left_e
            @ 
            [
              br_if 1l
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
  | E_raw_wasm (local_symbols, code) -> (w, add_locals env local_symbols, code)
  | E_create_contract (_,_,_,_) -> raise.error (not_supported e)

  (* Are these actually used? *)
  | E_constant {cons_name = C_LOOP_CONTINUE; arguments} -> raise.error (not_supported e)
  | E_constant {cons_name = C_LOOP_STOP; arguments} -> raise.error (not_supported e)
  | E_constant {cons_name = C_MAP; arguments } -> raise.error (not_supported e)
  | E_constant {cons_name = C_BIG_MAP; arguments } -> raise.error (not_supported e)
  | E_constant {cons_name = C_LEFT; arguments = [a; b] } -> raise.error (not_supported e)
  | E_constant {cons_name = C_RIGHT; arguments = [a; b]} -> raise.error (not_supported e)
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
    I.expression -> W.Ast.module_' -> W.Ast.module_' =
 fun e w ->
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
  let data_symbol = data_symbol at in
  let func_symbol = func_symbol at in
  let elem = elem at in
  let compare_eq = compare_eq at in
  let if_ = if_ at in 
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
    toplevel_bindings ~raise e2 w
  | E_let_in
      ( {content = E_literal (Literal_int z); _},
        _inline,
        ((name, _type), e2) ) ->
    (* we convert these to in memory values *)
    let name = var_to_string name in
    let data, symbols = convert_to_memory name z in
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
                        call_s actual_name ;
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
  let w = toplevel_bindings ~raise e w.it in
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
