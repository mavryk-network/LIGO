open Simple_utils.Trace
open Simple_utils
open Ligo_interpreter.Types
open Ligo_interpreter.Combinators
open Ligo_prim
module AST = Ast_aggregated
include AST.Types
module Env = Ligo_interpreter.Environment
module Monad = Execution_monad
module ModRes = Preprocessor.ModRes

type interpreter_error = Errors.interpreter_error

(* [resolve_contract_file ~mod_res ~source_file ~contract_file] tries to resolve
   [contract_file] w.r.t. to process directory
   if that fails it tries to resolve it as a relative path w.r.t. directory of [source_file]
   if that fails it tries to resolve it as a package path using [mod_res] *)
let resolve_contract_file ~mod_res ~source_file ~contract_file =
  match Sys_unix.file_exists contract_file with
  | `Yes -> contract_file
  | `No | `Unknown ->
    (match source_file with
     | Some source_file ->
       let d = Filename.dirname source_file in
       let s = Filename.concat d contract_file in
       (match Sys_unix.file_exists s with
        | `Yes -> s
        | `No | `Unknown -> ModRes.Helpers.resolve ~file:contract_file mod_res)
     | None -> ModRes.Helpers.resolve ~file:contract_file mod_res)


let get_file_from_location loc =
  let open Option in
  let* reg = Location.get_file loc in
  let file = reg#file in
  if String.(file = "") then None else Some file


let replace_loc_if_blank e location =
  match get_file_from_location e.location with
  | Some _ -> e
  | None -> { e with location }


let check_value value =
  let open Monad in
  match value with
  | V_Func_val
      { orig_lambda
      ; rec_name = _
      ; arg_binder = _
      ; arg_mut_flag = _
      ; body = _
      ; env = _
      } -> call @@ Check_obj_ligo orig_lambda
  | _ -> return ()


let monad_option error v =
  let open Monad in
  match v with
  | None -> fail error
  | Some s -> return s


let wrap_compare_result comp cmpres loc calltrace =
  let open Monad in
  let open Ligo_prim.Constant in
  match comp with
  | C_EQ -> return (cmpres = 0)
  | C_NEQ -> return (cmpres <> 0)
  | C_LT -> return (cmpres < 0)
  | C_LE -> return (cmpres <= 0)
  | C_GT -> return (cmpres > 0)
  | C_GE -> return (cmpres >= 0)
  | _ -> fail @@ Errors.meta_lang_eval loc calltrace @@ v_string "Only valid comparisons are: EQ, NEQ, LT, LE, GT, GE"

let compare_constants ~raise o1 o2 loc calltrace =
  match o1, o2 with
  | V_Ct (C_int64 a'), V_Ct (C_int64 b') ->
    Int64.compare a' b'
  | V_Ct (C_int a'), V_Ct (C_int b')
  | V_Ct (C_mutez a'), V_Ct (C_mutez b')
  | V_Ct (C_timestamp a'), V_Ct (C_timestamp b')
  | V_Ct (C_nat a'), V_Ct (C_nat b') ->
    Z.compare a' b'
  | V_Ct (C_bool a), V_Ct (C_bool b) ->
    Bool.compare a b
  | V_Ct (C_address a), V_Ct (C_address b) ->
    Tezos_state.compare_account a b
  | V_Ct (C_key_hash a), V_Ct (C_key_hash b) ->
    Tezos_crypto.Signature.Public_key_hash.compare a b
  | V_Ct C_unit, V_Ct C_unit ->
    0
  | V_Ct (C_string a'), V_Ct (C_string b') ->
    String.compare a' b'
  | V_Ct (C_bytes a'), V_Ct (C_bytes b') ->
    Bytes.compare a' b'
  | V_Ct (C_contract { address = addr1; entrypoint = entr1 })
    , V_Ct (C_contract { address = addr2; entrypoint = entr2 })
    -> Tuple2.compare ~cmp1:Tezos_state.compare_account ~cmp2:(Option.compare String.compare) (addr1, entr1) (addr2, entr2)
  | operand, operand' ->
    let msg =
      Format.asprintf
        "Comparison not supported: %a"
        (PP_helpers.pair Ligo_interpreter.PP.pp_value Ligo_interpreter.PP.pp_value)
        (operand, operand') in
    raise.error @@ Errors.meta_lang_eval loc calltrace @@ v_string msg

let rec apply_comparison ~raise
  :  Location.t -> calltrace -> Ast_aggregated.type_expression -> value -> value
  -> int
  =
 fun loc calltrace type_ operand operand' ->
  match operand, operand' with
  | (V_Michelson _ as a), (V_Michelson _ as b) ->
    Michelson_backend.compare_michelson ~raise loc a b
  | (V_Ct _ as v1), (V_Ct _ as v2) ->
    compare_constants ~raise v1 v2 loc calltrace
  | V_List xs, V_List ys ->
    let type_ = trace_option ~raise (Errors.generic_error ~calltrace loc "Expected list type") @@ AST.get_t_list type_ in
    List.compare (apply_comparison ~raise loc calltrace type_) xs ys
  | (V_Set s), (V_Set s') ->
    let type_ = trace_option ~raise (Errors.generic_error ~calltrace loc "Expected set type") @@ AST.get_t_set type_ in
    List.compare
      (apply_comparison ~raise loc calltrace type_)
      (List.dedup_and_sort ~compare:(apply_comparison ~raise loc calltrace type_) s)
      (List.dedup_and_sort ~compare:(apply_comparison ~raise loc calltrace type_) s')
  | V_Map m, V_Map m' ->
    let type_key, type_value = trace_option ~raise (Errors.generic_error ~calltrace loc "Expected map or big_map type") @@ AST.get_t_map_or_big_map type_ in
    let compare_key = apply_comparison ~raise loc calltrace type_key in
    let compare_value = apply_comparison ~raise loc calltrace type_value in
    let compare_kv = Tuple2.compare ~cmp1:compare_key ~cmp2:compare_value in
    let m = List.sort ~compare:compare_kv m in
    let m' = List.sort ~compare:compare_kv m' in
    List.compare compare_kv m m'
  | V_Record r, V_Record r' ->
    let { fields : row_element Record.t ; layout } = trace_option ~raise (Errors.generic_error ~calltrace loc "Expected a record type") (AST.get_t_record type_) in
    let row_kv = AST.Helpers.kv_list_of_t_record_or_tuple ~layout fields in
    let rec aux (row_kv : (Label.t * row_element) list) =
      match row_kv with
      | [] -> 0
      | (label, ({ associated_type ; _ } : row_element)) :: row_kv ->
        let value_a = Record.LMap.find label r in
        let value_b = Record.LMap.find label r' in
        match apply_comparison ~raise loc calltrace associated_type value_a value_b with
         | 0 -> aux row_kv
         | c -> c in
    aux row_kv
  | V_Construct (ctor_a, args_a), V_Construct (ctor_b, args_b) ->
    let { fields : row_element Record.t ; layout } = trace_option ~raise (Errors.generic_error ~calltrace loc "Expected a sum type") (AST.get_t_sum type_) in
    let order = AST.Helpers.kv_list_of_t_sum ~layout fields |> List.map ~f:fst in
    let ith_a, _ = List.findi_exn order ~f:(fun _i (Label l) -> String.equal l ctor_a) in
    let ith_b, _ = List.findi_exn order ~f:(fun _i (Label l) -> String.equal l ctor_b) in
    (match Int.compare ith_a ith_b with
     | 0 ->
       let { associated_type ; _ } : row_element = Record.LMap.find (Label ctor_a) fields in
       apply_comparison ~raise loc calltrace associated_type args_a args_b
     | c -> c)
  | V_Func_val _, V_Func_val _ | V_Gen _, V_Gen _ | V_Location _, V_Location _
  | V_Typed_address _, V_Typed_address _ | V_Ast_contract _, V_Ast_contract _
  | V_Mutation _, V_Mutation _ | V_Michelson_contract _, V_Michelson_contract _ ->
    (* NOTE: These are not comparable, either meta-LIGO or not supporting natural comparison *)
    let msg =
      Format.asprintf
        "Comparison not supported: %a"
        (PP_helpers.pair Ligo_interpreter.PP.pp_value Ligo_interpreter.PP.pp_value)
        (operand, operand') in
    raise.error @@ Errors.meta_lang_eval loc calltrace @@ v_string msg
  | ( ( V_Ct _
      | V_List _
      | V_Record _
      | V_Map _
      | V_Set _
      | V_Construct _
      | V_Michelson _
      | V_Mutation _
      | V_Func_val _
      | V_Michelson_contract _
      | V_Ast_contract _
      | V_Gen _
      | V_Location _
      | V_Typed_address _ )
    , ( V_Ct _
      | V_List _
      | V_Record _
      | V_Map _
      | V_Set _
      | V_Construct _
      | V_Michelson _
      | V_Mutation _
      | V_Func_val _
      | V_Michelson_contract _
      | V_Ast_contract _
      | V_Gen _
      | V_Location _
      | V_Typed_address _ ) ) ->
    let msg =
      Format.asprintf
        "Different value types, cannot be compared: %a"
        (PP_helpers.pair Ligo_interpreter.PP.pp_value Ligo_interpreter.PP.pp_value)
        (operand, operand') in
    raise.error @@ Errors.meta_lang_eval loc calltrace @@ v_string msg

let apply_comparison ~raise
  :  Location.t -> calltrace -> Ast_aggregated.type_expression -> Ligo_prim.Constant.constant' -> value -> value
  -> value Monad.t
  = fun loc calltrace type_ c operand operand' ->
    let open Monad in
    let cmpres = apply_comparison ~raise loc calltrace type_ operand operand' in
    let* b = wrap_compare_result c cmpres loc calltrace in
    return @@ v_bool b

let bind_param
  :  env -> Value_var.t -> Param.mutable_flag -> type_expression * value
  -> in_:(env -> 'a Monad.t) -> 'a Monad.t
  =
  let open Monad in
  fun env var mut_flag (type_, val_) ~in_ ->
    match mut_flag with
    | Immutable ->
      let env = Env.extend env var (type_, val_) in
      in_ env
    | Mutable ->
      let@ loc = Alloc val_ in
      let env = Env.extend env var (type_, V_Location loc) in
      let* result = in_ env in
      let@ () = Free loc in
      return result


let rec apply_operator ~raise ~steps ~(options : Compiler_options.t)
  :  Location.t -> calltrace -> AST.type_expression -> env -> Constant.constant'
  -> (value * AST.type_expression * Location.t) list -> value Monad.t
  =
 fun loc calltrace expr_ty env c operands ->
  let open Constant in
  let open Monad in
  let eval_ligo = eval_ligo ~raise ~steps ~options in
  let types = List.map ~f:(fun (_, b, _) -> b) operands in
  let operands = List.map ~f:(fun (a, _, _) -> a) operands in
  let error_type () =
    Errors.generic_error
      loc
      (Format.asprintf
         "Type error: evaluating constant %a with types:@.%a@."
         Ligo_prim.Constant.pp_constant'
         c
         (PP_helpers.list_sep_d AST.PP.type_expression)
         types)
  in
  let div_by_zero_str = v_string "Dividing by zero" in
  let nth_type n =
    trace_option ~raise (Errors.generic_error loc "Could not recover types")
    @@ List.nth types n
  in
  let return_contract_exec_exn = function
    | `Exec_ok gas -> return (v_nat gas)
    | `Exec_failed e -> fail @@ Errors.target_lang_error loc calltrace e
  in
  let return_contract_exec = function
    | `Exec_ok gas -> return (LC.v_ctor "Success" (LC.v_nat gas))
    | `Exec_failed e ->
      let>> a = State_error_to_value e in
      return a
  in
  let source_file = get_file_from_location loc in
  match c, operands with
  (* nullary *)
  | C_NONE, [] -> return @@ v_none ()
  | C_NONE, _ -> fail @@ error_type ()
  | C_UNIT, [] -> return @@ v_unit ()
  | C_UNIT, _ -> fail @@ error_type ()
  | C_NIL, [] -> return @@ V_List []
  | C_NIL, _ -> fail @@ error_type ()
  | C_TRUE, [] -> return @@ v_bool true
  | C_TRUE, _ -> fail @@ error_type ()
  | C_FALSE, [] -> return @@ v_bool false
  | C_FALSE, _ -> fail @@ error_type ()
  (* unary *)
  | C_NOT, [ V_Ct (C_bool a') ] -> return @@ v_bool (not a')
  | C_NOT, [ V_Ct (C_int a') ] -> return @@ v_int (Z.lognot a')
  | C_NOT, [ V_Ct (C_nat a') ] -> return @@ v_int (Z.lognot a')
  | C_NOT, _ -> fail @@ error_type ()
  | C_NEG, [ V_Ct (C_int a') ] -> return @@ v_int (Z.neg a')
  | C_NEG, [ V_Ct (C_bls12_381_g1 a') ] ->
    return @@ v_bls12_381_g1 (Bls12_381.G1.negate a')
  | C_NEG, [ V_Ct (C_bls12_381_g2 a') ] ->
    return @@ v_bls12_381_g2 (Bls12_381.G2.negate a')
  | C_NEG, [ V_Ct (C_bls12_381_fr a') ] ->
    return @@ v_bls12_381_fr (Bls12_381.Fr.negate a')
  | C_NEG, _ -> fail @@ error_type ()
  | C_INT, [ V_Ct (C_nat a') ] -> return @@ v_int a'
  | C_INT, [ V_Ct (C_bls12_381_fr a') ] ->
    return @@ v_int (Bls12_381.Fr.to_z a')
  | C_INT, _ -> fail @@ error_type ()
  | C_ABS, [ V_Ct (C_int a') ] -> return @@ v_nat (Z.abs a')
  | C_ABS, _ -> fail @@ error_type ()
  | C_SOME, [ v ] -> return @@ v_some v
  | C_SOME, _ -> fail @@ error_type ()
  | C_MAP_FIND_OPT, [ k; V_Map l ] ->
    (match List.Assoc.find ~equal:LC.equal_value l k with
     | Some v -> return @@ v_some v
     | None -> return @@ v_none ())
  | C_MAP_FIND_OPT, _ -> fail @@ error_type ()
  | C_MAP_FIND, [ k; V_Map l ] ->
    (match List.Assoc.find ~equal:LC.equal_value l k with
     | Some v -> return @@ v
     | None ->
       fail
       @@ Errors.meta_lang_eval
            loc
            calltrace
            (v_string @@ Predefined.Tree_abstraction.pseudo_module_to_string c))
  | C_MAP_FIND, _ -> fail @@ error_type ()
  (* binary *)
  | (C_EQ | C_NEQ | C_LT | C_LE | C_GT | C_GE), [ operand ; operand' ] ->
    (* we use the type of the first argument to guide comparison *)
    let type_ = nth_type 0 in
    apply_comparison ~raise loc calltrace type_ c operand operand'
  | (C_EQ | C_NEQ | C_LT | C_LE | C_GT | C_GE), _ -> fail @@ error_type ()
  | C_SUB, [ V_Ct (C_int64 a'); V_Ct (C_int64 b') ] ->
    return @@ v_int64 Int64.(a' - b')
  | C_SUB, [ V_Ct (C_int a' | C_nat a'); V_Ct (C_int b' | C_nat b') ] ->
    return @@ v_int (Z.sub a' b')
  | ( C_SUB
    , [ V_Ct (C_timestamp a'); V_Ct (C_timestamp b') ] )
    ->
    let res = Michelson_backend.Tezos_eq.timestamp_sub a' b' in
    return @@ v_int res
  | ( C_SUB
    , [ V_Ct (C_int a' | C_timestamp a'); V_Ct (C_timestamp b' | C_int b') ] )
    ->
    let res = Michelson_backend.Tezos_eq.timestamp_sub a' b' in
    return @@ v_timestamp res
  | C_SUB, [ V_Ct (C_mutez a'); V_Ct (C_mutez b') ] ->
    (match Michelson_backend.Tezos_eq.mutez_sub a' b' with
     | Some res -> return @@ v_mutez res
     | None ->
       fail
         (Errors.meta_lang_eval
            loc
            calltrace
            (v_string "Mutez underflow/overflow")))
  | C_SUB_MUTEZ, [ V_Ct (C_mutez a'); V_Ct (C_mutez b') ] ->
    (match Michelson_backend.Tezos_eq.mutez_sub a' b' with
     | Some res -> return @@ v_some @@ v_mutez res
     | None -> return @@ v_none ())
  | C_SUB, _ -> fail @@ error_type ()
  | C_SUB_MUTEZ, _ -> fail @@ error_type ()
  | C_CONS, [ v; V_List vl ] -> return @@ V_List (v :: vl)
  | C_CONS, _ -> fail @@ error_type ()
  | C_ADD, [ V_Ct (C_int64 a); V_Ct (C_int64 b) ] ->
    return @@ v_int64 Int64.(a + b)
  | C_ADD, [ V_Ct (C_int a); V_Ct (C_int b) ]
  | C_ADD, [ V_Ct (C_nat a); V_Ct (C_int b) ]
  | C_ADD, [ V_Ct (C_int a); V_Ct (C_nat b) ] ->
    let r = Z.add a b in
    return (v_int r)
  | C_ADD, [ V_Ct (C_nat a); V_Ct (C_nat b) ] ->
    let r = Z.add a b in
    return (v_nat r)
  | ( C_ADD
    , [ V_Ct (C_int a' | C_timestamp a'); V_Ct (C_timestamp b' | C_int b') ] )
    ->
    let res = Michelson_backend.Tezos_eq.timestamp_add a' b' in
    return @@ v_timestamp res
  | C_ADD, [ V_Ct (C_mutez a'); V_Ct (C_mutez b') ] ->
    (match Michelson_backend.Tezos_eq.mutez_add a' b' with
     | Some res -> return @@ v_mutez res
     | None ->
       fail
         (Errors.meta_lang_eval
            loc
            calltrace
            (v_string "Mutez underflow/overflow")))
  | C_ADD, [ V_Ct (C_bls12_381_g1 a); V_Ct (C_bls12_381_g1 b) ] ->
    let r = Bls12_381.G1.(add a b) in
    return (v_bls12_381_g1 r)
  | C_ADD, [ V_Ct (C_bls12_381_g2 a); V_Ct (C_bls12_381_g2 b) ] ->
    let r = Bls12_381.G2.(add a b) in
    return (v_bls12_381_g2 r)
  | C_ADD, [ V_Ct (C_bls12_381_fr a); V_Ct (C_bls12_381_fr b) ] ->
    let r = Bls12_381.Fr.(a + b) in
    return (v_bls12_381_fr r)
  | C_ADD, _ -> fail @@ error_type ()
  | C_MUL, [ V_Ct (C_int64 a); V_Ct (C_int64 b) ] ->
    return @@ v_int64 Int64.(a * b)
  | C_MUL, [ V_Ct (C_int a); V_Ct (C_int b) ]
  | C_MUL, [ V_Ct (C_nat a); V_Ct (C_int b) ]
  | C_MUL, [ V_Ct (C_int a); V_Ct (C_nat b) ] ->
    let r = Z.mul a b in
    return (v_int r)
  | C_MUL, [ V_Ct (C_nat a); V_Ct (C_nat b) ] ->
    let r = Z.mul a b in
    return (v_nat r)
  | C_MUL, [ V_Ct (C_nat a); V_Ct (C_mutez b) ] ->
    let r = Z.mul a b in
    return (v_mutez r)
  | C_MUL, [ V_Ct (C_mutez a); V_Ct (C_nat b) ] ->
    let r = Z.mul a b in
    return (v_mutez r)
  | C_MUL, [ V_Ct (C_bls12_381_g1 a); V_Ct (C_bls12_381_fr b) ] ->
    let r = Bls12_381.G1.(mul a b) in
    return (v_bls12_381_g1 r)
  | C_MUL, [ V_Ct (C_bls12_381_g2 a); V_Ct (C_bls12_381_fr b) ] ->
    let r = Bls12_381.G2.(mul a b) in
    return (v_bls12_381_g2 r)
  | C_MUL, [ V_Ct (C_bls12_381_fr a); V_Ct (C_bls12_381_fr b) ] ->
    let r = Bls12_381.Fr.(a * b) in
    return (v_bls12_381_fr r)
  | C_MUL, [ V_Ct (C_nat a); V_Ct (C_bls12_381_fr b) ] ->
    let r = Bls12_381.Fr.(b ** a) in
    return (v_bls12_381_fr r)
  | C_MUL, [ V_Ct (C_int a); V_Ct (C_bls12_381_fr b) ] ->
    let r = Bls12_381.Fr.(b ** a) in
    return (v_bls12_381_fr r)
  | C_MUL, [ V_Ct (C_bls12_381_fr a); V_Ct (C_nat b) ] ->
    let r = Bls12_381.Fr.(a ** b) in
    return (v_bls12_381_fr r)
  | C_MUL, [ V_Ct (C_bls12_381_fr a); V_Ct (C_int b) ] ->
    let r = Bls12_381.Fr.(a ** b) in
    return (v_bls12_381_fr r)
  | C_MUL, _ -> fail @@ error_type ()
  | C_DIV, [ V_Ct (C_int a'); V_Ct (C_int b') ]
  | C_DIV, [ V_Ct (C_int a'); V_Ct (C_nat b') ]
  | C_DIV, [ V_Ct (C_nat a'); V_Ct (C_int b') ] ->
    let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
    (match a with
     | Some (res, _) -> return @@ v_int res
     | None -> fail @@ Errors.meta_lang_eval loc calltrace div_by_zero_str)
  | C_DIV, [ V_Ct (C_nat a'); V_Ct (C_nat b') ] ->
    let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
    (match a with
     | Some (res, _) -> return @@ v_nat res
     | None -> fail @@ Errors.meta_lang_eval loc calltrace div_by_zero_str)
  | C_DIV, [ V_Ct (C_mutez a'); V_Ct (C_mutez b') ] ->
    let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
    (match a with
     | Some (res, _) -> return @@ v_nat res
     | None -> fail @@ Errors.meta_lang_eval loc calltrace div_by_zero_str)
  | C_DIV, [ V_Ct (C_mutez a'); V_Ct (C_nat b') ] ->
    let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
    (match a with
     | Some (res, _) -> return @@ v_mutez res
     | None -> fail @@ Errors.meta_lang_eval loc calltrace div_by_zero_str)
  | C_DIV, _ -> fail @@ error_type ()
  | C_MOD, [ V_Ct (C_int64 a'); V_Ct (C_int64 b') ] ->
    return @@ v_int64 Int64.(rem a' b')
  | C_MOD, [ V_Ct (C_int a'); V_Ct (C_int b') ]
  | C_MOD, [ V_Ct (C_int a'); V_Ct (C_nat b') ]
  | C_MOD, [ V_Ct (C_nat a'); V_Ct (C_int b') ] ->
    let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
    (match a with
     | Some (_, r) -> return @@ v_nat r
     | None -> fail @@ Errors.meta_lang_eval loc calltrace div_by_zero_str)
  | C_MOD, [ V_Ct (C_nat a'); V_Ct (C_nat b') ] ->
    let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
    (match a with
     | Some (_, r) -> return @@ v_nat r
     | None -> fail @@ Errors.meta_lang_eval loc calltrace div_by_zero_str)
  | C_MOD, _ -> fail @@ error_type ()
  | C_CONCAT, [ V_Ct (C_string a'); V_Ct (C_string b') ] ->
    return @@ v_string (a' ^ b')
  | C_CONCAT, [ V_Ct (C_bytes a'); V_Ct (C_bytes b') ] ->
    return @@ v_bytes (BytesLabels.cat a' b')
  | C_CONCAT, _ -> fail @@ error_type ()
  | C_OR, [ V_Ct (C_bool a'); V_Ct (C_bool b') ] -> return @@ v_bool (a' || b')
  | C_AND, [ V_Ct (C_bool a'); V_Ct (C_bool b') ] -> return @@ v_bool (a' && b')
  | C_XOR, [ V_Ct (C_bool a'); V_Ct (C_bool b') ] ->
    return @@ v_bool ((a' || b') && not (a' && b'))
  (* Bitwise operators *)
  | C_AND, [ V_Ct (C_int a'); V_Ct (C_nat b') ] ->
    let v = Z.logand a' b' in
    return @@ v_nat v
  | C_AND, [ V_Ct (C_nat a'); V_Ct (C_nat b') ] ->
    let v = Z.logand a' b' in
    return @@ v_nat v
  | C_AND, [ V_Ct (C_int64 a'); V_Ct (C_int64 b') ] ->
    return @@ v_int64 Int64.(a' land b')
  | C_OR, [ V_Ct (C_nat a'); V_Ct (C_nat b') ] ->
    let v = Z.logor a' b' in
    return @@ v_nat v
  | C_OR, [ V_Ct (C_int64 a'); V_Ct (C_int64 b') ] ->
    return @@ v_int64 Int64.(a' lor b')
  | C_XOR, [ V_Ct (C_nat a'); V_Ct (C_nat b') ] ->
    let v = Z.logxor a' b' in
    return @@ v_nat v
  | C_XOR, [ V_Ct (C_int64 a'); V_Ct (C_int64 b') ] ->
    return @@ v_int64 Int64.(a' lxor b')
  | C_OR, _ -> fail @@ error_type ()
  | C_AND, _ -> fail @@ error_type ()
  | C_XOR, _ -> fail @@ error_type ()
  | C_LSL, [ V_Ct (C_int64 a'); V_Ct (C_nat b') ] ->
    let b' = Z.to_int b' in
    return @@ v_int64 Int64.(shift_left a' b')
  | C_LSL, [ V_Ct (C_nat a'); V_Ct (C_nat b') ] ->
    let v = Michelson_backend.Tezos_eq.nat_shift_left a' b' in
    (match v with
     | Some v -> return @@ v_nat v
     | None -> fail @@ Errors.meta_lang_eval loc calltrace (v_string "Overflow"))
  | C_LSL, _ -> fail @@ error_type ()
  | C_LSR, [ V_Ct (C_int64 a'); V_Ct (C_nat b') ] ->
    let b' = Z.to_int b' in
    return @@ v_int64 Int64.(shift_right_logical a' b')
  | C_LSR, [ V_Ct (C_nat a'); V_Ct (C_nat b') ] ->
    let v = Michelson_backend.Tezos_eq.nat_shift_right a' b' in
    (match v with
     | Some v -> return @@ v_nat v
     | None -> fail @@ Errors.meta_lang_eval loc calltrace (v_string "Overflow"))
  | C_LSR, _ -> fail @@ error_type ()
  | C_LIST_EMPTY, [] -> return @@ V_List []
  | C_LIST_EMPTY, _ -> fail @@ error_type ()
  | ( C_LIST_MAP
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_List elts
      ] ) ->
    let lst_ty = nth_type 1 in
    let* ty =
      monad_option (Errors.generic_error lst_ty.location "Expected list type")
      @@ AST.get_t_list lst_ty
    in
    let* elts =
      Monad.bind_map_list
        (fun elt ->
          bind_param env arg_binder arg_mut_flag (ty, elt) ~in_:(fun env' ->
            eval_ligo body calltrace env'))
        elts
    in
    return (V_List elts)
  | C_LIST_MAP, _ -> fail @@ error_type ()
  | ( C_MAP_MAP
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_Map elts
      ] ) ->
    let map_ty = nth_type 1 in
    let* k_ty, v_ty =
      monad_option (Errors.generic_error map_ty.location "Expected map type")
      @@ AST.get_t_map map_ty
    in
    let* elts =
      Monad.bind_map_list
        (fun (k, v) ->
          bind_param
            env
            arg_binder
            arg_mut_flag
            (AST.t_pair k_ty v_ty, v_pair (k, v))
            ~in_:(fun env ->
              let* v' = eval_ligo body calltrace env in
              return @@ (k, v')))
        elts
    in
    return (V_Map elts)
  | C_MAP_MAP, _ -> fail @@ error_type ()
  | ( C_LIST_ITER
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_List elts
      ] ) ->
    let lst_ty = nth_type 1 in
    let* ty =
      monad_option (Errors.generic_error lst_ty.location "Expected list type")
      @@ AST.get_t_list lst_ty
    in
    Monad.bind_fold_list
      (fun _ elt ->
        bind_param env arg_binder arg_mut_flag (ty, elt) ~in_:(fun env ->
          eval_ligo body calltrace env))
      (v_unit ())
      elts
  | C_LIST_ITER, _ -> fail @@ error_type ()
  | ( C_MAP_ITER
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_Map elts
      ] ) ->
    let map_ty = nth_type 1 in
    let* k_ty, v_ty =
      monad_option (Errors.generic_error map_ty.location "Expected map type")
      @@ AST.get_t_map map_ty
    in
    Monad.bind_fold_list
      (fun _ kv ->
        bind_param
          env
          arg_binder
          arg_mut_flag
          (AST.t_pair k_ty v_ty, v_pair kv)
          ~in_:(fun env -> eval_ligo body calltrace env))
      (v_unit ())
      elts
  | C_MAP_ITER, _ -> fail @@ error_type ()
  (* ternary *)
  | ( C_LOOP_LEFT
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; init
      ] ) ->
    let init_ty = nth_type 1 in
    let rec aux cur_env =
      bind_param
        env
        arg_binder
        arg_mut_flag
        (init_ty, cur_env)
        ~in_:(fun env' ->
        let* ret = eval_ligo body calltrace env' in
        match ret with
        | V_Construct ("##Loop_continue", v) -> aux v
        | V_Construct ("##Loop_stop", v) -> return v
        | _ -> fail @@ error_type ())
    in
    aux init
  | C_LOOP_LEFT, _ -> fail @@ error_type ()
  | C_LOOP_CONTINUE, [ v ] -> return (v_ctor "##Loop_continue" v)
  | C_LOOP_CONTINUE, _ -> fail @@ error_type ()
  | C_LOOP_STOP, [ v ] -> return (v_ctor "##Loop_stop" v)
  | C_LOOP_STOP, _ -> fail @@ error_type ()
  | ( C_LIST_FOLD_LEFT
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; init
      ; V_List elts
      ] ) ->
    let acc_ty = nth_type 1 in
    let lst_ty = nth_type 2 in
    let* ty =
      monad_option (Errors.generic_error lst_ty.location "Expected list type")
      @@ AST.get_t_list lst_ty
    in
    Monad.bind_fold_list
      (fun prev elt ->
        let fold_args = v_pair (prev, elt) in
        bind_param
          env
          arg_binder
          arg_mut_flag
          (AST.t_pair acc_ty ty, fold_args)
          ~in_:(fun env' -> eval_ligo body calltrace env'))
      init
      elts
  | C_LIST_FOLD_LEFT, _ -> fail @@ error_type ()
  | ( C_FOLD
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_List elts
      ; init
      ] )
  | ( C_LIST_FOLD
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_List elts
      ; init
      ] ) ->
    let lst_ty = nth_type 1 in
    let acc_ty = nth_type 2 in
    let* ty =
      monad_option (Errors.generic_error lst_ty.location "Expected list type")
      @@ AST.get_t_list lst_ty
    in
    Monad.bind_fold_list
      (fun prev elt ->
        let fold_args = v_pair (prev, elt) in
        bind_param
          env
          arg_binder
          arg_mut_flag
          (AST.t_pair acc_ty ty, fold_args)
          ~in_:(fun env' -> eval_ligo body calltrace env'))
      init
      elts
  | ( C_FOLD
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_Set elts
      ; init
      ] )
  | ( C_SET_FOLD
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_Set elts
      ; init
      ] ) ->
    let set_ty = nth_type 1 in
    let acc_ty = nth_type 2 in
    let* ty =
      monad_option (Errors.generic_error set_ty.location "Expected set type")
      @@ AST.get_t_set set_ty
    in
    Monad.bind_fold_list
      (fun prev elt ->
        let fold_args = v_pair (prev, elt) in
        bind_param
          env
          arg_binder
          arg_mut_flag
          (AST.(t_pair acc_ty ty), fold_args)
          ~in_:(fun env' -> eval_ligo body calltrace env'))
      init
      elts
  | C_FOLD, _ -> fail @@ error_type ()
  | C_LIST_FOLD, _ -> fail @@ error_type ()
  | C_SET_FOLD, _ -> fail @@ error_type ()
  | ( C_LIST_FOLD_RIGHT
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_List elts
      ; init
      ] ) ->
    let lst_ty = nth_type 1 in
    let acc_ty = nth_type 2 in
    let* ty =
      monad_option (Errors.generic_error lst_ty.location "Expected list type")
      @@ AST.get_t_list lst_ty
    in
    Monad.bind_fold_right_list
      (fun elt prev ->
        let fold_args = v_pair (elt, prev) in
        bind_param
          env
          arg_binder
          arg_mut_flag
          (AST.t_pair ty acc_ty, fold_args)
          ~in_:(fun env' -> eval_ligo body calltrace env'))
      init
      elts
  | C_LIST_FOLD_RIGHT, _ -> fail @@ error_type ()
  | C_BIG_MAP_EMPTY, [] -> return @@ V_Map []
  | C_BIG_MAP_EMPTY, _ -> fail @@ error_type ()
  | C_MAP_EMPTY, [] -> return @@ V_Map []
  | C_MAP_EMPTY, _ -> fail @@ error_type ()
  | ( C_MAP_FOLD
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_Map kvs
      ; init
      ] ) ->
    let map_ty = nth_type 1 in
    let acc_ty = nth_type 2 in
    let* k_ty, v_ty =
      monad_option (Errors.generic_error map_ty.location "Expected map type")
      @@ AST.get_t_map map_ty
    in
    Monad.bind_fold_list
      (fun prev kv ->
        let fold_args = v_pair (prev, v_pair kv) in
        bind_param
          env
          arg_binder
          arg_mut_flag
          (AST.(t_pair acc_ty (t_pair k_ty v_ty)), fold_args)
          ~in_:(fun env' -> eval_ligo body calltrace env'))
      init
      kvs
  | C_MAP_FOLD, _ -> fail @@ error_type ()
  | C_MAP_ADD, [ k; v; V_Map kvs ] ->
    return (V_Map ((k, v) :: List.Assoc.remove ~equal:LC.equal_value kvs k))
  | C_MAP_ADD, _ -> fail @@ error_type ()
  | C_MAP_REMOVE, [ k; V_Map kvs ] ->
    return @@ V_Map (List.Assoc.remove ~equal:LC.equal_value kvs k)
  | C_MAP_REMOVE, _ -> fail @@ error_type ()
  | C_MAP_UPDATE, [ k; option; V_Map kvs ] ->
    (match LC.get_option option with
     | Some (Some v) ->
       return @@ V_Map ((k, v) :: List.Assoc.remove ~equal:LC.equal_value kvs k)
     | Some None ->
       return @@ V_Map (List.Assoc.remove ~equal:LC.equal_value kvs k)
     | _ -> assert false)
  | C_MAP_UPDATE, _ -> fail @@ error_type ()
  | C_MAP_MEM, [ k; V_Map kvs ] ->
    return @@ v_bool (List.Assoc.mem ~equal:LC.equal_value kvs k)
  | C_MAP_MEM, _ -> fail @@ error_type ()
  | C_BIG_MAP_GET_AND_UPDATE, [ k; option; V_Map kvs ]
  | C_MAP_GET_AND_UPDATE, [ k; option; V_Map kvs ] ->
    let old_value = List.Assoc.find ~equal:LC.equal_value kvs k in
    let old_value =
      match old_value with
      | Some v -> v_some v
      | None -> v_none ()
    in
    (match LC.get_option option with
     | Some (Some v) ->
       return
       @@ v_pair
            ( old_value
            , V_Map ((k, v) :: List.Assoc.remove ~equal:LC.equal_value kvs k) )
     | Some None ->
       return
       @@ v_pair
            (old_value, V_Map (List.Assoc.remove ~equal:LC.equal_value kvs k))
     | None -> assert false)
  | C_BIG_MAP_GET_AND_UPDATE, _ | C_MAP_GET_AND_UPDATE, _ ->
    fail @@ error_type ()
  | C_SET_EMPTY, [] -> return @@ V_Set []
  | C_SET_EMPTY, _ -> fail @@ error_type ()
  | C_SET_ADD, [ v; V_Set l ] ->
    return @@ V_Set (List.dedup_and_sort ~compare:LC.compare_value (v :: l))
  | C_SET_ADD, _ -> fail @@ error_type ()
  | ( C_SET_FOLD_DESC
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_Set elts
      ; init
      ] ) ->
    let set_ty = nth_type 1 in
    let acc_ty = nth_type 2 in
    let* ty =
      monad_option (Errors.generic_error set_ty.location "Expected set type")
      @@ AST.get_t_set set_ty
    in
    Monad.bind_fold_right_list
      (fun prev elt ->
        let fold_args = v_pair (prev, elt) in
        bind_param
          env
          arg_binder
          arg_mut_flag
          (AST.(t_pair acc_ty ty), fold_args)
          ~in_:(fun env' -> eval_ligo body calltrace env'))
      init
      elts
  | C_SET_FOLD_DESC, _ -> fail @@ error_type ()
  | ( C_SET_ITER
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_Set elts
      ] ) ->
    let set_ty = nth_type 1 in
    let* ty =
      monad_option (Errors.generic_error set_ty.location "Expected set type")
      @@ AST.get_t_set set_ty
    in
    Monad.bind_fold_list
      (fun _ elt ->
        bind_param env arg_binder arg_mut_flag (ty, elt) ~in_:(fun env' ->
          eval_ligo body calltrace env'))
      (v_unit ())
      elts
  | C_SET_ITER, _ -> fail @@ error_type ()
  | C_SET_MEM, [ v; V_Set elts ] ->
    return @@ v_bool (List.mem ~equal:LC.equal_value elts v)
  | C_SET_MEM, _ -> fail @@ error_type ()
  | C_SET_REMOVE, [ v; V_Set elts ] ->
    return @@ V_Set (List.filter ~f:(fun el -> not (equal_value el v)) elts)
  | C_SET_REMOVE, _ -> fail @@ error_type ()
  | C_SET_UPDATE, [ v; b; V_Set elts ] ->
    if is_true b
    then
      return
      @@ V_Set (List.dedup_and_sort ~compare:LC.compare_value (v :: elts))
    else
      return @@ V_Set (List.filter ~f:(fun el -> not (equal_value el v)) elts)
  | C_SET_UPDATE, _ -> fail @@ error_type ()
  | ( C_OPTION_MAP
    , [ V_Func_val
          { arg_binder; arg_mut_flag; body; env; rec_name = _; orig_lambda = _ }
      ; V_Construct ("Some", v)
      ] ) ->
    let opt_ty = nth_type 1 in
    let* ty =
      monad_option (Errors.generic_error opt_ty.location "Expected option type")
      @@ AST.get_t_option opt_ty
    in
    let* new_v =
      bind_param env arg_binder arg_mut_flag (ty, v) ~in_:(fun env' ->
        eval_ligo body calltrace env')
    in
    return @@ v_some new_v
  | C_OPTION_MAP, [ V_Func_val _; (V_Construct ("None", V_Ct C_unit) as v) ] ->
    return v
  | C_OPTION_MAP, _ -> fail @@ error_type ()
  | C_LIST_SIZE, [ V_List l ] -> return @@ v_nat (Z.of_int @@ List.length l)
  | C_LIST_SIZE, _ -> fail @@ error_type ()
  | C_SET_SIZE, [ V_Set l ] -> return @@ v_nat (Z.of_int @@ List.length l)
  | C_SET_SIZE, _ -> fail @@ error_type ()
  | C_MAP_SIZE, [ V_Map l ] -> return @@ v_nat (Z.of_int @@ List.length l)
  | C_MAP_SIZE, _ -> fail @@ error_type ()
  | C_SIZE, [ V_Ct (C_string s) ] ->
    return @@ v_nat (Z.of_int @@ String.length s)
  | C_SIZE, [ V_Ct (C_bytes b) ] -> return @@ v_nat (Z.of_int @@ Bytes.length b)
  | C_SIZE, _ -> fail @@ error_type ()
  | C_SLICE, [ V_Ct (C_nat start); V_Ct (C_nat length); V_Ct (C_string s) ] ->
    let start = Z.to_int start in
    let length = Z.to_int length in
    if start >= String.length s || start + length > String.length s
    then
      fail @@ Errors.meta_lang_failwith loc calltrace (V_Ct (C_string "SLICE"))
    else return @@ v_string (String.sub s ~pos:start ~len:length)
  | C_SLICE, [ V_Ct (C_nat start); V_Ct (C_nat length); V_Ct (C_bytes bytes) ]
    ->
    let start = Z.to_int start in
    let length = Z.to_int length in
    if start >= Bytes.length bytes || start + length > Bytes.length bytes
    then
      fail @@ Errors.meta_lang_failwith loc calltrace (V_Ct (C_string "SLICE"))
    else return @@ v_bytes (Bytes.sub bytes ~pos:start ~len:length)
  | C_SLICE, _ -> fail @@ error_type ()
  (*
    >>>>>>>>
      Test operators
    >>>>>>>>
    *)
  | C_TEST_MUTATE_MICHELSON, [ V_Michelson (Untyped_code m | Ty_code { micheline_repr = { code = m ; _ } ; _ }) ] ->
    let ms = Michelson_backend.Mutation.generate ~oracle:() m in
    List.iter ~f:(fun (m, _) ->
        print_endline (Format.asprintf "%a" Tezos_utils.Michelson.pp m)) ms;
    ignore ms ;
    return @@ v_unit ()
  | C_TEST_MUTATE_MICHELSON, _ -> fail @@ error_type ()
  | C_TEST_ADDRESS, [ V_Ct (C_contract { address; entrypoint = _ }) ] ->
    return (V_Ct (C_address address))
  | C_TEST_ADDRESS, _ -> fail @@ error_type ()
  | C_TEST_FAILWITH, [ v ] -> fail @@ Errors.meta_lang_failwith loc calltrace v
  | C_TEST_FAILWITH, _ -> fail @@ error_type ()
  | ( C_TEST_TRY_WITH
    , [ V_Func_val
          { arg_binder = try_binder
          ; arg_mut_flag = try_mut_flag
          ; body = try_body
          ; env = try_env
          ; rec_name = _
          ; orig_lambda = try_lambda
          }
      ; V_Func_val
          { arg_binder = catch_binder
          ; arg_mut_flag = catch_mut_flag
          ; body = catch_body
          ; env = catch_env
          ; rec_name = _
          ; orig_lambda = catch_lambda
          }
      ] ) ->
    let eval_branch arg_binder arg_mut_flag orig_lambda body calltrace env =
      let Arrow.{ type1 = in_ty; type2 = _ } =
        AST.get_t_arrow_exn orig_lambda.type_expression
      in
      bind_param
        env
        arg_binder
        arg_mut_flag
        (in_ty, v_unit ())
        ~in_:(fun f_env' ->
          eval_ligo { body with location = loc } (loc :: calltrace) f_env')
    in
    try_or
      (eval_branch
         try_binder
         try_mut_flag
         try_lambda
         try_body
         calltrace
         try_env)
      (eval_branch
         catch_binder
         catch_mut_flag
         catch_lambda
         catch_body
         calltrace
         catch_env)
  | C_TEST_TRY_WITH, _ -> fail @@ error_type ()
  | ( C_TEST_COMPILE_CONTRACT_FROM_FILE
    , [ V_Ct (C_string contract_file)
      ; V_Ct (C_string entryp)
      ; V_List views
      ; mutation
      ] ) ->
    let@ mod_res = Get_mod_res () in
    let contract_file =
      resolve_contract_file ~mod_res ~source_file ~contract_file
    in
    let views =
      List.map
        ~f:(fun x ->
          trace_option ~raise (Errors.corner_case ()) @@ get_string x)
        views
    in
    let* mutation =
      monad_option (Errors.generic_error loc "Expected option")
      @@ LC.get_nat_option mutation
    in
    let>> code =
      Compile_contract_from_file (contract_file, entryp, views, mutation)
    in
    return @@ code
  | C_TEST_COMPILE_CONTRACT_FROM_FILE, _ -> fail @@ error_type ()
  | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN
    , [ V_Ct (C_address address)
      ; entrypoint
      ; V_Michelson (Ty_code { micheline_repr = { code = param; _ }; _ })
      ; V_Ct (C_mutez amt)
      ] ) ->
    let entrypoint = Option.join @@ LC.get_string_option entrypoint in
    let contract = { address; entrypoint } in
    let>> res = External_call (loc, calltrace, contract, param, amt) in
    return_contract_exec_exn res
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN, _ -> fail @@ error_type ()
  | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS
    , [ V_Ct (C_address address)
      ; entrypoint
      ; V_Michelson (Ty_code { micheline_repr = { code = param; _ }; _ })
      ; V_Ct (C_mutez amt)
      ] ) ->
    let entrypoint = Option.join @@ LC.get_string_option entrypoint in
    let contract = { address; entrypoint } in
    let>> res = External_call (loc, calltrace, contract, param, amt) in
    return_contract_exec res
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS, _ -> fail @@ error_type ()
  | C_TEST_SET_SOURCE, [ addr ] ->
    let>> () = Set_source addr in
    return @@ v_unit ()
  | C_TEST_SET_SOURCE, _ -> fail @@ error_type ()
  | C_TEST_SET_BAKER, [ addr ] ->
    let>> () = Set_baker (loc, calltrace, addr) in
    return @@ v_unit ()
  | C_TEST_SET_BAKER, _ -> fail @@ error_type ()
  | C_TEST_GET_STORAGE_OF_ADDRESS, [ addr ] ->
    let>> storage = Get_storage_of_address (loc, calltrace, addr) in
    return storage
  | C_TEST_GET_STORAGE_OF_ADDRESS, _ -> fail @@ error_type ()
  | C_TEST_GET_BALANCE, [ addr ] ->
    let>> balance = Get_balance (loc, calltrace, addr) in
    return balance
  | C_TEST_GET_BALANCE, _ -> fail @@ error_type ()
  | C_TEST_PRINT, [ V_Ct (C_int i); V_Ct (C_string v) ] ->
    let () =
      match Z.to_int i with
      | 2 -> Format.eprintf "%s" v
      | 1 -> Format.printf "%s" v
      | _ -> ()
    in
    return @@ v_unit ()
  | C_TEST_PRINT, _ -> fail @@ error_type ()
  | C_TEST_TO_STRING, [ v; V_Ct (C_int z) ] ->
    (match Z.to_int z with
     | 1 ->
       let json = Ligo_interpreter.Types.value_to_yojson v in
       return (v_string @@ Yojson.Safe.to_string json)
     | _ ->
       let s = Format.asprintf "%a" Ligo_interpreter.PP.pp_value v in
       return (v_string s))
  | C_TEST_TO_STRING, _ -> fail @@ error_type ()
  | C_TEST_UNESCAPE_STRING, [ V_Ct (C_string s) ] ->
    return (v_string (Scanf.unescaped s))
  | C_TEST_UNESCAPE_STRING, _ -> fail @@ error_type ()
  | C_TEST_BOOTSTRAP_CONTRACT, [ V_Ct (C_mutez z); contract; storage ] ->
    let contract_ty = nth_type 1 in
    let storage_ty = nth_type 2 in
    let>> code = Compile_contract (loc, contract) in
    let>> storage = Eval (loc, storage, storage_ty) in
    let>> () = Bootstrap_contract (Z.to_int z, code, storage, contract_ty) in
    return @@ v_unit ()
  | C_TEST_BOOTSTRAP_CONTRACT, _ -> fail @@ error_type ()
  | C_TEST_NTH_BOOTSTRAP_CONTRACT, [ V_Ct (C_nat n) ] ->
    let n = Z.to_int n in
    let>> address = Nth_bootstrap_contract n in
    return (v_address address)
  | C_TEST_NTH_BOOTSTRAP_CONTRACT, _ -> fail @@ error_type ()
  | C_TEST_STATE_RESET, [ ts_opt; n; amts ] ->
    let ts_opt =
      let v_opt = trace_option ~raise (error_type ()) @@ LC.get_option ts_opt in
      Option.map v_opt ~f:(fun x ->
        trace_option ~raise (error_type ()) @@ LC.get_timestamp x)
    in
    let>> () = Reset_state (loc, ts_opt, calltrace, n, amts) in
    return @@ v_unit ()
  | C_TEST_STATE_RESET, _ -> fail @@ error_type ()
  | C_TEST_GET_NTH_BS, [ n ] ->
    let>> x = Get_bootstrap (loc, calltrace, n) in
    return x
  | C_TEST_GET_NTH_BS, _ -> fail @@ error_type ()
  | C_TEST_LAST_ORIGINATIONS, [ _ ] ->
    let>> x = Get_last_originations () in
    return x
  | C_TEST_LAST_ORIGINATIONS, _ -> fail @@ error_type ()
  | C_TEST_LAST_EVENTS, [ V_Ct (C_string tag) ] ->
    let event_payload_type_opt =
      let open Option in
      let* x = Ast_aggregated.get_t_list expr_ty in
      let* _addr, a = Ast_aggregated.get_t_pair x in
      return a
    in
    (match event_payload_type_opt with
     | Some p_ty ->
       let>> x = Get_last_events (tag, p_ty) in
       return x
     | None -> fail @@ error_type ())
  | C_TEST_LAST_EVENTS, _ -> fail @@ error_type ()
  | ( C_TEST_MUTATE_CONTRACT
    , [ V_Ct (C_nat n); (V_Ast_contract { main; views } as v) ] ) ->
    let* () = check_value v in
    let v = Mutation.mutate_some_contract ~raise n main in
    (match v with
     | None -> return (v_none ())
     | Some (main, m) ->
       return
       @@ v_some
            (V_Record
               (Record.LMap.of_list
                  [ Label "0", V_Ast_contract { main; views }
                  ; Label "1", V_Mutation m
                  ])))
  | C_TEST_MUTATE_CONTRACT, _ -> fail @@ error_type ()
  | C_TEST_MUTATE_VALUE, [ V_Ct (C_nat n); v ] ->
    let* () = check_value v in
    let value_ty = nth_type 1 in
    (match
       Mutation.mutate_some_value
         ~raise
         ?syntax:options.frontend.syntax
         loc
         n
         v
         value_ty
     with
     | None -> return @@ v_none ()
     | Some (e, m) ->
       let* v = eval_ligo e calltrace env in
       return
       @@ v_some
            (V_Record
               (Record.LMap.of_list [ Label "0", v; Label "1", V_Mutation m ])))
  | C_TEST_MUTATE_VALUE, _ -> fail @@ error_type ()
  | ( C_TEST_SAVE_MUTATION
    , [ V_Ct (C_string dir); V_Mutation ((loc, _, _) as mutation) ] ) ->
    let* reg =
      monad_option (Errors.generic_error loc "Not a valid mutation")
      @@ Location.get_file loc
    in
    let file_contents = Fuzz.Ast_aggregated.buffer_of_mutation mutation in
    let id = Fuzz.Ast_aggregated.get_mutation_id mutation in
    let file_path = reg#file in
    (try
       let odir = Sys_unix.getcwd () in
       let () = Sys_unix.chdir dir in
       let file_path = Filename.basename file_path in
       let file_path =
         Caml.Filename.remove_extension file_path
         ^ "."
         ^ id
         ^ Caml.Filename.extension file_path
       in
       let out_chan = Out_channel.create file_path in
       let () = Caml.Buffer.output_buffer out_chan file_contents in
       let () = Sys_unix.chdir odir in
       return (v_some (v_string file_path))
     with
     | Sys_error _ -> return (v_none ()))
  | C_TEST_SAVE_MUTATION, _ -> fail @@ error_type ()
  | C_TEST_TO_CONTRACT, [ addr ] ->
    let contract_ty = nth_type 0 in
    let>> code = To_contract (loc, addr, None, contract_ty) in
    return code
  | C_TEST_TO_CONTRACT, _ -> fail @@ error_type ()
  | C_TEST_TO_ENTRYPOINT, [ V_Ct (C_string ent); addr ] ->
    let contract_ty = nth_type 0 in
    let>> code = To_contract (loc, addr, Some ent, contract_ty) in
    return code
  | C_TEST_TO_ENTRYPOINT, _ -> fail @@ error_type ()
  | C_TEST_TO_TYPED_ADDRESS, [ V_Ct (C_contract { address; _ }) ] ->
    let>> () = Check_storage_address (loc, address, expr_ty) in
    return @@ v_address address
  | C_TEST_TO_TYPED_ADDRESS, _ -> fail @@ error_type ()
  | C_TEST_RUN, [ V_Func_val f; v ] ->
    let* () = check_value (V_Func_val f) in
    let* () = check_value v in
    let>> code = Run (loc, f, v) in
    return code
  | C_TEST_RUN, _ -> fail @@ error_type ()
  | ( C_TEST_DECOMPILE
    , [ V_Michelson (Ty_code { micheline_repr = { code_ty; code }; ast_ty }) ] )
    ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error loc
        @@ Format.asprintf
             "This Michelson value has assigned type '%a', which does not \
              coincide with expected type '%a'."
             AST.PP.type_expression
             ast_ty
             AST.PP.type_expression
             expr_ty)
      @@ AST.Helpers.assert_type_expression_eq
           ~unforged_tickets:true
           (ast_ty, expr_ty)
    in
    let>> v = Decompile (code, code_ty, expr_ty) in
    return v
  | C_TEST_DECOMPILE, _ -> fail @@ error_type ()
  | C_TEST_COMPILE_CONTRACT, [ contract ] ->
    let>> code = Compile_contract (loc, contract) in
    return @@ code
  | C_TEST_COMPILE_CONTRACT, _ -> fail @@ error_type ()
  | C_TEST_COMPILE_AST_CONTRACT, [ contract ] ->
    let>> code = Compile_ast_contract (loc, contract) in
    return @@ code
  | C_TEST_COMPILE_AST_CONTRACT, _ -> fail @@ error_type ()
  | C_TEST_SIZE, [ contract ] ->
    let>> size = Get_size contract in
    return @@ size
  | C_TEST_SIZE, _ -> fail @@ error_type ()
  | C_TEST_ORIGINATE, [ contract; storage; V_Ct (C_mutez amt) ] ->
    let>> addr = Inject_script (loc, calltrace, contract, storage, amt) in
    return @@ addr
  | C_TEST_ORIGINATE, _ -> fail @@ error_type ()
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS, [ V_Ct (C_nat n) ] ->
    let n = Z.to_int n in
    let* parameter_ty', storage_ty' =
      monad_option (Errors.generic_error loc "Expected typed address")
      @@ AST.get_t_typed_address expr_ty
    in
    let>> address, parameter_ty, storage_ty =
      Nth_bootstrap_typed_address (loc, n)
    in
    let* () =
      monad_option
        (Errors.generic_error
           loc
           "Parameter in bootstrap contract does not match")
      @@ AST.Helpers.assert_type_expression_eq (parameter_ty, parameter_ty')
    in
    let* () =
      monad_option
        (Errors.generic_error
           loc
           "Storage in bootstrap contract does not match")
      @@ AST.Helpers.assert_type_expression_eq (storage_ty, storage_ty')
    in
    return (v_typed_address address)
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS, _ -> fail @@ error_type ()
  | C_TEST_RANDOM, [ V_Ct (C_bool small) ] ->
    let* gen_type =
      monad_option (Errors.generic_error loc "Expected typed address")
      @@ AST.get_t_gen expr_ty
    in
    let>> (ctxt : Tezos_state.context) = Get_state () in
    let known_addresses =
      ctxt.internals.bootstrapped
      @ List.concat (List.map ~f:snd ctxt.transduced.last_originations)
    in
    let generator =
      Mutation.value_gen ~raise ~small ~known_addresses gen_type
    in
    return (V_Gen { generator; gen_type })
  | C_TEST_RANDOM, _ -> fail @@ error_type ()
  | C_TEST_GENERATOR_EVAL, [ V_Gen { generator; gen_type = _ } ] ->
    let v = QCheck.Gen.generate1 generator in
    return v
  | C_TEST_GENERATOR_EVAL, _ -> fail @@ error_type ()
  | C_TEST_SET_BIG_MAP, [ V_Ct (C_int n); V_Map kv ] ->
    let bigmap_ty = nth_type 1 in
    let>> () = Set_big_map (n, kv, bigmap_ty) in
    return @@ v_unit ()
  | C_TEST_SET_BIG_MAP, _ -> fail @@ error_type ()
  | C_TEST_CAST_ADDRESS, [ V_Ct (C_address x) ] ->
    let _, ty =
      trace_option
        ~raise
        (Errors.generic_error expr_ty.location "Expected typed_address type")
      @@ Ast_aggregated.get_t_typed_address expr_ty
    in
    let>> () = Add_cast (loc, x, ty) in
    return @@ v_typed_address x
  | C_TEST_CAST_ADDRESS, _ -> fail @@ error_type ()
  | C_TEST_ADD_ACCOUNT, [ V_Ct (C_string sk); V_Ct (C_key pk) ] ->
    let>> () = Add_account (loc, calltrace, sk, pk) in
    return @@ v_unit ()
  | C_TEST_ADD_ACCOUNT, _ -> fail @@ error_type ()
  | C_TEST_NEW_ACCOUNT, [ V_Ct C_unit ] ->
    let>> v = New_account () in
    return @@ v
  | C_TEST_NEW_ACCOUNT, _ -> fail @@ error_type ()
  | C_TEST_BAKER_ACCOUNT, [ account; amount ] ->
    let>> () = Baker_account (account, amount) in
    return @@ v_unit ()
  | C_TEST_BAKER_ACCOUNT, _ -> fail @@ error_type ()
  | C_TEST_REGISTER_DELEGATE, [ V_Ct (C_key_hash pkh) ] ->
    let>> v = Register_delegate (loc, calltrace, pkh) in
    return @@ v
  | C_TEST_REGISTER_DELEGATE, _ -> fail @@ error_type ()
  | C_TEST_BAKE_UNTIL_N_CYCLE_END, [ V_Ct (C_nat n) ] ->
    let>> v = Bake_until_n_cycle_end (loc, calltrace, n) in
    return @@ v
  | C_TEST_BAKE_UNTIL_N_CYCLE_END, _ -> fail @@ error_type ()
  | C_TEST_CREATE_CHEST, [ V_Ct (C_bytes payload); V_Ct (C_nat time) ] ->
    let chest, chest_key =
      Michelson_backend.create_chest payload (Z.to_int time)
    in
    return @@ v_pair (V_Ct (C_bytes chest), V_Ct (C_bytes chest_key))
  | C_TEST_CREATE_CHEST, _ -> fail @@ error_type ()
  | C_TEST_CREATE_CHEST_KEY, [ V_Ct (C_bytes chest); V_Ct (C_nat time) ] ->
    let chest_key = Michelson_backend.create_chest_key chest (Z.to_int time) in
    return @@ v_bytes chest_key
  | C_TEST_CREATE_CHEST_KEY, _ -> fail @@ error_type ()
  | C_TEST_GET_VOTING_POWER, [ V_Ct (C_key_hash hk) ] ->
    let>> vp = Get_voting_power (loc, calltrace, hk) in
    return vp
  | C_TEST_GET_VOTING_POWER, _ -> fail @@ error_type ()
  | C_TEST_GET_TOTAL_VOTING_POWER, [ V_Ct C_unit ] ->
    let>> tvp = Get_total_voting_power (loc, calltrace) in
    return tvp
  | C_TEST_GET_TOTAL_VOTING_POWER, _ -> fail @@ error_type ()
  | ( C_TEST_REGISTER_CONSTANT
    , [ V_Michelson
          (Ty_code { micheline_repr = { code; _ }; _ } | Untyped_code code)
      ] ) ->
    let>> s = Register_constant (loc, calltrace, code) in
    return @@ v_string s
  | C_TEST_REGISTER_CONSTANT, _ -> fail @@ error_type ()
  | C_TEST_CONSTANT_TO_MICHELSON, [ V_Ct (C_string m) ] ->
    let>> s = Constant_to_Michelson (loc, calltrace, m) in
    return @@ V_Michelson (Untyped_code s)
  | C_TEST_CONSTANT_TO_MICHELSON, _ -> fail @@ error_type ()
  | C_TEST_REGISTER_FILE_CONSTANTS, [ V_Ct (C_string path) ] ->
    let>> v = Register_file_constants (loc, calltrace, path) in
    return @@ v
  | C_TEST_REGISTER_FILE_CONSTANTS, _ -> fail @@ error_type ()
  | C_TEST_PUSH_CONTEXT, [ V_Ct C_unit ] ->
    let>> () = Push_context () in
    return @@ v_unit ()
  | C_TEST_PUSH_CONTEXT, _ -> fail @@ error_type ()
  | C_TEST_POP_CONTEXT, [ V_Ct C_unit ] ->
    let>> () = Pop_context () in
    return @@ v_unit ()
  | C_TEST_POP_CONTEXT, _ -> fail @@ error_type ()
  | C_TEST_DROP_CONTEXT, [ V_Ct C_unit ] ->
    let>> () = Drop_context () in
    return @@ v_unit ()
  | C_TEST_DROP_CONTEXT, _ -> fail @@ error_type ()
  | C_TEST_READ_CONTRACT_FROM_FILE, [ V_Ct (C_string contract_file) ] ->
    let@ mod_res = Get_mod_res () in
    let contract_file =
      resolve_contract_file ~mod_res ~source_file ~contract_file
    in
    let>> contract = Read_contract_from_file (loc, calltrace, contract_file) in
    return @@ contract
  | C_TEST_READ_CONTRACT_FROM_FILE, _ -> fail @@ error_type ()
  | C_TEST_SIGN, [ V_Ct (C_string sk); V_Ct (C_bytes d) ] ->
    let>> signature = Sign (loc, calltrace, sk, d) in
    return @@ signature
  | C_TEST_SIGN, _ -> fail @@ error_type ()
  | C_TEST_GET_ENTRYPOINT, [ V_Ct (C_contract { address = _; entrypoint }) ] ->
    (match entrypoint with
     | None -> return @@ v_none ()
     | Some s -> return @@ v_some (v_string s))
  | C_TEST_GET_ENTRYPOINT, _ -> fail @@ error_type ()
  | C_TEST_INT64_OF_INT, [ V_Ct (C_int n) ] ->
    return @@ V_Ct (C_int64 (Z.to_int64 n))
  | C_TEST_INT64_OF_INT, _ -> fail @@ error_type ()
  | C_TEST_INT64_TO_INT, [ V_Ct (C_int64 n) ] ->
    return @@ V_Ct (C_int (Z.of_int64 n))
  | C_TEST_INT64_TO_INT, _ -> fail @@ error_type ()
  | C_CHECK_ENTRYPOINT, _ -> return @@ v_unit ()
  | (C_CHECK_SELF | C_CHECK_EMIT_EVENT), _ ->
    fail
    @@ Errors.generic_error loc "Check should not be present in testing mode."
  | C_TEST_SET_PRINT_VALUES, [ V_Ct (C_bool b) ] ->
    let@ b = Set_print_values b in
    return @@ v_bool b
  | C_TEST_SET_PRINT_VALUES, _ -> fail @@ error_type ()
  | C_POLYMORPHIC_ADD, _ ->
    fail @@ Errors.generic_error loc "POLYMORPHIC_ADD is solved in checking."
  | C_POLYMORPHIC_SUB, _ ->
    fail @@ Errors.generic_error loc "POLYMORPHIC_SUB is solved in checking."
  | ( ( C_UPDATE
      | C_ITER
      | C_FOLD_LEFT
      | C_FOLD_RIGHT
      | C_PAIR
      | C_CAR
      | C_CDR
      | C_LEFT
      | C_RIGHT
      | C_SET_LITERAL
      | C_LIST_LITERAL
      | C_MAP
      | C_MAP_LITERAL
      | C_MAP_GET
      | C_MAP_GET_FORCE
      | C_BIG_MAP
      | C_BIG_MAP_LITERAL
      | C_CREATE_CONTRACT
      | C_GLOBAL_CONSTANT )
    , _ ) -> fail @@ Errors.generic_error loc "Unbound primitive."


(*interpreter*)
and eval_literal : Ligo_prim.Literal_value.t -> value Monad.t = function
  | Literal_unit -> Monad.return @@ v_unit ()
  | Literal_int i -> Monad.return @@ v_int i
  | Literal_nat n -> Monad.return @@ v_nat n
  | Literal_timestamp i -> Monad.return @@ v_timestamp i
  | Literal_string s -> Monad.return @@ v_string (Ligo_string.extract s)
  | Literal_bytes s -> Monad.return @@ v_bytes s
  | Literal_mutez s -> Monad.return @@ v_mutez s
  | Literal_key_hash s ->
    (match Tezos_crypto.Signature.Public_key_hash.of_b58check s with
     | Ok kh -> Monad.return @@ v_key_hash kh
     | Error _ ->
       Monad.fail @@ Errors.literal Location.generated (Literal_key_hash s))
  | Literal_key s ->
    (match Tezos_crypto.Signature.Public_key.of_b58check s with
     | Ok k -> Monad.return @@ v_key k
     | Error _ ->
       Monad.fail @@ Errors.literal Location.generated (Literal_key s))
  | Literal_signature s ->
    (match Tezos_crypto.Signature.of_b58check s with
     | Ok s -> Monad.return @@ v_signature s
     | Error _ ->
       Monad.fail @@ Errors.literal Location.generated (Literal_signature s))
  | Literal_address s ->
    (match Tezos_protocol.Protocol.Alpha_context.Contract.of_b58check s with
     | Ok t -> Monad.return @@ v_address t
     | Error _ ->
       Monad.fail @@ Errors.literal Location.generated (Literal_address s))
  | Literal_bls12_381_g1 b ->
    (match Bls12_381.G1.of_bytes_opt b with
     | Some t -> Monad.return @@ v_bls12_381_g1 t
     | None ->
       Monad.fail @@ Errors.literal Location.generated (Literal_bls12_381_g1 b))
  | Literal_bls12_381_g2 b ->
    (match Bls12_381.G2.of_bytes_opt b with
     | Some t -> Monad.return @@ v_bls12_381_g2 t
     | None ->
       Monad.fail @@ Errors.literal Location.generated (Literal_bls12_381_g2 b))
  | Literal_bls12_381_fr b ->
    (match Bls12_381.Fr.of_bytes_opt b with
     | Some t -> Monad.return @@ v_bls12_381_fr t
     | None ->
       Monad.fail @@ Errors.literal Location.generated (Literal_bls12_381_fr b))
  | l -> Monad.fail @@ Errors.literal Location.generated l


and eval_ligo ~raise ~steps ~options
  : AST.expression -> calltrace -> env -> value Monad.t
  =
 fun term calltrace env ->
  let eval_ligo ?(steps = steps - 1) v =
    let v = replace_loc_if_blank v term.location in
    eval_ligo ~raise ~steps ~options v
  in
  let open Monad in
  let* () =
    if steps <= 0
    then
      fail
        (Errors.meta_lang_eval term.location calltrace (v_string "Out of fuel"))
    else return ()
  in
  match term.expression_content with
  | E_application { lamb = f; args } ->
    let* f' = eval_ligo f calltrace env in
    let* args' = eval_ligo args calltrace env in
    (match f' with
     | V_Func_val
         { arg_binder; arg_mut_flag; body; env; rec_name = None; orig_lambda }
       ->
       let Arrow.{ type1 = in_ty; type2 = _ } =
         AST.get_t_arrow_exn orig_lambda.type_expression
       in
       bind_param env arg_binder arg_mut_flag (in_ty, args') ~in_:(fun f_env' ->
         eval_ligo
           { body with location = term.location }
           (term.location :: calltrace)
           f_env')
     | V_Func_val
         { arg_binder
         ; arg_mut_flag
         ; body
         ; env
         ; rec_name = Some fun_name
         ; orig_lambda
         } ->
       let Arrow.{ type1 = in_ty; type2 = _ } =
         AST.get_t_arrow_exn orig_lambda.type_expression
       in
       let f_env' = Env.extend env fun_name (orig_lambda.type_expression, f') in
       bind_param
         f_env'
         arg_binder
         arg_mut_flag
         (in_ty, args')
         ~in_:(fun f_env'' ->
         eval_ligo
           { body with location = term.location }
           (term.location :: calltrace)
           f_env'')
     | V_Michelson
         (Ty_code { micheline_repr = { code; code_ty = _ }; ast_ty = _ }) ->
       let () =
         match code with
         | Seq (_, [ Prim (_, "FAILWITH", _, _) ]) ->
           raise.warning (`Use_meta_ligo term.location)
         | _ -> ()
       in
       let>> v =
         Run_Michelson
           ( term.location
           , calltrace
           , code
           , term.type_expression
           , args'
           , args.type_expression )
       in
       return v
     | _ ->
       fail
       @@ Errors.generic_error
            term.location
            "Trying to apply on something that is not a function?")
  | E_lambda { binder; output_type = _; result } ->
    let fv = Self_ast_aggregated.Helpers.Free_variables.expression term in
    let env =
      List.filter ~f:(fun (v, _) -> List.mem fv v ~equal:Value_var.equal) env
    in
    return
    @@ V_Func_val
         { rec_name = None
         ; orig_lambda = term
         ; arg_binder = Param.get_var binder
         ; arg_mut_flag = Param.get_mut_flag binder
         ; body = result
         ; env
         }
  | E_let_in
      { let_binder
      ; rhs
      ; let_result
      ; attr =
          { no_mutation; inline; view = _; public = _; hidden = _; thunk = _ }
      } ->
    let* rhs' = eval_ligo rhs calltrace env in
    eval_ligo
      let_result
      calltrace
      (Env.extend
         env
         (Binder.get_var let_binder)
         ~inline
         ~no_mutation
         (rhs.type_expression, rhs'))
  | E_literal l -> eval_literal l
  | E_variable var ->
    let fst (a, _, _) = a in
    let { eval_term = v; _ } =
      try fst (Option.value_exn (Env.lookup env var)) with
      | _ -> failwith (Format.asprintf "unbound variable: %a" Value_var.pp var)
    in
    return v
  | E_record recmap ->
    let* lv' =
      Monad.bind_map_list
        (fun (label, (v : AST.expression)) ->
          let* v' = eval_ligo v calltrace env in
          return (label, v'))
        (Record.LMap.to_kv_list_rev recmap)
    in
    return @@ V_Record (Record.of_list lv')
  | E_accessor { struct_; path } ->
    let* record' = eval_ligo struct_ calltrace env in
    (match record' with
     | V_Record recmap ->
       let a = Record.LMap.find path recmap in
       return a
     | _ -> failwith "trying to access a non-record")
  | E_update { struct_; path; update } ->
    let* record' = eval_ligo struct_ calltrace env in
    (match record' with
     | V_Record recmap ->
       if Record.LMap.mem path recmap
       then
         let* field' = eval_ligo update calltrace env in
         return @@ V_Record (Record.LMap.add path field' recmap)
       else failwith "field l does not exist in record"
     | _ -> failwith "this expression isn't a record")
  | E_constant { cons_name; arguments } ->
    let* arguments' =
      Monad.bind_map_list
        (fun (ae : AST.expression) ->
          let* value = eval_ligo ae calltrace env in
          return @@ (value, ae.type_expression, ae.location))
        arguments
    in
    apply_operator
      ~raise
      ~steps
      ~options
      term.location
      calltrace
      term.type_expression
      env
      cons_name
      arguments'
  | E_constructor
      { constructor = Label "True"
      ; element = { expression_content = E_literal Literal_unit; _ }
      } -> return @@ V_Ct (C_bool true)
  | E_constructor
      { constructor = Label "False"
      ; element = { expression_content = E_literal Literal_unit; _ }
      } -> return @@ V_Ct (C_bool false)
  | E_constructor { constructor = Label "Some"; element } ->
    let* v = eval_ligo element (term.location :: calltrace) env in
    return @@ v_some v
  | E_constructor
      { constructor = Label "None"
      ; element = { expression_content = E_literal Literal_unit; _ }
      } -> return @@ v_none ()
  | E_constructor { constructor = Label c; element } ->
    let* v' = eval_ligo element calltrace env in
    return @@ V_Construct (c, v')
  | E_matching { matchee; cases } ->
    let* e' = eval_ligo matchee calltrace env in
    (match cases, e' with
     | Match_variant { cases; _ }, V_List [] ->
       let { constructor = _; pattern = _; body } =
         List.find_exn
           ~f:(fun { constructor = Label c; pattern = _; body = _ } ->
             String.equal "Nil" c)
           cases
       in
       eval_ligo body calltrace env
     | Match_variant { cases; tv }, V_List lst ->
       let { constructor = _; pattern; body } =
         List.find_exn
           ~f:(fun { constructor = Label c; pattern = _; body = _ } ->
             String.equal "Cons" c)
           cases
       in
       let ty = AST.get_t_list_exn tv in
       let hd = List.hd_exn lst in
       let tl = V_List (List.tl_exn lst) in
       let proj = v_pair (hd, tl) in
       let env' = Env.extend env pattern (ty, proj) in
       eval_ligo body calltrace env'
     | Match_variant { cases; _ }, V_Ct (C_bool b) ->
       let ctor_body (case : _ matching_content_case) =
         case.constructor, case.body
       in
       let cases = Record.of_list (List.map ~f:ctor_body cases) in
       let get_case c = Record.LMap.find (Label c) cases in
       let match_true = get_case "True" in
       let match_false = get_case "False" in
       if b
       then eval_ligo match_true calltrace env
       else eval_ligo match_false calltrace env
     | Match_variant { cases; tv }, V_Construct (matched_c, proj) ->
       let* tv =
         match AST.get_t_sum_opt tv with
         | Some tv ->
           let ({ associated_type; michelson_annotation = _; decl_pos = _ }
                 : row_element)
             =
             Record.LMap.find (Label matched_c) tv.fields
           in
           return associated_type
         | None ->
           (match AST.get_t_option tv with
            | Some tv -> return tv
            | None -> fail @@ Errors.generic_error tv.location "Expected sum")
       in
       let { constructor = _; pattern; body } =
         List.find_exn
           ~f:(fun { constructor = Label c; pattern = _; body = _ } ->
             String.equal matched_c c)
           cases
       in
       (* TODO-er: check *)
       let env' = Env.extend env pattern (tv, proj) in
       eval_ligo body calltrace env'
     | Match_record { fields; body; tv = _ }, V_Record rv ->
       let aux : Label.t -> _ Binder.t -> env -> env =
        fun l b env ->
         let iv =
           match Record.LMap.find_opt l rv with
           | Some x -> x
           | None -> failwith "label do not match"
         in
         Env.extend env (Binder.get_var b) (Binder.get_ascr b, iv)
       in
       let env' = Record.LMap.fold aux fields env in
       eval_ligo body calltrace env'
     | _, v ->
       failwith
         ("not yet supported case "
         ^ Format.asprintf "%a" Ligo_interpreter.PP.pp_value v
         ^ Format.asprintf "%a" AST.PP.expression term))
  | E_recursive { fun_name; fun_type = _; lambda } ->
    let fv = Self_ast_aggregated.Helpers.Free_variables.expression term in
    let env =
      List.filter ~f:(fun (v, _) -> List.mem fv v ~equal:Value_var.equal) env
    in
    return
    @@ V_Func_val
         { rec_name = Some fun_name
         ; orig_lambda = term
         ; arg_binder = Param.get_var lambda.binder
         ; arg_mut_flag = Param.get_mut_flag lambda.binder
         ; body = lambda.result
         ; env
         }
  | E_raw_code { language; code = { expression_content = m; _ } }
    when Option.is_some (AST.get_e_tuple m) ->
    let open AST in
    let tuple = Option.value ~default:[] (AST.get_e_tuple m) in
    let code, args =
      match tuple with
      | [] ->
        raise.error
          (Errors.generic_error
             term.location
             "expected non-empty tuple in %Michelson")
      | hd :: tl -> hd, tl
    in
    let rec bind_list = function
      | [] -> return []
      | hd :: tl ->
        let* hd = hd in
        let* tl = bind_list tl in
        return @@ (hd :: tl)
    in
    let bind_map_list ~f lst = bind_list (List.map ~f lst) in
    let* args =
      bind_map_list
        ~f:(fun e ->
          let* v = eval_ligo e calltrace env in
          return
            ( v
            , Stacking.To_micheline.translate_type
                (Scoping.translate_type
                   (trace ~raise Main_errors.spilling_tracer
                   @@ Spilling.compile_type e.type_expression)) ))
        args
    in
    (match code.expression_content with
     | E_literal (Literal_string x)
       when String.equal language Backend.Michelson.name
            && (is_t_arrow (get_type code) || is_t_arrow term.type_expression)
       ->
       let ast_ty = get_type code in
       let exp_as_string = Ligo_string.extract x in
       let code, code_ty =
         Michelson_backend.parse_raw_michelson_code ~raise exp_as_string ast_ty
       in
       let replace m =
         let open Tezos_micheline.Micheline in
         match m with
         | Prim (_, s, [], [ id ])
           when String.equal "type" s && String.is_prefix ~prefix:"$" id ->
           let id = String.chop_prefix_exn ~prefix:"$" id in
           let id = Int.of_string id in
           (match List.nth args id with
            | None ->
              raise.error
                (Errors.generic_error
                   term.location
                   (Format.sprintf "could not resolve (type %d)" id))
            | Some (_, t) ->
              Tezos_micheline.Micheline.map_node (fun _ -> ()) (fun s -> s) t)
         | Prim (_, s, [], [ id ])
           when String.equal "litstr" s && String.is_prefix ~prefix:"$" id ->
           let id = String.chop_prefix_exn ~prefix:"$" id in
           let id = Int.of_string id in
           (match List.nth args id with
            | Some (V_Ct (C_string s), _) ->
              Tezos_micheline.Micheline.String ((), s)
            | _ ->
              raise.error
                (Errors.generic_error
                   term.location
                   (Format.sprintf "could not resolve (litstr %d)" id)))
         | Prim (a, b, c, d) ->
           let open Tezos_micheline.Micheline in
           let f arg (c, d) =
             match arg with
             | Prim (_, s, [], [ id ])
               when String.equal "annot" s && String.is_prefix ~prefix:"$" id ->
               let id = String.chop_prefix_exn ~prefix:"$" id in
               let id = Int.of_string id in
               let annot =
                 match List.nth args id with
                 | Some (V_Ct (C_string s), _) -> s
                 | _ ->
                   raise.error
                     (Errors.generic_error
                        term.location
                        (Format.sprintf "could not resolve (annot %d)" id))
               in
               c, annot :: d
             | m -> m :: c, d
           in
           let c, d = List.fold_right ~f ~init:([], d) c in
           Prim (a, b, c, d)
         | m -> m
       in
       let code = Tezos_utils.Michelson.map replace code in
       return
       @@ V_Michelson (Ty_code { micheline_repr = { code; code_ty }; ast_ty })
     | _ ->
       raise.error
       @@ Errors.generic_error
            term.location
            "Embedded raw code can only have a functional type")
  | E_raw_code { language; code } ->
    let open AST in
    (match code.expression_content with
     | E_literal (Literal_string x)
       when String.equal language Backend.Michelson.name
            && (is_t_arrow (get_type code) || is_t_arrow term.type_expression)
       ->
       let ast_ty = get_type code in
       let exp_as_string = Ligo_string.extract x in
       let code, code_ty =
         Michelson_backend.parse_raw_michelson_code ~raise exp_as_string ast_ty
       in
       return
       @@ V_Michelson (Ty_code { micheline_repr = { code; code_ty }; ast_ty })
     | _ ->
       raise.error
       @@ Errors.generic_error
            term.location
            "Embedded raw code can only have a functional type")
  | E_assign { binder; expression } ->
    let loc =
      match Env.lookup env (Binder.get_var binder) with
      | Some ({ eval_term = V_Location loc; _ }, _, _) -> loc
      | _ ->
        failwith
          (Format.asprintf
             "@[<hv>%a@.unbound variable mutable: %a@]"
             Snippet.pp
             term.location
             Value_var.pp
             (Binder.get_var binder))
    in
    let* val_ = eval_ligo expression calltrace env in
    let@ () = Set (loc, val_) in
    return @@ v_unit ()
  | E_deref mut_var ->
    let loc =
      match Env.lookup env mut_var with
      | Some ({ eval_term = V_Location loc; _ }, _, _) -> loc
      | _ ->
        failwith
          (Format.asprintf
             "@[<hv>%a@.unbound variable mutable: %a@]"
             Snippet.pp
             term.location
             Value_var.pp
             mut_var)
    in
    let@ val_ = Deref loc in
    return val_
  | E_let_mut_in { let_binder; rhs; let_result; attr = _ } ->
    let* rhs' = eval_ligo rhs calltrace env in
    let@ loc = Alloc rhs' in
    let* let_result =
      eval_ligo
        let_result
        calltrace
        (Env.extend
           env
           (Binder.get_var let_binder)
           (rhs.type_expression, V_Location loc))
    in
    let@ () = Free loc in
    return let_result
  | E_while { cond; body } ->
    let rec loop () =
      let* cond = eval_ligo cond calltrace env in
      match cond with
      | V_Ct (C_bool true) ->
        let* _ = eval_ligo body calltrace env in
        loop ()
      | V_Ct (C_bool false) -> return @@ v_unit ()
      | _ ->
        failwith (Format.asprintf "Non-boolean value for while-loop condition")
    in
    loop ()
  | E_for_each { fe_binder = binder1, Some binder2; collection; fe_body; _ } ->
    let* k_ty, v_ty =
      monad_option
        (Errors.generic_error collection.location "Expected map type")
      @@ AST.get_t_map collection.type_expression
    in
    let* collection = eval_ligo collection calltrace env in
    (match collection with
     | V_Map elts ->
       Monad.bind_fold_list
         (fun _ (k_val, v_val) ->
           let env = Env.extend env binder1 (k_ty, k_val) in
           let env = Env.extend env binder2 (v_ty, v_val) in
           eval_ligo fe_body calltrace env)
         (v_unit ())
         elts
     | _ -> failwith (Format.asprintf "Expected map value for for-each loop"))
  | E_for_each { fe_binder = binder1, None; collection; fe_body; _ } ->
    let type_ = collection.type_expression in
    let* v_ty =
      monad_option
        (Errors.generic_error collection.location "Expected list or set type")
        (* [assert false] bcs this case can *never* occur *)
        (Option.merge
           (AST.get_t_set type_)
           (AST.get_t_list type_)
           ~f:(fun _ _ -> assert false))
    in
    let* collection = eval_ligo collection calltrace env in
    (match collection with
     | V_Set elts | V_List elts ->
       Monad.bind_fold_list
         (fun _ v_val ->
           let env = Env.extend env binder1 (v_ty, v_val) in
           eval_ligo fe_body calltrace env)
         (v_unit ())
         elts
     | _ ->
       failwith (Format.asprintf "Expected list or set value for for-each loop"))
  | E_for { binder; start; final; incr; f_body } ->
    let* start = eval_ligo start calltrace env in
    let* incr = eval_ligo incr calltrace env in
    let* final = eval_ligo final calltrace env in
    (match start, incr, final with
     | V_Ct (C_int start), V_Ct (C_int incr), V_Ct (C_int final) ->
       let rec loop curr =
         if Z.geq curr final
         then return @@ v_unit ()
         else (
           let env = Env.extend env binder (AST.t_int (), V_Ct (C_int curr)) in
           let* _ = eval_ligo f_body calltrace env in
           loop (Z.add curr incr))
       in
       loop start
     | _ -> failwith (Format.asprintf "Expected int types for for loop"))
  | E_type_abstraction { type_binder = _; result } ->
    eval_ligo result calltrace env
  | E_type_inst _ ->
    fail
    @@ Errors.generic_error
         term.location
         "Polymorphism not supported: polymorphic expressions should be \
          monomorphized before being interpreted. This could mean that the \
          expression that you are trying to interpret is too generic, try \
          adding a type annotation."


and try_eval ~raise ~steps ~options expr env state r =
  Monad.eval
    ~raise
    ~options
    (eval_ligo ~raise ~steps ~options expr [] env)
    state
    r


let eval_expression ~raise ~steps ~options
  : Ast_typed.program -> Ast_typed.expression -> bool * value
  =
 fun prg expr ->
  (* Compile new context *)
  let initial_state = Execution_monad.make_state ~raise ~options in
  let expr =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      prg
      expr
  in
  let expr =
    trace ~raise Main_errors.self_ast_aggregated_tracer
    @@ Self_ast_aggregated.all_expression ~options:options.middle_end expr
  in
  let value, st =
    try_eval ~raise ~steps ~options expr Env.empty_env initial_state None
  in
  st.print_values, value


let eval_test ~raise ~steps ~options
  : Ast_typed.program -> bool * (string * value) list
  =
 fun prg ->
  let decl_lst = prg in
  (* Pass over declarations, for each "test"-prefixed one, add a new
     declaration and in the end, gather all of them together *)
  let aux decl r =
    let ds, defs = r in
    match decl.Location.wrap_content with
    | Ast_typed.D_value { binder; expr; _ } ->
      let var = Binder.get_var binder in
      if (not (Value_var.is_generated var))
         && Base.String.is_prefix (Value_var.to_name_exn var) ~prefix:"test"
      then (
        let expr = Ast_typed.(e_a_variable var expr.type_expression) in
        (* TODO: check that variables are unique, as they are ignored *)
        decl :: ds, (binder, expr.type_expression) :: defs)
      else decl :: ds, defs
    | _ -> decl :: ds, defs
  in
  let decl_lst, lst = List.fold_right ~f:aux ~init:([], []) decl_lst in
  (* Compile new context *)
  let f (n, t) r =
    let s, _ = Value_var.internal_get_name_and_counter @@ Binder.get_var n in
    Record.LMap.add (Label s) (Ast_typed.e_a_variable (Binder.get_var n) t) r
  in
  let map = List.fold_right lst ~f ~init:Record.LMap.empty in
  let expr = Ast_typed.e_a_record map in
  match eval_expression ~raise ~steps ~options decl_lst expr with
  | b, V_Record m ->
    let f (n, _) r =
      let s, _ = Value_var.internal_get_name_and_counter @@ Binder.get_var n in
      match Record.LMap.find_opt (Label s) m with
      | None -> failwith "Cannot find"
      | Some v -> (s, v) :: r
    in
    b, List.fold_right ~f ~init:[] @@ lst
  | _ -> failwith "Not a tuple?"


let () = Printexc.record_backtrace true
