open Simple_utils.Trace
open Simple_utils
open Ligo_interpreter.Types
open Ligo_interpreter.Combinators

module AST = Ast_aggregated

include AST.Types
module Env = Ligo_interpreter.Environment
module Monad = Execution_monad

type interpreter_error = Errors.interpreter_error

let check_value value =
  let open Monad in
  match value with
  | V_Func_val {orig_lambda;rec_name=_;arg_binder=_;body=_;env=_} ->
     call @@ Check_obj_ligo orig_lambda
  | _ -> return ()

let monad_option error = fun v ->
    let open Monad in
    match v with
      None -> fail error
    | Some s -> return s

let wrap_compare_result comp cmpres loc calltrace =
  let open Monad in
  match comp with
  | C_EQ -> return (cmpres = 0)
  | C_NEQ -> return (cmpres <> 0)
  | C_LT -> return (cmpres < 0)
  | C_LE -> return (cmpres <= 0)
  | C_GT -> return (cmpres > 0)
  | C_GE -> return (cmpres >= 0)
  | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"

let compare_constants c o1 o2 loc calltrace =
  let open Monad in
  match (c, [o1; o2]) with
  | (comp, [V_Ct (C_int a'); V_Ct (C_int b')])
  | (comp, [V_Ct (C_mutez a'); V_Ct (C_mutez b')])
  | (comp, [V_Ct (C_timestamp a'); V_Ct (C_timestamp b')])
  | (comp, [V_Ct (C_nat a'); V_Ct (C_nat b')]) ->
      let cmpres = Z.compare a' b' in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      return @@ v_bool x
  | (comp, [V_Ct (C_bool b); V_Ct (C_bool a)]) ->
      let cmpres = Bool.compare b a in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      return @@ v_bool x
  | (comp, [V_Ct (C_address b); V_Ct (C_address a)]) ->
      let cmpres = Tezos_state.compare_account b a in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      return @@ v_bool x
  | (comp, [V_Ct (C_key_hash b); V_Ct (C_key_hash a)]) ->
      let cmpres = Tezos_crypto.Signature.Public_key_hash.compare b a in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      return @@ v_bool x
  | (comp, [V_Ct C_unit; V_Ct C_unit]) ->
      let* x =
        match comp with
        | C_EQ -> return true
        | C_NEQ -> return false
        | C_LT -> return false
        | C_LE -> return true
        | C_GT -> return false
        | C_GE -> return true
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
  | (comp, [V_Ct (C_string a'); V_Ct (C_string b')]) ->
      let* f_cmp = return @@ fun a b -> String.compare a b in
      let* cmpres = return @@ f_cmp a' b' in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      Monad.return @@ v_bool x
  | (comp, [V_Ct (C_bytes a'); V_Ct (C_bytes b')]) ->
      let* f_cmp = return @@ fun a b -> Bytes.compare a b in
      let* cmpres = return @@ f_cmp a' b' in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      Monad.return @@ v_bool x
  | ( comp,
      [
        V_Ct (C_contract {address = addr1; entrypoint = entr1});
        V_Ct (C_contract {address = addr2; entrypoint = entr2});
      ] ) ->
      let compare_opt_strings o1 o2 =
        match (o1, o2) with (Some s1, Some s2) -> String.equal s1 s2 | _ -> false
      in
      let cmpres = Tezos_state.compare_account addr1 addr2 in
      let* x =
        match comp with
        | C_EQ -> return (cmpres = 0 && compare_opt_strings entr1 entr2)
        | C_NEQ -> return (cmpres <> 0 && compare_opt_strings entr1 entr2)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
  | (_, l) ->
      print_endline
        (Format.asprintf
            "%a"
            (PP_helpers.list_sep_d Ligo_interpreter.PP.pp_value)
            l) ;
      fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"

let rec apply_comparison :
    Location.t ->
    calltrace ->
    AST.constant' ->
    value list ->
    value Monad.t =
  fun loc calltrace c operands ->
  let open Monad in
  match (c, operands) with
  | (C_EQ  , [ (V_Michelson _) as a ; (V_Michelson _) as b ] ) ->
    let>> b = Michelson_equal (loc,a,b) in
    return @@ v_bool b
  | (C_NEQ  , [ (V_Michelson _) as a ; (V_Michelson _) as b ] ) ->
    let>> b = Michelson_equal (loc,a,b) in
    return @@ v_bool (not b)
  | (comp, [(V_Ct _ as v1); (V_Ct _ as v2)]) ->
      compare_constants comp v1 v2 loc calltrace
  | (comp, [V_Ligo (a1, b1); V_Ligo (a2, b2)]) ->
      let* x =
        match comp with
        | C_EQ  -> return String.(a1 = a2 && b1 = b2)
        | C_NEQ -> return String.(a1 = a2 && b1 = b2)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
  | (comp, [V_List   _ as xs; V_List   _ as ys])
  | (comp, [V_Set    _ as xs; V_Set    _ as ys])
  | (comp, [V_Map    _ as xs; V_Map    _ as ys])
  | (comp, [V_Record _ as xs; V_Record _ as ys]) ->
    let c = Ligo_interpreter.Combinators.equal_value xs ys in
    let* v =
      match comp with
      | C_EQ  -> return @@ v_bool c
      | C_NEQ -> return @@ v_bool (not c)
      | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
    in
    return v
  | (comp, [V_Construct (ctor_a, args_a); V_Construct (ctor_b, args_b)]) -> (
      match comp with
      | C_EQ ->
          if String.equal ctor_a ctor_b then
            let* r = apply_comparison loc calltrace c [args_a; args_b] in
            Monad.return @@ v_bool @@ is_true r
          else Monad.return @@ v_bool false
      | C_NEQ ->
          if not (String.equal ctor_a ctor_b) then Monad.return @@ v_bool true
          else
            let* r = apply_comparison loc calltrace c [args_a; args_b] in
            Monad.return @@ v_bool @@ is_true r
      | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable")
  | (_, l) ->
    (* TODO: Don't know how to compare these *)
      (* V_Func_val *)
      (* V_Mutation *)
      (* V_Failure *)
      (* V_Michelson *)
      (* V_BigMap *)
      print_endline
        (Format.asprintf
            "%a"
            (PP_helpers.list_sep_d Ligo_interpreter.PP.pp_value)
            l) ;
      fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"

let rec apply_operator ~raise ~steps ~(options : Compiler_options.t) : Location.t -> calltrace -> AST.type_expression -> env -> AST.constant' -> (value * AST.type_expression * Location.t) list -> value Monad.t =
  fun loc calltrace expr_ty env c operands ->
  let open Monad in
  let eval_ligo = eval_ligo ~raise ~steps ~options in
  let locs = List.map ~f:(fun (_, _, c) -> c) operands in
  let types = List.map ~f:(fun (_, b, _) -> b) operands in
  let operands = List.map ~f:(fun (a, _, _) -> a) operands in
  let error_type = Errors.generic_error loc "Type error." in
  let return_ct v = return @@ V_Ct v in
  let return_none () = return @@ v_none () in
  let return_some v = return @@ v_some v in
  let return_contract_exec_exn = function
    | `Exec_ok gas -> return_ct (C_nat gas)
    | `Exec_failed e -> fail @@ Errors.target_lang_error loc calltrace e
  in
  let return_contract_exec = function
    | `Exec_ok gas -> return (LC.v_ctor "Success" (LC.v_nat gas))
    | `Exec_failed e ->
      let>> a = State_error_to_value e in
      return a
  in
  ( match (c,operands) with
    (* nullary *)
    | ( C_NONE , [] ) -> return_none ()
    | ( C_NONE , _  ) -> fail @@ error_type
    | ( C_UNIT , [] ) -> return @@ V_Ct C_unit
    | ( C_UNIT , _  ) -> fail @@ error_type
    | ( C_NIL  , [] ) -> return @@ V_List []
    | ( C_NIL , _  ) -> fail @@ error_type
    | ( C_TRUE , [] ) -> return @@ v_bool true
    | ( C_TRUE , _  ) -> fail @@ error_type
    | ( C_FALSE , [] ) -> return @@ v_bool false
    | ( C_FALSE , _  ) -> fail @@ error_type
    (* unary *)
    | ( C_SIZE   , [(V_Set l | V_List l)] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Map l            ] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Ct (C_string s ) ] ) -> return_ct @@ C_nat (Z.of_int @@ String.length s)
    | ( C_SIZE   , [ V_Ct (C_bytes b  ) ] ) -> return_ct @@ C_nat (Z.of_int @@ Bytes.length b)
    | ( C_SIZE , _  ) -> fail @@ error_type
    | ( C_NOT    , [ V_Ct (C_bool a'  ) ] ) -> return_ct @@ C_bool (not a')
    (* TODO-er: fix two complements: *)
    | ( C_NOT    , [ V_Ct (C_int a'   ) ] ) -> return_ct @@ C_int (Z.neg a')
    | ( C_NOT    , [ V_Ct (C_nat a'   ) ] ) -> return_ct @@ C_int (Z.neg a')
    | ( C_NOT , _  ) -> fail @@ error_type
    | ( C_INT    , [ V_Ct (C_nat a')    ] ) -> return_ct @@ C_int a'
    | ( C_INT    , [ V_Ct (C_bls12_381_fr a')    ] ) -> return_ct @@ C_int (Bls12_381.Fr.to_z a')
    | ( C_INT , _  ) -> fail @@ error_type
    | ( C_ABS    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_nat (Z.abs a')
    | ( C_ABS , _  ) -> fail @@ error_type
    | ( C_NEG    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (Z.neg a')
    | ( C_NEG    , [ V_Ct (C_bls12_381_g1 a')    ] ) -> return_ct @@ C_bls12_381_g1 (Bls12_381.G1.negate a')
    | ( C_NEG    , [ V_Ct (C_bls12_381_g2 a')    ] ) -> return_ct @@ C_bls12_381_g2 (Bls12_381.G2.negate a')
    | ( C_NEG    , [ V_Ct (C_bls12_381_fr a')    ] ) -> return_ct @@ C_bls12_381_fr (Bls12_381.Fr.negate a')
    | ( C_NEG , _  ) -> fail @@ error_type
    | ( C_SOME   , [ v                  ] ) -> return_some v
    | ( C_SOME , _  ) -> fail @@ error_type
    | ( C_IS_NAT , [ V_Ct (C_int a')    ] ) ->
      if Z.Compare.(>) a' Z.zero then return_some @@ V_Ct (C_nat a')
      else return_none ()
    | ( C_IS_NAT , _  ) -> fail @@ error_type
    | ( C_ADDRESS , [ V_Ct (C_contract { address ; entrypoint=_}) ] ) ->
      return (V_Ct (C_address address))
    | ( C_ADDRESS , _  ) -> fail @@ error_type
    | ( C_BYTES_PACK , [ value ] ) ->
      let* () = check_value value in
      let value_ty = List.nth_exn types 0 in
      let>> value = Pack (loc, value, value_ty) in
      return value
    | ( C_BYTES_PACK , _  ) -> fail @@ error_type
    | ( C_BYTES_UNPACK , [ V_Ct (C_bytes bytes) ] ) ->
      let value_ty = expr_ty in
      let>> value = Unpack (loc, bytes, value_ty) in
      return value
    | ( C_BYTES_UNPACK , _  ) -> fail @@ error_type
    | ( C_ASSERTION , [ v ] ) ->
      if (is_true v) then return_ct @@ C_unit
      else fail @@ Errors.meta_lang_eval loc calltrace "Failed assertion"
    | ( C_ASSERTION , _  ) -> fail @@ error_type
    | ( C_ASSERTION_WITH_ERROR , [ v ; V_Ct (C_string s) ] ) ->
      if (is_true v) then return_ct @@ C_unit
      else fail @@ Errors.meta_lang_eval loc calltrace s
    | ( C_ASSERTION_WITH_ERROR , _  ) -> fail @@ error_type
    | ( C_ASSERT_SOME , [ v ] ) -> (
      match get_option v with
      | Some (Some _) -> return_ct @@ C_unit
      | Some None -> fail @@ Errors.meta_lang_eval loc calltrace "Failed assert some"
      | None -> fail @@ Errors.generic_error loc "Expected option type"
    )
    | ( C_ASSERT_SOME , _  ) -> fail @@ error_type
    | ( C_ASSERT_SOME_WITH_ERROR , [ v ; V_Ct (C_string s) ] ) -> (
      match get_option v with
      | Some (Some _) -> return_ct @@ C_unit
      | Some None -> fail @@ Errors.meta_lang_eval loc calltrace s
      | None -> fail @@ Errors.generic_error loc "Expected option type"
    )
    | ( C_ASSERT_SOME_WITH_ERROR , _  ) -> fail @@ error_type
    | ( C_ASSERT_NONE , [ v ] ) -> (
      match get_option v with
      | Some (Some _) -> fail @@ Errors.meta_lang_eval loc calltrace "Failed assert none"
      | Some None -> return_ct @@ C_unit
      | None -> fail @@ Errors.generic_error loc "Expected option type"
    )
    | ( C_ASSERT_NONE , _  ) -> fail @@ error_type
    | ( C_ASSERT_NONE_WITH_ERROR , [ v ; V_Ct (C_string s) ] ) -> (
      match get_option v with
      | Some (Some _) -> fail @@ Errors.meta_lang_eval loc calltrace s
      | Some None -> return_ct @@ C_unit
      | None -> fail @@ Errors.generic_error loc "Expected option type"
    )
    | ( C_ASSERT_NONE_WITH_ERROR , _  ) -> fail @@ error_type
    | ( C_UNOPT , [ v ] ) -> (
      match get_option v with
      | Some (Some value) -> return @@ value
      | Some None -> fail @@ Errors.meta_lang_eval loc calltrace "option is None"
      | None -> fail @@ Errors.generic_error loc "Expected option type"
    )
    | ( C_UNOPT , _  ) -> fail @@ error_type
    | ( C_UNOPT_WITH_ERROR , [ v ; V_Ct (C_string s) ] ) -> (
      match get_option v with
      | Some (Some value) -> return @@ value
      | Some None -> fail @@ Errors.meta_lang_eval loc calltrace s
      | None -> fail @@ Errors.generic_error loc "Expected option type"
    )
    | ( C_UNOPT_WITH_ERROR , _  ) -> fail @@ error_type
    | ( C_MAP_FIND_OPT , [ k ; V_Map l ] ) -> ( match List.Assoc.find ~equal:LC.equal_value l k with
      | Some v -> return @@ v_some v
      | None -> return @@ v_none ()
    )
    | ( C_MAP_FIND_OPT , _  ) -> fail @@ error_type
    | ( C_MAP_FIND , [ k ; V_Map l ] ) -> ( match List.Assoc.find ~equal:LC.equal_value l k with
      | Some v -> return @@ v
      | None -> fail @@ Errors.meta_lang_eval loc calltrace (Predefined.Tree_abstraction.pseudo_module_to_string c)
    )
    | ( C_MAP_FIND , _  ) -> fail @@ error_type
    (* binary *)
    | ( (C_EQ | C_NEQ | C_LT | C_LE | C_GT | C_GE) , _ ) -> apply_comparison loc calltrace c operands
    | ( C_SUB    , [ V_Ct (C_int a' | C_nat a') ; V_Ct (C_int b' | C_nat b') ] ) -> return_ct @@ C_int (Z.sub a' b')
    | ( C_SUB    , [ V_Ct (C_int a' | C_timestamp a') ; V_Ct (C_timestamp b' | C_int b') ] ) ->
      let res = Michelson_backend.Tezos_eq.timestamp_sub a' b' in
      return_ct @@ C_timestamp res
    | ( C_SUB    , [ V_Ct (C_mutez a') ; V_Ct (C_mutez b') ] ) -> (
      match Michelson_backend.Tezos_eq.mutez_sub a' b' with
      | Some res -> return_ct @@ C_mutez res
      | None -> fail (Errors.meta_lang_eval loc calltrace "Mutez underflow/overflow")
    )
    | ( C_SUB_MUTEZ    , [ V_Ct (C_mutez a') ; V_Ct (C_mutez b') ] ) -> (
      match Michelson_backend.Tezos_eq.mutez_sub a' b' with
      | Some res -> return @@ v_some @@ V_Ct (C_mutez res)
      | None -> return @@ v_none ()
    )
    | ( C_SUB , _  ) -> fail @@ error_type
    | ( C_SUB_MUTEZ , _  ) -> fail @@ error_type
    | ( C_CONS   , [ v                  ; V_List vl          ] ) -> return @@ V_List (v::vl)
    | ( C_CONS , _  ) -> fail @@ error_type
    | ( C_ADD    , [ V_Ct (C_int a  )  ; V_Ct (C_int b  )  ] )
    | ( C_ADD    , [ V_Ct (C_nat a  )  ; V_Ct (C_int b  )  ] )
    | ( C_ADD    , [ V_Ct (C_int a  )  ; V_Ct (C_nat b  )  ] ) -> let r = Z.add a b in return_ct (C_int r)
    | ( C_ADD    , [ V_Ct (C_nat a  )  ; V_Ct (C_nat b  )  ] ) -> let r = Z.add a b in return_ct (C_nat r)
    | ( C_ADD    , [ V_Ct (C_int a' | C_timestamp a') ; V_Ct (C_timestamp b' | C_int b') ] ) ->
      let res = Michelson_backend.Tezos_eq.timestamp_add a' b' in
      return_ct @@ C_timestamp res
    | ( C_ADD    , [ V_Ct (C_mutez a') ; V_Ct (C_mutez b') ] ) -> (
      match Michelson_backend.Tezos_eq.mutez_add a' b' with
      | Some res -> return_ct @@ C_mutez res
      | None -> fail (Errors.meta_lang_eval loc calltrace "Mutez underflow/overflow")
    )
    | ( C_ADD    , [ V_Ct (C_bls12_381_g1 a) ; V_Ct (C_bls12_381_g1 b) ] ) -> let r = Bls12_381.G1.(add a b) in return_ct (C_bls12_381_g1 r)
    | ( C_ADD    , [ V_Ct (C_bls12_381_g2 a) ; V_Ct (C_bls12_381_g2 b) ] ) -> let r = Bls12_381.G2.(add a b) in return_ct (C_bls12_381_g2 r)
    | ( C_ADD    , [ V_Ct (C_bls12_381_fr a) ; V_Ct (C_bls12_381_fr b) ] ) -> let r = Bls12_381.Fr.(a + b) in return_ct (C_bls12_381_fr r)
    | ( C_ADD , _  ) -> fail @@ error_type
    | ( C_MUL    , [ V_Ct (C_int a  )  ; V_Ct (C_int b  )  ] )
    | ( C_MUL    , [ V_Ct (C_nat a  )  ; V_Ct (C_int b  )  ] )
    | ( C_MUL    , [ V_Ct (C_int a  )  ; V_Ct (C_nat b  )  ] ) -> let r = Z.mul a b in return_ct (C_int r)
    | ( C_MUL    , [ V_Ct (C_nat a  )  ; V_Ct (C_nat b  )  ] ) -> let r = Z.mul a b in return_ct (C_nat r)
    | ( C_MUL    , [ V_Ct (C_nat a  )  ; V_Ct (C_mutez b)  ] ) -> let r = Z.mul a b in return_ct (C_mutez r)
    | ( C_MUL    , [ V_Ct (C_mutez a)  ; V_Ct (C_nat b  )  ] ) -> let r = Z.mul a b in return_ct (C_mutez r)
    | ( C_MUL    , [ V_Ct (C_bls12_381_g1 a) ; V_Ct (C_bls12_381_fr b) ] ) -> let r = Bls12_381.G1.(mul a b) in return_ct (C_bls12_381_g1 r)
    | ( C_MUL    , [ V_Ct (C_bls12_381_g2 a) ; V_Ct (C_bls12_381_fr b) ] ) -> let r = Bls12_381.G2.(mul a b) in return_ct (C_bls12_381_g2 r)
    | ( C_MUL    , [ V_Ct (C_bls12_381_fr a) ; V_Ct (C_bls12_381_fr b) ] ) -> let r = Bls12_381.Fr.(a * b) in return_ct (C_bls12_381_fr r)
    | ( C_MUL    , [ V_Ct (C_nat a) ; V_Ct (C_bls12_381_fr b) ] ) -> let r = Bls12_381.Fr.(b ** a) in return_ct (C_bls12_381_fr r)
    | ( C_MUL    , [ V_Ct (C_int a) ; V_Ct (C_bls12_381_fr b) ] ) -> let r = Bls12_381.Fr.(b ** a) in return_ct (C_bls12_381_fr r)
    | ( C_MUL    , [ V_Ct (C_bls12_381_fr a) ; V_Ct (C_nat b) ] ) -> let r = Bls12_381.Fr.(a ** b) in return_ct (C_bls12_381_fr r)
    | ( C_MUL    , [ V_Ct (C_bls12_381_fr a) ; V_Ct (C_int b) ] ) -> let r = Bls12_381.Fr.(a ** b) in return_ct (C_bls12_381_fr r)
    | ( C_MUL , _  ) -> fail @@ error_type
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] ) ->
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_int res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV    , [ V_Ct (C_nat a')  ; V_Ct (C_nat b')  ] ) ->
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_nat res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_mutez b')  ] ) ->
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_nat res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_nat b')  ] ) ->
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_mutez res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV , _  ) -> fail @@ error_type
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_int b')    ] )
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_nat b')    ] )
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_int b')    ] ) -> (
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      match a with
      | Some (_,r) -> return_ct @@ C_nat r
      | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
    )
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_nat b')    ] ) -> (
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      match a with
      | Some (_,r) -> return_ct @@ C_nat r
      | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
    )
    | ( C_MOD , _  ) -> fail @@ error_type
    | ( C_CONCAT , [ V_Ct (C_string a') ; V_Ct (C_string b') ] ) -> return_ct @@ C_string (a' ^ b')
    | ( C_CONCAT , [ V_Ct (C_bytes a' ) ; V_Ct (C_bytes b' ) ] ) -> return_ct @@ C_bytes  (BytesLabels.cat a' b')
    | ( C_CONCAT , _  ) -> fail @@ error_type
    | ( C_OR     , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' || b')
    | ( C_AND    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' && b')
    | ( C_XOR    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   ( (a' || b') && (not (a' && b')) )
    (* Bitwise operators *)
    | ( C_AND    , [ V_Ct (C_int a'  ) ; V_Ct (C_nat b'  ) ] ) -> let v = Z.logand a' b' in return_ct @@ C_nat v
    | ( C_AND    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) -> let v = Z.logand a' b' in return_ct @@ C_nat v
    | ( C_OR     , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) -> let v = Z.logor a' b' in return_ct @@ C_nat v
    | ( C_XOR    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) -> let v = Z.logxor a' b' in return_ct @@ C_nat v
    | ( C_OR , _  ) -> fail @@ error_type
    | ( C_AND , _  ) -> fail @@ error_type
    | ( C_XOR , _  ) -> fail @@ error_type
    | ( C_LSL    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) ->
      let v = Michelson_backend.Tezos_eq.nat_shift_left a' b' in
      begin
        match v with
        | Some v -> return_ct @@ C_nat v
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Overflow"
      end
    | ( C_LSL , _  ) -> fail @@ error_type
    | ( C_LSR    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) ->
      let v = Michelson_backend.Tezos_eq.nat_shift_right a' b' in
      begin
        match v with
        | Some v -> return_ct @@ C_nat v
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Overflow"
      end
    | ( C_LSR , _  ) -> fail @@ error_type
    | ( C_LIST_EMPTY, []) -> return @@ V_List ([])
    | ( C_LIST_EMPTY , _  ) -> fail @@ error_type
    | ( C_LIST_MAP , [ V_Func_val {arg_binder ; body ; env ; rec_name=_ ; orig_lambda=_}  ; V_List (elts) ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ AST.get_t_list lst_ty in
      let* elts =
        Monad.bind_map_list
          (fun elt ->
            let env' = Env.extend env arg_binder (ty,elt) in
            eval_ligo body calltrace env')
          elts
      in
      return (V_List elts)
    | ( C_LIST_MAP , _  ) -> fail @@ error_type
    | ( C_MAP_MAP , [ V_Func_val {arg_binder ; body ; env ; rec_name=_ ; orig_lambda=_}  ; V_Map (elts) ] ) ->
      let* map_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* k_ty,v_ty = monad_option (Errors.generic_error map_ty.location "Expected map type") @@ AST.get_t_map map_ty in
      let* elts =
        Monad.bind_map_list
          (fun (k,v) ->
            let env' = Env.extend env arg_binder ((AST.t_pair k_ty v_ty),v_pair (k,v)) in
            let* v' = eval_ligo body calltrace env' in
            return @@ (k,v')
          )
          elts
      in
      return (V_Map elts)
    | ( C_MAP_MAP , _  ) -> fail @@ error_type
    | ( C_LIST_ITER , [ V_Func_val {arg_binder ; body ; env ; rec_name=_; orig_lambda=_}  ; V_List (elts) ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ AST.get_t_list lst_ty in
      Monad.bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env arg_binder (ty,elt) in
          eval_ligo body calltrace env'
        )
        (V_Ct C_unit) elts
    | ( C_LIST_ITER , _  ) -> fail @@ error_type
    | ( C_MAP_ITER , [ V_Func_val {arg_binder ; body ; env; rec_name=_; orig_lambda=_}  ; V_Map (elts) ] ) ->
      let* map_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* k_ty,v_ty = monad_option (Errors.generic_error map_ty.location "Expected map type") @@ AST.get_t_map map_ty in
      Monad.bind_fold_list
        (fun _ kv ->
          let env' = Env.extend env arg_binder ((AST.t_pair k_ty v_ty),v_pair kv) in
          eval_ligo body calltrace env'
        )
        (V_Ct C_unit) elts
    | ( C_MAP_ITER , _  ) -> fail @@ error_type
    (* ternary *)
    | ( C_SLICE , [ V_Ct (C_nat st) ; V_Ct (C_nat ed) ; V_Ct (C_string s) ] ) ->
      (*TODO : allign with tezos*)
      return @@ V_Ct (C_string (String.sub s ~pos:(Z.to_int st) ~len:(Z.to_int ed)))
    | ( C_SLICE , [ V_Ct (C_nat start) ; V_Ct (C_nat length) ; V_Ct (C_bytes bytes) ] ) ->
      let start = Z.to_int start in
      let length = Z.to_int length in
      if (start > Bytes.length bytes) || (start + length > Bytes.length bytes) then
        fail @@ Errors.meta_lang_failwith loc calltrace (V_Ct (C_string "SLICE"))
      else
        return @@ V_Ct (C_bytes (Bytes.sub bytes ~pos:start ~len:length))
    | ( C_SLICE , _  ) -> fail @@ error_type
    | ( C_LOOP_LEFT , [ V_Func_val {arg_binder ; body ; env ; rec_name=_; orig_lambda=_} ; init ] ) -> (
      let* init_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let rec aux cur_env =
        let env' = Env.extend env arg_binder (init_ty, cur_env) in
        let* ret = eval_ligo body calltrace env' in
        match ret with
        | V_Construct ("##Loop_continue", v) -> aux v
        | V_Construct ("##Loop_stop", v) -> return v
        | _ -> fail @@ error_type
      in
      aux init
    )
    | ( C_LOOP_LEFT , _ ) -> fail @@ error_type
    | C_LOOP_CONTINUE , [ v ] -> return (v_ctor "##Loop_continue" v)
    | ( C_LOOP_CONTINUE , _ ) -> fail @@ error_type
    | C_LOOP_STOP , [ v ]  -> return (v_ctor "##Loop_stop" v)
    | ( C_LOOP_STOP , _ ) -> fail @@ error_type
    | ( C_LIST_FOLD_LEFT , [ V_Func_val {arg_binder ; body ; env ; rec_name=_; orig_lambda=_}  ; init ; V_List elts ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ AST.get_t_list lst_ty in
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env arg_binder ((AST.t_pair acc_ty ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_LIST_FOLD_LEFT , _  ) -> fail @@ error_type
    | ( C_FOLD ,      [ V_Func_val {arg_binder ; body ; env; rec_name=_; orig_lambda=_}  ; V_List elts ; init ] )
    | ( C_LIST_FOLD , [ V_Func_val {arg_binder ; body ; env; rec_name=_; orig_lambda=_}  ; V_List elts ; init ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ AST.get_t_list lst_ty in
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env arg_binder ((AST.t_pair acc_ty ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_FOLD ,     [ V_Func_val {arg_binder ; body ; env ; rec_name=_; orig_lambda=_}  ; V_Set elts ; init ] )
    | ( C_SET_FOLD , [ V_Func_val {arg_binder ; body ; env ; rec_name=_; orig_lambda=_}  ; V_Set elts ; init ] ) ->
      let* set_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* ty = monad_option (Errors.generic_error set_ty.location "Expected set type") @@ AST.get_t_set set_ty in
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env arg_binder (AST.(t_pair acc_ty ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_FOLD , _  ) -> fail @@ error_type
    | ( C_LIST_FOLD , _  ) -> fail @@ error_type
    | ( C_SET_FOLD , _  ) -> fail @@ error_type
    | ( C_LIST_FOLD_RIGHT , [ V_Func_val {arg_binder ; body ; env; rec_name=_; orig_lambda=_}  ; V_List elts ; init ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ AST.get_t_list lst_ty in
      Monad.bind_fold_right_list
        (fun elt prev ->
          let fold_args = v_pair (elt,prev) in
          let env' = Env.extend env arg_binder ((AST.t_pair ty acc_ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_LIST_FOLD_RIGHT , _  ) -> fail @@ error_type
    | ( C_LIST_HEAD_OPT , [ V_List elts ] ) ->
      (match (List.hd elts) with
      | Some v -> return @@ v_some v
      | None   -> return @@ v_none ())
    | ( C_LIST_HEAD_OPT , _  ) -> fail @@ error_type
    | ( C_LIST_TAIL_OPT , [ V_List elts ] ) ->
      (match (List.tl elts) with
      | Some v -> return @@ v_some (V_List v)
      | None   -> return @@ v_none ())
    | ( C_LIST_TAIL_OPT , _  ) -> fail @@ error_type
    | ( C_BIG_MAP_EMPTY , []) -> return @@ V_Map ([])
    | ( C_BIG_MAP_EMPTY , _  ) -> fail @@ error_type
    | ( C_MAP_EMPTY , []) -> return @@ V_Map ([])
    | ( C_MAP_EMPTY , _  ) -> fail @@ error_type
    | ( C_MAP_FOLD , [ V_Func_val {arg_binder ; body ; env; rec_name=_; orig_lambda=_}  ; V_Map kvs ; init ] ) ->
      let* map_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* k_ty,v_ty = monad_option (Errors.generic_error map_ty.location "Expected map type") @@ AST.get_t_map map_ty in
      Monad.bind_fold_list
        (fun prev kv ->
          let fold_args = v_pair (prev, v_pair kv) in
          let env' = Env.extend env arg_binder (AST.(t_pair acc_ty (t_pair k_ty v_ty)),  fold_args) in
          eval_ligo body calltrace env'
        )
        init kvs
    | ( C_MAP_FOLD , _  ) -> fail @@ error_type
    | ( C_MAP_MEM , [k ; V_Map kvs]) -> return @@ v_bool (List.Assoc.mem ~equal:LC.equal_value kvs k)
    | ( C_MAP_MEM , _  ) -> fail @@ error_type
    | ( C_MAP_ADD , [ k ; v ; V_Map kvs] ) -> return (V_Map ((k,v) :: List.Assoc.remove ~equal:LC.equal_value kvs k))
    | ( C_MAP_ADD , _  ) -> fail @@ error_type
    | ( C_MAP_REMOVE , [ k ; V_Map kvs] ) -> return @@ V_Map (List.Assoc.remove ~equal:LC.equal_value kvs k)
    | ( C_MAP_REMOVE , _  ) -> fail @@ error_type
    | ( C_MAP_UPDATE , [ k ; V_Construct (option,v) ; V_Map kvs] ) -> (match option with
      | "Some" -> return @@ V_Map ((k,v)::(List.Assoc.remove ~equal:LC.equal_value kvs k))
      | "None" -> return @@ V_Map (List.Assoc.remove ~equal:LC.equal_value kvs k)
      | _ -> assert false
    )
    | ( C_MAP_UPDATE , _  ) -> fail @@ error_type
    | ( C_MAP_GET_AND_UPDATE , [k ; V_Construct (option,v) ; V_Map kvs ] ) ->
      let old_value = List.Assoc.find ~equal:LC.equal_value kvs k in
      let old_value = (match old_value with
        | Some v -> v_some v
        | None -> v_none ())
      in
      (match option with
      | "Some" -> return @@ v_pair (old_value, V_Map ((k,v)::(List.Assoc.remove ~equal:LC.equal_value kvs k)))
      | "None" -> return @@ v_pair (old_value, V_Map (List.Assoc.remove ~equal:LC.equal_value kvs k))
      | _ -> assert false)
    | ( C_MAP_GET_AND_UPDATE , _  ) -> fail @@ error_type
    | ( C_SET_EMPTY, []) -> return @@ V_Set ([])
    | ( C_SET_EMPTY , _  ) -> fail @@ error_type
    | ( C_SET_ADD , [ v ; V_Set l ] ) -> return @@ V_Set (List.dedup_and_sort ~compare:LC.compare_value (v::l))
    | ( C_SET_ADD , _  ) -> fail @@ error_type
    | ( C_SET_FOLD_DESC , [ V_Func_val {arg_binder ; body ; env; rec_name=_; orig_lambda=_}  ; V_Set elts ; init ] ) ->
      let* set_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* ty = monad_option (Errors.generic_error set_ty.location "Expected set type") @@ AST.get_t_set set_ty in
      Monad.bind_fold_right_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env arg_binder (AST.(t_pair acc_ty ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_SET_FOLD_DESC , _  ) -> fail @@ error_type
    | ( C_SET_ITER , [ V_Func_val {arg_binder ; body ; env; rec_name=_; orig_lambda=_}  ; V_Set (elts) ] ) ->
      let* set_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* ty = monad_option (Errors.generic_error set_ty.location "Expected set type") @@ AST.get_t_set set_ty in
      Monad.bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env arg_binder (ty,elt) in
          eval_ligo body calltrace env'
        )
        (V_Ct C_unit) elts
    | ( C_SET_ITER , _  ) -> fail @@ error_type
    | ( C_SET_MEM    , [ v ; V_Set (elts) ] ) -> return @@ v_bool (List.mem ~equal:LC.equal_value elts v)
    | ( C_SET_MEM , _  ) -> fail @@ error_type
    | ( C_SET_REMOVE , [ v ; V_Set (elts) ] ) -> return @@ V_Set (List.filter ~f:(fun el -> not (equal_value el v)) elts)
    | ( C_SET_REMOVE , _  ) -> fail @@ error_type
    | ( C_SET_UPDATE , [ v ; b ; V_Set elts ] ) ->
      if is_true b
      then return @@ V_Set (List.dedup_and_sort ~compare:LC.compare_value (v::elts))
      else return @@ V_Set (List.filter ~f:(fun el -> not (equal_value el v)) elts)
    | ( C_SET_UPDATE , _  ) -> fail @@ error_type
    | ( C_OPTION_MAP , [ V_Func_val {arg_binder ; body ; env ; rec_name=_ ; orig_lambda=_}  ; V_Construct ("Some" , v) ] ) ->
      let* opt_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* ty = monad_option (Errors.generic_error opt_ty.location "Expected option type") @@ AST.get_t_option opt_ty in
      let* new_v =
        let env' = Env.extend env arg_binder (ty,v) in
        eval_ligo body calltrace env'
      in
      return (V_Construct ("Some" , new_v))
    | ( C_OPTION_MAP , [ V_Func_val _  ; V_Construct ("None" , V_Ct C_unit) as v ] ) ->
      return v
    | ( C_OPTION_MAP , _  ) -> fail @@ error_type
    | ( C_SHA256, [ V_Ct (C_bytes b) ] )->
      let>> value = Sha256 b in
      return @@ value
    | ( C_SHA256 , _  ) -> fail @@ error_type
    | ( C_SHA512, [ V_Ct (C_bytes b) ] )->
      let>> value = Sha512 b in
      return @@ value
    | ( C_SHA512 , _  ) -> fail @@ error_type
    | ( C_BLAKE2b, [ V_Ct (C_bytes b) ] )->
      let>> value = Blake2b b in
      return @@ value
    | ( C_BLAKE2b , _  ) -> fail @@ error_type
    | ( C_KECCAK, [ V_Ct (C_bytes b) ] )->
      let>> value = Keccak b in
      return @@ value
    | ( C_KECCAK , _  ) -> fail @@ error_type
    | ( C_SHA3, [ V_Ct (C_bytes b) ] )->
      let>> value = Sha3 b in
      return @@ value
    | ( C_SHA3 , _  ) -> fail @@ error_type
    | ( C_HASH_KEY, [ V_Ct (C_key k) ] )->
      let>> value = Hash_key k in
      return @@ value
    | ( C_HASH_KEY , _  ) -> fail @@ error_type
    | ( C_CHECK_SIGNATURE, [ V_Ct (C_key k) ; V_Ct (C_signature s) ; V_Ct (C_bytes b) ] )->
      let>> value = Check_signature (k, s, b) in
      return @@ value
    | ( C_CHECK_SIGNATURE , _  ) -> fail @@ error_type
    | ( C_NEVER , [ _ ] ) -> failwith "Ex falso?"
    | ( C_NEVER , _  ) -> fail @@ error_type
    | ( C_PAIRING_CHECK , [ V_List l ] ) ->
      let rec aux acc = function
        | [] -> Some acc
        | (v :: tl) ->
           match get_pair v with
           | None -> None
           | Some (l, r) ->
              match get_bls12_381_g1 l, get_bls12_381_g2 r with
              | Some b, Some b' -> aux (acc @ [(b, b')]) tl
              | _ -> None in
      let l = trace_option ~raise error_type @@ aux [] l in
      let>> value = Pairing_check l in
      return @@ value
    | ( C_PAIRING_CHECK , _  ) -> fail @@ error_type
    | ( C_IMPLICIT_ACCOUNT, [ V_Ct (C_key_hash kh) ] )->
      let>> value = Implicit_account (loc, kh) in
      return @@ value
    | ( C_IMPLICIT_ACCOUNT , _  ) -> fail @@ error_type
    | ( C_FAILWITH , [ a ] ) ->
      fail @@ Errors.meta_lang_failwith loc calltrace a
    | ( C_FAILWITH , _  ) -> fail @@ error_type
    | ( (C_TICKET | C_READ_TICKET | C_SPLIT_TICKET | C_JOIN_TICKET) , _ ) ->
      fail @@ Errors.generic_error loc "Tickets are not supported."
    (*
    >>>>>>>>
      Test operators
    >>>>>>>>
    *)
    | ( C_TEST_ORIGINATE_FROM_FILE, [ V_Ct (C_string source_file) ; V_Ct (C_string entryp) ; V_List views ; storage ; V_Ct ( C_mutez amt ) ]) ->
      let views = List.map
                    ~f:(fun x -> trace_option ~raise (Errors.corner_case ()) @@ get_string x)
                    views
      in
      let>> (code,size) = Compile_contract_from_file (source_file,entryp,views) in
      let>> addr = Inject_script (loc, calltrace, code, storage, amt) in
      return @@ V_Record (LMap.of_list [ (Label "0", addr) ; (Label "1", code) ; (Label "2", size) ])
    | ( C_TEST_ORIGINATE_FROM_FILE , _  ) -> fail @@ error_type
    | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN , [ (V_Ct (C_address address)) ; V_Michelson (Ty_code { code = param ; _ }) ; V_Ct ( C_mutez amt ) ] ) -> (
      let contract = { address; entrypoint = None } in
      let>> res = External_call (loc,calltrace,contract,param,amt) in
      return_contract_exec_exn res
    )
    | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN , _  ) -> fail @@ error_type
    | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS , [ (V_Ct (C_address address)) ; V_Michelson (Ty_code { code = param ; _ }) ; V_Ct ( C_mutez amt ) ] ) -> (
      let contract = { address; entrypoint = None } in
      let>> res = External_call (loc,calltrace,contract,param,amt) in
      return_contract_exec res
    )
    | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS , _  ) -> fail @@ error_type
    | ( C_TEST_SET_SOURCE , [ addr ] ) ->
      let>> () = Set_source addr in
      return_ct C_unit
    | ( C_TEST_SET_SOURCE , _  ) -> fail @@ error_type
    | ( C_TEST_SET_BAKER , [ addr ] ) ->
      let>> () = Set_baker addr in
      return_ct C_unit
    | ( C_TEST_SET_BAKER , _  ) -> fail @@ error_type
    | ( C_TEST_GET_STORAGE_OF_ADDRESS , [ addr ] ) ->
      let>> storage = Get_storage_of_address (loc, calltrace, addr) in
      return storage
    | ( C_TEST_GET_STORAGE_OF_ADDRESS , _  ) -> fail @@ error_type
    | ( C_TEST_GET_BALANCE , [ addr ] ) ->
      let>> balance = Get_balance (loc, calltrace, addr) in
      return balance
    | ( C_TEST_GET_BALANCE , _  ) -> fail @@ error_type
    | ( C_TEST_MICHELSON_EQUAL , [ a ; b ] ) ->
      let>> b = Michelson_equal (loc,a,b) in
      return_ct (C_bool b)
    | ( C_TEST_MICHELSON_EQUAL , _  ) -> fail @@ error_type
    | ( C_TEST_LOG , [ v ]) ->
      let () = Format.printf "%a\n" Ligo_interpreter.PP.pp_value v in
      return_ct C_unit
    | ( C_TEST_LOG , _  ) -> fail @@ error_type
    | ( C_TEST_BOOTSTRAP_CONTRACT , [ V_Ct (C_mutez z) ; contract ; storage ] ) ->
       let* contract_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
       let* storage_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
       let>> code = Compile_contract (loc, contract, contract_ty) in
       let>> storage = Eval (loc, storage, storage_ty) in
       let>> () = Bootstrap_contract ((Z.to_int z), code, storage, contract_ty) in
       return_ct C_unit
    | ( C_TEST_BOOTSTRAP_CONTRACT , _  ) -> fail @@ error_type
    | ( C_TEST_NTH_BOOTSTRAP_CONTRACT , [ V_Ct (C_nat n) ] ) ->
       let n = Z.to_int n in
       let>> address = Nth_bootstrap_contract n in
       return_ct (C_address address)
    | ( C_TEST_NTH_BOOTSTRAP_CONTRACT , _  ) -> fail @@ error_type
    | ( C_TEST_STATE_RESET , [ n ; amts ] ) ->
      let>> () = Reset_state (loc,calltrace,n,amts) in
      return_ct C_unit
    | ( C_TEST_STATE_RESET , _  ) -> fail @@ error_type
    | ( C_TEST_GET_NTH_BS , [ n ] ) ->
      let>> x = Get_bootstrap (loc,n) in
      return x
    | ( C_TEST_GET_NTH_BS , _  ) -> fail @@ error_type
    | ( C_TEST_LAST_ORIGINATIONS , [ _ ] ) ->
      let>> x = Get_last_originations () in
      return x
    | ( C_TEST_LAST_ORIGINATIONS , _  ) -> fail @@ error_type
    | ( C_TEST_MUTATE_VALUE , [ V_Ct (C_nat n); v ] ) -> (
      let* () = check_value v in
      let* value_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let v = Mutation.mutate_some_value ~raise loc n v value_ty in
      match v with
      | None ->
         return (v_none ())
      | Some (e, m) ->
         let* v = eval_ligo e calltrace env in
         return @@ (v_some (V_Record (LMap.of_list [ (Label "0", v) ; (Label "1", V_Mutation m) ]))))
    | ( C_TEST_MUTATE_VALUE , _  ) -> fail @@ error_type
    | ( C_TEST_MUTATION_TEST , [ v; tester ] ) -> (
      let* () = check_value v in
      let* value_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
      let l = Mutation.mutate_all_value ~raise loc v value_ty in
      let aux : AST.expression * 'a -> (value * 'a) option t = fun (e, m) ->
        let* v = eval_ligo e calltrace env in
        let r = match tester with
          | V_Func_val {arg_binder ; body ; env; rec_name = None ; orig_lambda } ->
            let* AST.{ type1 = in_ty ; type2 = _ } = monad_option (Errors.generic_error loc "Expected function type") @@
                             AST.get_t_arrow orig_lambda.type_expression in
            let f_env' = Env.extend env arg_binder (in_ty, v) in
            eval_ligo body (loc :: calltrace) f_env'
          | V_Func_val {arg_binder ; body ; env; rec_name = Some fun_name; orig_lambda } ->
            let* AST.{ type1 = in_ty ; type2 = _ } = monad_option (Errors.generic_error loc "Expected function type") @@
                              AST.get_t_arrow orig_lambda.type_expression in
            let f_env' = Env.extend env arg_binder (in_ty, v) in
            let f_env'' = Env.extend f_env' fun_name (orig_lambda.type_expression, tester) in
            eval_ligo body (loc :: calltrace) f_env''
          | _ -> raise.raise @@ Errors.generic_error loc "Trying to apply on something that is not a function?"
        in
        try_or (let* v = r in return (Some (v, m))) (return None)
      in
      let* r = iter_while aux l in
      match r with
       | None -> return (v_none ())
       | Some (v, m) -> return (v_some (V_Record (LMap.of_list [ (Label "0", v) ; (Label "1", V_Mutation m) ])))
    )
    | ( C_TEST_MUTATION_TEST , _  ) -> fail @@ error_type
    | ( C_TEST_MUTATION_TEST_ALL , [ v; tester ] ) ->
      let* () = check_value v in
      let* value_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
      let l = Mutation.mutate_all_value ~raise loc v value_ty in
      let* mutations = bind_map_list (fun (e, m) ->
        let* v = eval_ligo e calltrace env in
        let r =  match tester with
          | V_Func_val {arg_binder ; body ; env; rec_name = None ; orig_lambda } ->
             let AST.{ type1 = in_ty ; type2 = _ } = AST.get_t_arrow_exn orig_lambda.type_expression in
             let f_env' = Env.extend env arg_binder (in_ty, v) in
             eval_ligo body (loc :: calltrace) f_env'
          | V_Func_val {arg_binder ; body ; env; rec_name = Some fun_name; orig_lambda } ->
             let AST.{ type1 = in_ty ; type2 = _ } = AST.get_t_arrow_exn orig_lambda.type_expression in
             let f_env' = Env.extend env arg_binder (in_ty, v) in
             let f_env'' = Env.extend f_env' fun_name (orig_lambda.type_expression, tester) in
             eval_ligo body (loc :: calltrace) f_env''
          | _ -> fail @@ Errors.generic_error loc "Trying to apply on something that is not a function?" in
        Monad.try_or (let* v = r in return (Some (v, m))) (return None)) l in
      let r = List.map ~f:(fun (v, m) -> V_Record (LMap.of_list [ (Label "0", v) ; (Label "1", V_Mutation m) ])) @@ List.filter_opt mutations in
      return (V_List r)
    | ( C_TEST_MUTATION_TEST_ALL , _  ) -> fail @@ error_type
    | ( C_TEST_SAVE_MUTATION , [(V_Ct (C_string dir)) ; (V_Mutation ((loc, _) as mutation)) ] ) ->
      let* reg = monad_option (Errors.generic_error loc "Not a valid mutation") @@ Location.get_file loc in
      let file_contents = Fuzz.Ast_aggregated.buffer_of_mutation mutation in
      let id = Fuzz.Ast_aggregated.get_mutation_id mutation in
      let file_path = reg # file in
      (try
        let odir = Sys.getcwd () in
        let () = Sys.chdir dir in
        let file_path = Filename.basename file_path in
        let file_path = Caml.Filename.remove_extension file_path ^ "." ^ id ^ Caml.Filename.extension file_path in
        let out_chan = Out_channel.create file_path in
        let () = Caml.Buffer.output_buffer out_chan file_contents in
        let () = Sys.chdir odir in
        return (v_some (v_string file_path))
       with
       | Sys_error _ ->
          return (v_none ()))
    | ( C_TEST_SAVE_MUTATION , _  ) -> fail @@ error_type
    | ( C_TEST_TO_CONTRACT , [ addr ] ) ->
       let* contract_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let>> code = To_contract (loc, addr, None, contract_ty) in
       return code
    | ( C_TEST_TO_CONTRACT , _  ) -> fail @@ error_type
    | ( C_TEST_TO_ENTRYPOINT , [ V_Ct (C_string ent) ; addr ] ) ->
       let* contract_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let>> code = To_contract (loc, addr, Some ent, contract_ty) in
       return code
    | ( C_TEST_TO_ENTRYPOINT , _  ) -> fail @@ error_type
    | ( C_TEST_TO_TYPED_ADDRESS , [ V_Ct (C_contract { address; _ }) ] ) ->
       let>> () = Check_storage_address (loc, address, expr_ty) in
       let addr = LT.V_Ct ( C_address address ) in
       return addr
    | ( C_TEST_TO_TYPED_ADDRESS , _  ) -> fail @@ error_type
    | ( C_TEST_RUN , [ V_Func_val f ; v ] ) ->
       let* () = check_value (V_Func_val f) in
       let* () = check_value v in
       let>> code = Run (loc, f, v) in
       return code
    | ( C_TEST_RUN , _  ) -> fail @@ error_type
    | ( C_TEST_EVAL , [ v ] )
    | ( C_TEST_COMPILE_META_VALUE , [ v ] ) ->
       let* () = check_value v in
       let* value_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let>> code = Eval (loc, v, value_ty) in
       return code
    | ( C_TEST_EVAL , _  ) -> fail @@ error_type
    | ( C_TEST_COMPILE_META_VALUE , _  ) -> fail @@ error_type
    | ( C_TEST_DECOMPILE , [ V_Michelson (Ty_code { code_ty ; code ; ast_ty }) ] ) ->
      let* loc = monad_option (Errors.generic_error loc "Could not recover locations") @@ List.nth locs 0 in
      let () = trace_option ~raise (Errors.generic_error loc @@ Format.asprintf "This Michelson value has assigned type '%a', which does not coincide with expected type '%a'." AST.PP.type_expression ast_ty AST.PP.type_expression expr_ty) @@ AST.Helpers.assert_type_expression_eq (ast_ty, expr_ty) in
      let>> v = Decompile (code, code_ty, expr_ty) in
      return v
    | ( C_TEST_DECOMPILE , _  ) -> fail @@ error_type
    | ( C_TEST_GET_STORAGE , [ addr ] ) ->
       let* typed_address_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let storage_ty = match AST.get_t_typed_address typed_address_ty with
         | Some (_, storage_ty) -> storage_ty
         | _ -> failwith "Expecting typed_address" in
       let>> value = Get_storage(loc, calltrace, addr, storage_ty) in
       return value
    | ( C_TEST_GET_STORAGE , _  ) -> fail @@ error_type
    | ( C_TEST_ORIGINATE , [ contract ; storage ; V_Ct ( C_mutez amt ) ] ) ->
       let* contract_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let* storage_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
       let>> code = Compile_contract (loc, contract, contract_ty) in
       let>> storage = Eval (loc, storage, storage_ty) in
       let>> size = Get_size code in
       let>> addr  = Inject_script (loc, calltrace, code, storage, amt) in
       return @@ V_Record (LMap.of_list [ (Label "0", addr) ; (Label "1", code) ; (Label "2", size) ])
    | ( C_TEST_ORIGINATE , _  ) -> fail @@ error_type
    | ( C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN , [ (V_Ct (C_contract contract)) ; param ; V_Ct ( C_mutez amt ) ] ) -> (
       let* param_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
       let>> param = Eval (loc, param, param_ty) in
       match param with
       | V_Michelson (Ty_code { code = param ; _ }) ->
          let>> res = External_call (loc,calltrace,contract,param,amt) in
          return_contract_exec_exn res
       | _ -> fail @@ Errors.generic_error loc "Error typing param"
    )
    | ( C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN , _  ) -> fail @@ error_type
    | ( C_TEST_EXTERNAL_CALL_TO_CONTRACT , [ (V_Ct (C_contract contract)) ; param; V_Ct ( C_mutez amt ) ] ) -> (
       let* param_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
       let>> param = Eval (loc, param, param_ty) in
       match param with
       | V_Michelson (Ty_code { code = param ; _ }) ->
          let>> res = External_call (loc,calltrace,contract,param,amt) in
          return_contract_exec res
       | _ -> fail @@ Errors.generic_error loc "Error typing param"
    )
    | ( C_TEST_EXTERNAL_CALL_TO_CONTRACT , _  ) -> fail @@ error_type
    | ( C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS , [ V_Ct (C_nat n) ] ) ->
      let n = Z.to_int n in
      let* parameter_ty', storage_ty' = monad_option (Errors.generic_error loc "Expected typed address") @@
                                          AST.get_t_typed_address expr_ty in
      let>> (address, parameter_ty, storage_ty) = Nth_bootstrap_typed_address (loc, n) in
      let* () = monad_option (Errors.generic_error loc "Parameter in bootstrap contract does not match") @@
                   AST.Helpers.assert_type_expression_eq (parameter_ty, parameter_ty') in
      let* () = monad_option (Errors.generic_error loc "Storage in bootstrap contract does not match") @@
                   AST.Helpers.assert_type_expression_eq (storage_ty, storage_ty') in
      return_ct (C_address address)
    | ( C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS , _  ) -> fail @@ error_type
    | ( C_TEST_RANDOM , [ V_Ct (C_unit) ] ) ->
      let expr_gen = QCheck.Gen.generate1 (Mutation.expr_gen ~raise expr_ty)  in
      let* value = eval_ligo expr_gen calltrace env in
      return value
    | ( C_TEST_RANDOM , _  ) -> fail @@ error_type
    | ( C_TEST_SET_BIG_MAP , [ V_Ct (C_int n) ; V_Map kv ] ) ->
      let bigmap_ty = List.nth_exn types 1 in
      let>> () = Set_big_map (n, kv, bigmap_ty) in
      return_ct (C_unit)
    | ( C_TEST_SET_BIG_MAP , _  ) -> fail @@ error_type
    | ( C_TEST_CAST_ADDRESS , [ V_Ct (C_address x) ] ) ->
      return_ct (C_address x)
    | ( C_TEST_CAST_ADDRESS , _  ) -> fail @@ error_type
    | ( C_TEST_ADD_ACCOUNT , [ V_Ct (C_string sk) ; V_Ct (C_key pk) ] ) ->
      let>> () = Add_account (loc, sk, pk) in
      return @@ v_unit ()
    | ( C_TEST_ADD_ACCOUNT , _ ) -> fail @@ error_type
    | ( C_TEST_NEW_ACCOUNT , [ V_Ct (C_unit) ] ) ->
      let>> v = New_account () in
      return @@ v
    | ( C_TEST_NEW_ACCOUNT , _ ) -> fail @@ error_type
    | ( C_TEST_BAKER_ACCOUNT , [ account ; amount ] ) ->
      let>> () = Baker_account (account, amount) in
      return @@ v_unit ()
    | ( C_TEST_BAKER_ACCOUNT , _ ) -> fail @@ error_type
    | ( C_TEST_REGISTER_DELEGATE , [ V_Ct (C_key_hash pkh) ] ) ->
      let>> v = Register_delegate (loc, calltrace, pkh) in
      return @@ v
    | ( C_TEST_REGISTER_DELEGATE , _ ) -> fail @@ error_type
    | ( C_TEST_BAKE_UNTIL_N_CYCLE_END , [ V_Ct (C_nat n) ] ) ->
      let>> v = Bake_until_n_cycle_end (loc, calltrace, n) in
      return @@ v
    | ( C_TEST_BAKE_UNTIL_N_CYCLE_END , _ ) -> fail @@ error_type
    | ( C_TEST_CREATE_CHEST , [ V_Ct (C_bytes payload) ; V_Ct (C_nat time)] ) ->
      let (chest,chest_key) = Michelson_backend.create_chest payload (Z.to_int time) in
      return @@ v_pair (V_Ct (C_bytes chest) , V_Ct (C_bytes chest_key))
    | ( C_TEST_CREATE_CHEST , _  ) -> fail @@ error_type
    | ( C_TEST_CREATE_CHEST_KEY , [ V_Ct (C_bytes chest) ; V_Ct (C_nat time)] ) ->
      let chest_key = Michelson_backend.create_chest_key chest (Z.to_int time) in
      return @@ V_Ct (C_bytes chest_key)
    | ( C_TEST_GET_VOTING_POWER, [ V_Ct (C_key_hash hk) ]) ->
      let>> vp = Get_voting_power (loc, calltrace, hk) in
      return vp
    | ( C_TEST_GET_VOTING_POWER , _ ) -> fail @@ error_type
    | ( C_TEST_GET_TOTAL_VOTING_POWER, []) ->
      let>> tvp = Get_total_voting_power (loc, calltrace) in
      return tvp
    | ( C_TEST_GET_TOTAL_VOTING_POWER , _ ) -> fail @@ error_type
    | ( C_TEST_REGISTER_CONSTANT , [ V_Michelson (Ty_code { code ; _ } | Untyped_code code) ] ) ->
      let>> s = Register_constant (loc, calltrace, code) in
      return @@ V_Ct (C_string s)
    | ( C_TEST_REGISTER_CONSTANT , _ ) -> fail @@ error_type
    | ( C_TEST_CONSTANT_TO_MICHELSON , [ V_Ct (C_string m) ] ) ->
      let>> s = Constant_to_Michelson (loc, calltrace, m) in
      return @@ V_Michelson (Untyped_code s)
    | ( C_TEST_CONSTANT_TO_MICHELSON , _ ) -> fail @@ error_type
    | ( C_TEST_CREATE_CHEST_KEY , _  ) -> fail @@ error_type
    | ( (C_SAPLING_VERIFY_UPDATE | C_SAPLING_EMPTY_STATE) , _ ) ->
      fail @@ Errors.generic_error loc "Sapling is not supported."
    | ( (C_SOURCE | C_SENDER | C_AMOUNT | C_BALANCE | C_NOW | C_LEVEL | C_SELF |
         C_SELF_ADDRESS | C_CHAIN_ID | C_VOTING_POWER | C_TOTAL_VOTING_POWER) , _ ) ->
      fail @@ Errors.generic_error loc "Primitive not valid in testing mode."
    | ( C_POLYMORPHIC_ADD , _ ) ->
      fail @@ Errors.generic_error loc "POLYMORPHIC_ADD is solved in checking."
    | ( C_POLYMORPHIC_SUB , _ ) ->
      fail @@ Errors.generic_error loc "POLYMORPHIC_SUB is solved in checking."
    | ( (C_ASSERT_INFERRED | C_UPDATE | C_ITER |
         C_FOLD_LEFT | C_FOLD_RIGHT | C_EDIV | C_PAIR | C_CAR | C_CDR | C_LEFT | C_RIGHT |
         C_SET_LITERAL | C_LIST_LITERAL | C_MAP | C_MAP_LITERAL | C_MAP_GET | C_MAP_GET_FORCE |
         C_BIG_MAP | C_BIG_MAP_LITERAL | C_BIG_MAP_GET_AND_UPDATE | C_CALL | C_CONTRACT |
         C_CONTRACT_OPT | C_CONTRACT_WITH_ERROR | C_CONTRACT_ENTRYPOINT |
         C_CONTRACT_ENTRYPOINT_OPT | C_SET_DELEGATE |
         C_CREATE_CONTRACT | C_OPEN_CHEST | C_VIEW | C_TEST_COMPILE_CONTRACT | C_GLOBAL_CONSTANT) , _ ) ->
      fail @@ Errors.generic_error loc "Unbound primitive."
  )

(*interpreter*)
and eval_literal : AST.literal -> value Monad.t = function
  | Literal_unit           -> Monad.return @@ V_Ct (C_unit)
  | Literal_int i          -> Monad.return @@ V_Ct (C_int i)
  | Literal_nat n          -> Monad.return @@ V_Ct (C_nat n)
  | Literal_timestamp i    -> Monad.return @@ V_Ct (C_timestamp i)
  | Literal_string s       -> Monad.return @@ V_Ct (C_string (Ligo_string.extract s))
  | Literal_bytes s        -> Monad.return @@ V_Ct (C_bytes s)
  | Literal_mutez s        -> Monad.return @@ V_Ct (C_mutez s)
  | Literal_key_hash s     -> (
    match Tezos_crypto.Signature.Public_key_hash.of_b58check s with
    | Ok kh -> Monad.return @@ V_Ct (C_key_hash kh)
    | Error _ -> Monad.fail @@ Errors.literal Location.generated (Literal_key_hash s)
  )
  | Literal_key s          -> (
    match Tezos_crypto.Signature.Public_key.of_b58check s with
    | Ok k -> Monad.return @@ V_Ct (C_key k)
    | Error _ -> Monad.fail @@ Errors.literal Location.generated (Literal_key s)
  )
  | Literal_signature s    -> (
    match Tezos_crypto.Signature.of_b58check s with
    | Ok s -> Monad.return @@ V_Ct (C_signature s)
    | Error _ -> Monad.fail @@ Errors.literal Location.generated (Literal_signature s)
  )
  | Literal_address s      -> (
    match Tezos_protocol.Protocol.Alpha_context.Contract.of_b58check s with
    | Ok t -> Monad.return @@ V_Ct (C_address t)
    | Error _ -> Monad.fail @@ Errors.literal Location.generated (Literal_address s)
  )
  | Literal_bls12_381_g1 b -> (
    match Bls12_381.G1.of_bytes_opt b with
    | Some t -> Monad.return @@ V_Ct (C_bls12_381_g1 t)
    | None -> Monad.fail @@ Errors.literal Location.generated (Literal_bls12_381_g1 b)
  )
  | Literal_bls12_381_g2 b -> (
    match Bls12_381.G2.of_bytes_opt b with
    | Some t -> Monad.return @@ V_Ct (C_bls12_381_g2 t)
    | None -> Monad.fail @@ Errors.literal Location.generated (Literal_bls12_381_g2 b)
  )
  | Literal_bls12_381_fr b -> (
    match Bls12_381.Fr.of_bytes_opt b with
    | Some t -> Monad.return @@ V_Ct (C_bls12_381_fr t)
    | None -> Monad.fail @@ Errors.literal Location.generated (Literal_bls12_381_fr b)
  )
  | l -> Monad.fail @@ Errors.literal Location.generated l

and eval_ligo ~raise ~steps ~options : AST.expression -> calltrace -> env -> value Monad.t
  = fun term calltrace env ->
    let eval_ligo ?(steps = steps - 1) = eval_ligo ~raise ~steps ~options in
    let open Monad in
    let* () = if steps <= 0 then fail (Errors.meta_lang_eval term.location calltrace "Out of fuel") else return () in
    match term.expression_content with
    | E_type_inst _ ->
       fail @@ Errors.generic_error term.location "Polymorphism not supported: polymorphic expressions should be monomorphized before being interpreted. This could mean that the expression that you are trying to interpret is too generic, try adding a type annotation."
    | E_application {lamb = f; args} -> (
        let* f' = eval_ligo f calltrace env in
        let* args' = eval_ligo args calltrace env in
        match f' with
          | V_Func_val {arg_binder ; body ; env; rec_name = None ; orig_lambda } ->
            let AST.{ type1 = in_ty ; type2 = _ } = AST.get_t_arrow_exn orig_lambda.type_expression in
            let f_env' = Env.extend env arg_binder (in_ty, args') in
            eval_ligo body (term.location :: calltrace) f_env'
          | V_Func_val {arg_binder ; body ; env; rec_name = Some fun_name; orig_lambda} ->
            let AST.{ type1 = in_ty ; type2 = _ } = AST.get_t_arrow_exn orig_lambda.type_expression in
            let f_env' = Env.extend env arg_binder (in_ty, args') in
            let f_env'' = Env.extend f_env' fun_name (orig_lambda.type_expression, f') in
            eval_ligo body (term.location :: calltrace) f_env''
          | V_Ligo (_, code) ->
            let>> ctxt = Get_state () in
            return @@ Michelson_backend.parse_and_run_michelson_func ~raise ~loc:term.location ctxt code term.type_expression args' args.type_expression
          | V_Michelson (Ty_code { code ; code_ty = _ ; ast_ty = _ }) ->
            let>> ctxt = Get_state () in
            return @@ Michelson_backend.run_michelson_func ~raise ~loc:term.location ctxt code term.type_expression args' args.type_expression
          | _ -> fail @@ Errors.generic_error term.location "Trying to apply on something that is not a function?"
      )
    | E_lambda {binder; result;} ->
      return @@ V_Func_val {rec_name = None; orig_lambda = term ; arg_binder=binder ; body=result ; env}
    | E_type_abstraction {type_binder=_ ; result} -> (
      eval_ligo (result) calltrace env
    )
    | E_let_in {let_binder ; rhs; let_result; attr = { no_mutation ; inline=_ ; view=_ ; public=_}} -> (
      let* rhs' = eval_ligo rhs calltrace env in
      eval_ligo (let_result) calltrace (Env.extend env let_binder ~no_mutation (rhs.type_expression,rhs'))
    )
    | E_type_in {type_binder=_ ; rhs=_; let_result} -> (
      eval_ligo (let_result) calltrace env
    )
    | E_literal l ->
      eval_literal l
    | E_variable var ->
      let {eval_term=v ; _} = try fst (Option.value_exn (Env.lookup env var)) with _ -> (failwith (Format.asprintf "unbound variable: %a" AST.PP.expression_variable var)) in
      return v
    | E_record recmap ->
      let* lv' = Monad.bind_map_list
        (fun (label,(v:AST.expression)) ->
          let* v' = eval_ligo v calltrace env in
          return (label,v'))
        (LMap.to_kv_list_rev recmap)
      in
      return @@ V_Record (LMap.of_list lv')
    | E_record_accessor { record ; path} -> (
      let* record' = eval_ligo record calltrace env in
      match record' with
      | V_Record recmap ->
        let a = LMap.find path recmap in
        return a
      | _ -> failwith "trying to access a non-record"
    )
    | E_record_update {record ; path ; update} -> (
      let* record' = eval_ligo record calltrace env in
      match record' with
      | V_Record recmap ->
        if LMap.mem path recmap then
          let* field' = eval_ligo update calltrace env in
          return @@ V_Record (LMap.add path field' recmap)
        else
          failwith "field l does not exist in record"
      | _ -> failwith "this expression isn't a record"
    )
    | E_constant {cons_name ; arguments} -> (
      let* arguments' = Monad.bind_map_list
        (fun (ae:AST.expression) ->
          let* value = eval_ligo ae calltrace env in
          return @@ (value, ae.type_expression, ae.location))
        arguments in
      apply_operator ~raise ~steps ~options term.location calltrace term.type_expression env cons_name arguments'
    )
    | E_constructor { constructor = Label c ; element = { expression_content = E_literal (Literal_unit) ; _ } } when String.equal c "True" ->
      return @@ V_Ct (C_bool true)
    | E_constructor { constructor = Label c ; element = { expression_content = E_literal (Literal_unit) ; _ } } when String.equal c "False" ->
      return @@ V_Ct (C_bool false)
    | E_constructor { constructor = Label c ; element } ->
      let* v' = eval_ligo element calltrace env in
      return @@ V_Construct (c,v')
    | E_matching { matchee ; cases} -> (
      let* e' = eval_ligo matchee calltrace env in
      match cases, e' with
      | Match_variant {cases;_}, V_List [] ->
        let {constructor=_ ; pattern=_ ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal "Nil" c)
            cases in
        eval_ligo body calltrace env
      | Match_variant {cases;tv}, V_List lst ->
        let {constructor=_ ; pattern ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal "Cons" c)
            cases in
        let ty = AST.get_t_list_exn tv in
        let hd = List.hd_exn lst in
        let tl = V_List (List.tl_exn lst) in
        let proj = v_pair (hd,tl) in
        let env' = Env.extend env pattern (ty, proj) in
        eval_ligo body calltrace env'
      | Match_variant {cases;_}, V_Ct (C_bool b) ->
        let ctor_body (case : matching_content_case) = (case.constructor, case.body) in
        let cases = LMap.of_list (List.map ~f:ctor_body cases) in
        let get_case c =
            (LMap.find (Label c) cases) in
        let match_true  = get_case "True" in
        let match_false = get_case "False" in
        if b then eval_ligo match_true calltrace env
        else eval_ligo match_false calltrace env
      | Match_variant {cases ; tv} , V_Construct (matched_c , proj) ->
        let* tv = match AST.get_t_sum_opt tv with
          | Some tv ->
             let {associated_type; michelson_annotation=_; decl_pos=_} = LMap.find
                                  (Label matched_c) tv.content in
             return associated_type
          | None ->
             match AST.get_t_option tv with
             | Some tv -> return tv
             | None ->
                fail @@
                  (Errors.generic_error tv.location "Expected sum") in
        let {constructor=_ ; pattern ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal matched_c c)
            cases in
        (* TODO-er: check *)
        let env' = Env.extend env pattern (tv, proj) in
        eval_ligo body calltrace env'
      | Match_record {fields ; body ; tv = _} , V_Record rv ->
        let aux : label -> ( expression_variable * _ ) -> env -> env =
          fun l (v,ty) env ->
            let iv = match LMap.find_opt l rv with
              | Some x -> x
              | None -> failwith "label do not match"
            in
            Env.extend env v (ty,iv)
        in
        let env' = LMap.fold aux fields env in
        eval_ligo body calltrace env'
      | _ , v -> failwith ("not yet supported case "^ Format.asprintf "%a" Ligo_interpreter.PP.pp_value v^ Format.asprintf "%a" AST.PP.expression term)
    )
    | E_recursive {fun_name; fun_type=_; lambda} ->
      return @@ V_Func_val { rec_name = Some fun_name ;
                             orig_lambda = term ;
                             arg_binder = lambda.binder ;
                             body = lambda.result ;
                             env = env }
    | E_raw_code {language ; code} -> (
      let open AST in
      match code.expression_content with
      | E_literal (Literal_string x) when String.equal language Stage_common.Backends.michelson && (is_t_arrow (get_type code) || is_t_arrow (term.type_expression)) ->
        let ast_ty = get_type code in
        let exp_as_string = Ligo_string.extract x in
        let code, code_ty = Michelson_backend.parse_raw_michelson_code ~raise exp_as_string ast_ty in
        return @@ V_Michelson (Ty_code { code ; code_ty ; ast_ty })
      | E_literal (Literal_string x) when is_t_arrow (get_type term) ->
        let exp_as_string = Ligo_string.extract x in
        return @@ V_Ligo (language , exp_as_string)
      | _ -> raise.raise @@ Errors.generic_error term.location "Embedded raw code can only have a functional type"
    )

and try_eval ~raise ~steps ~options expr env state r = Monad.eval ~raise ~options (eval_ligo ~raise ~steps ~options expr [] env) state r

let eval_test ~raise ~steps ~options : Ast_typed.program -> ((string * value) list) =
  fun prg ->
  let decl_lst = prg in
  (* Pass over declarations, for each "test"-prefixed one, add a new
     declaration and in the end, gather all of them together *)
  let aux decl r =
    let ds, defs = r in
    match decl.Location.wrap_content with
    | Ast_typed.Declaration_constant { binder ; expr ; _ } ->
      let var = binder.var in
      if not (ValueVar.is_generated var) && (Base.String.is_prefix (ValueVar.to_name_exn var) ~prefix:"test") then
        let expr = Ast_typed.(e_a_variable var expr.type_expression) in
        (* TODO: check that variables are unique, as they are ignored *)
        decl :: ds, (binder, expr.type_expression) :: defs
      else
        decl :: ds, defs
    | _ -> decl :: ds, defs in
  let decl_lst, lst = List.fold_right ~f:aux ~init:([], []) decl_lst in
  (* Compile new context *)
  let ctxt = Ligo_compile.Of_typed.compile_program ~raise decl_lst in
  let initial_state = Tezos_state.init_ctxt ~raise options.Compiler_options.backend.protocol_version [] in
  let f (n, t) r =
    let s, _ = ValueVar.internal_get_name_and_counter n.var in
    LMap.add (Label s) (Ast_typed.e_a_variable n.var t) r in
  let map = List.fold_right lst ~f ~init:LMap.empty in
  let expr = Ast_typed.e_a_record map in
  let expr = ctxt expr in
  let expr = Self_ast_aggregated.expression_mono expr in
  let value, _ = try_eval ~raise ~steps ~options expr Env.empty_env initial_state None in
  match value with
  | V_Record m ->
    let f (n, _) r =
      let s, _ = ValueVar.internal_get_name_and_counter n.var in
      match LMap.find_opt (Label s) m with
      | None -> failwith "Cannot find"
      | Some v -> (s, v) :: r in
    List.fold_right ~f ~init:[] @@ lst
  | _ -> failwith "Not a tuple?"

let () = Printexc.record_backtrace true
