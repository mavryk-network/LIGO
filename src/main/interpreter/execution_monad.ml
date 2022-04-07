open Simple_utils.Trace
(*
  That monad do not seem very useful now,
  but it could become useful if we want to support multiple testing mode (against node, or memory)
*)


module LT = Ligo_interpreter.Types
module LC = Ligo_interpreter.Combinators
module Exc = Ligo_interpreter_exc
module Tezos_protocol = Tezos_protocol_012_Psithaca
module Tezos_client = Tezos_client_012_Psithaca

module Location = Simple_utils.Location
module ModRes = Preprocessor.ModRes

open Errors
type execution_trace = unit

type state = {
  tezos_context : Tezos_state.context ;
  mod_res       : ModRes.t option ; 
}

let make_state ~raise ~(options : Compiler_options.t)  =
  let tezos_context = Tezos_state.init_ctxt ~raise options.backend.protocol_version [] in
  let mod_res       = Option.bind ~f:ModRes.make options.frontend.project_root in
  { tezos_context ; mod_res }

let add_warning _ = ()

let clean_locations ty = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations ty)

(* Command should _only_ contains instruction that needs or modify the tezos context *)
module Command = struct
  type 'a t =
    | Set_big_map : Z.t * (LT.value * LT.value) list * Ast_aggregated.type_expression -> unit t
    | Pack : Location.t * LT.value * Ast_aggregated.type_expression -> LT.value t
    | Unpack : Location.t * bytes * Ast_aggregated.type_expression -> LT.value t
    | Bootstrap_contract : int * LT.value * LT.value * Ast_aggregated.type_expression  -> unit t
    | Nth_bootstrap_contract : int -> Tezos_protocol.Protocol.Alpha_context.Contract.t t
    | Nth_bootstrap_typed_address : Location.t * int -> (Tezos_protocol.Protocol.Alpha_context.Contract.t * Ast_aggregated.type_expression * Ast_aggregated.type_expression) t
    | Reset_state : Location.t * LT.calltrace * LT.value * LT.value -> unit t
    | Get_state : unit -> Tezos_state.context t
    | Get_mod_res : unit -> ModRes.t option t
    | External_call : Location.t * Ligo_interpreter.Types.calltrace * LT.contract * (execution_trace, string) Tezos_micheline.Micheline.node * Z.t
      -> [`Exec_failed of Tezos_state.state_error | `Exec_ok of Z.t] t
    | State_error_to_value : Tezos_state.state_error -> LT.value t
    | Get_storage : Location.t * Ligo_interpreter.Types.calltrace * LT.value * Ast_aggregated.type_expression -> LT.value t
    | Get_storage_of_address : Location.t * Ligo_interpreter.Types.calltrace * LT.value -> LT.value t
    | Get_size : LT.value -> LT.value t
    | Get_balance : Location.t * Ligo_interpreter.Types.calltrace * LT.value -> LT.value t
    | Get_last_originations : unit -> LT.value t
    | Check_obj_ligo : LT.expression -> unit t
    | Compile_contract_from_file : string * string * string list -> (LT.value * LT.value) t
    | Run : Location.t * LT.func_val * LT.value -> LT.value t
    | Eval : Location.t * LT.value * Ast_aggregated.type_expression -> LT.value t
    | Compile_contract : Location.t * LT.value * Ast_aggregated.type_expression -> LT.value t
    | Decompile : LT.mcode * LT.mcode * Ast_aggregated.type_expression -> LT.value t
    | To_contract : Location.t * LT.value * string option * Ast_aggregated.type_expression -> LT.value t
    | Check_storage_address : Location.t * Tezos_protocol.Protocol.Alpha_context.Contract.t * Ast_aggregated.type_expression -> unit t
    | Inject_script : Location.t * Ligo_interpreter.Types.calltrace * LT.value * LT.value * Z.t -> LT.value t
    | Set_source : LT.value -> unit t
    | Set_baker : LT.value -> unit t
    | Get_voting_power : Location.t * Ligo_interpreter.Types.calltrace * Tezos_protocol.Protocol.Alpha_context.public_key_hash -> LT.value t
    | Get_total_voting_power : Location.t * Ligo_interpreter.Types.calltrace -> LT.value t
    | Get_bootstrap : Location.t * LT.value -> LT.value t
    (* TODO : move them ou to here *)
    | Michelson_equal : Location.t * LT.value * LT.value -> bool t
    | Sha256 : bytes -> LT.value t
    | Sha512 : bytes -> LT.value t
    | Blake2b : bytes -> LT.value t
    | Keccak : bytes -> LT.value t
    | Sha3 : bytes -> LT.value t
    | Hash_key : Tezos_protocol.Protocol.Alpha_context.public_key -> LT.value t
    | Implicit_account : Location.t * Tezos_protocol.Protocol.Alpha_context.public_key_hash -> LT.value t
    | Check_signature : Tezos_protocol.Protocol.Alpha_context.public_key * Tezos_protocol.Protocol.Alpha_context.signature * bytes -> LT.value t
    | Pairing_check : (Bls12_381.G1.t * Bls12_381.G2.t) list -> LT.value t
    | Add_account : Location.t * string * Tezos_protocol.Protocol.Alpha_context.public_key -> unit t
    | New_account : unit -> LT.value t
    | Baker_account : LT.value * LT.value -> unit t
    | Register_delegate : Location.t * Ligo_interpreter.Types.calltrace *  Tezos_protocol.Protocol.Alpha_context.public_key_hash -> LT.value t
    | Bake_until_n_cycle_end : Location.t * Ligo_interpreter.Types.calltrace *  Z.t -> LT.value t
    | Register_constant : Location.t * Ligo_interpreter.Types.calltrace * LT.mcode -> string t
    | Constant_to_Michelson : Location.t * Ligo_interpreter.Types.calltrace * string -> LT.mcode t

  let eval
    : type a.
      raise:Errors.interpreter_error raise ->
      options:Compiler_options.t ->
      a t ->
      state ->
      execution_trace ref option ->
      (a * Tezos_state.context)
    = fun ~raise ~options command state _log ->
    let ctxt = state.tezos_context in
    match command with
    | Set_big_map (id, kv, bigmap_ty) ->
      let (k_ty, v_ty) = trace_option ~raise (Errors.generic_error bigmap_ty.location "Expected big_map type") @@
                           Ast_aggregated.get_t_big_map bigmap_ty in
      let k_ty = Michelson_backend.compile_type ~raise k_ty in
      let v_ty = Michelson_backend.compile_type ~raise v_ty in
      let ctxt = Tezos_state.set_big_map ~raise ctxt (Z.to_int id) kv k_ty v_ty in
      ((), ctxt)
    | Pack (loc, value, value_ty) ->
      let expr = Michelson_backend.val_to_ast ~raise ~loc value value_ty in
      let expr = Ast_aggregated.e_a_pack expr in
      let mich = Michelson_backend.compile_value ~raise ~options expr in
      let ret_co, ret_ty = Michelson_backend.run_expression_unwrap ~raise ~ctxt ~loc mich in
      let ret = Michelson_to_value.decompile_to_untyped_value ~raise ~bigmaps:ctxt.transduced.bigmaps ret_ty ret_co in
      (ret, ctxt)
    | Unpack (loc, bytes, value_ty) ->
      let value_ty = trace_option ~raise (Errors.generic_error loc "Expected return type is not an option" ) @@ Ast_aggregated.get_t_option value_ty in
      let expr = Ast_aggregated.(e_a_unpack (e_a_bytes bytes) value_ty) in
      let mich = Michelson_backend.compile_value ~raise ~options expr in
      let (ret_co, ret_ty) = Michelson_backend.run_expression_unwrap ~raise ~ctxt ~loc mich in
      let ret = Michelson_to_value.decompile_to_untyped_value ~raise ~bigmaps:ctxt.transduced.bigmaps ret_ty ret_co in
      (ret, ctxt)
    | Nth_bootstrap_contract (n) ->
      let contract = Tezos_state.get_bootstrapped_contract ~raise n in
      (contract,ctxt)
    | Nth_bootstrap_typed_address (loc, n) ->
      let contract = Tezos_state.get_bootstrapped_contract ~raise n in
      let storage_ty =
        trace_option ~raise (Errors.generic_error loc "Storage type not available" ) @@
          List.Assoc.find ~equal:(Tezos_state.equal_account) ctxt.internals.storage_tys contract in
      let parameter_ty =
        trace_option ~raise (Errors.generic_error loc "Parameter type not available" ) @@
          List.Assoc.find ~equal:(Tezos_state.equal_account) ctxt.internals.parameter_tys contract in
      let contract = Tezos_state.get_bootstrapped_contract ~raise n in
      ((contract, parameter_ty, storage_ty),ctxt)
    | Bootstrap_contract (mutez, contract, storage, contract_ty) ->
      let contract = trace_option ~raise (corner_case ()) @@ LC.get_michelson_contract contract in
      let Ast_aggregated.{ type1 = input_ty ; type2 = _ } = trace_option ~raise (corner_case ()) @@ Ast_aggregated.get_t_arrow contract_ty in
      let parameter_ty, _ = trace_option ~raise (corner_case ()) @@ Ast_aggregated.get_t_pair input_ty in
      let { code = storage ; ast_ty = storage_ty ; _ } : LT.typed_michelson_code =
        trace_option ~raise (corner_case ()) @@ LC.get_michelson_expr storage in
      let next_bootstrapped_contracts = (mutez, contract, storage, parameter_ty, storage_ty) :: ctxt.internals.next_bootstrapped_contracts in
      let ctxt = { ctxt with internals = { ctxt.internals with next_bootstrapped_contracts } } in
      ((),ctxt)
    | Reset_state (loc,calltrace,n,amts) ->
      let amts = trace_option ~raise (corner_case ()) @@ LC.get_list amts in
      let amts = List.map ~f:
        (fun x ->
          let x = trace_option ~raise (corner_case ()) @@ LC.get_mutez x in
          (Z.to_int64 x) )
        amts
      in
      let n = trace_option ~raise (corner_case ()) @@ LC.get_nat n in
      let bootstrap_contracts = List.rev ctxt.internals.next_bootstrapped_contracts in
      let baker_accounts = List.rev ctxt.internals.next_baker_accounts in
      let ctxt = Tezos_state.init_ctxt
        ~raise ~loc ~calltrace ~initial_balances:amts ~n:(Z.to_int n)
        ctxt.internals.protocol_version bootstrap_contracts ~baker_accounts
      in
      ((),ctxt)
    | Get_state () ->
      (ctxt,ctxt)
    | Get_mod_res () -> 
      (state.mod_res,ctxt)
    | External_call (loc, calltrace, { address; entrypoint }, param, amt) -> (
      let x = Tezos_state.transfer ~raise ~loc ~calltrace ctxt address ?entrypoint param amt in
      match x with
      | Success (ctxt',gas_consumed) ->
        (`Exec_ok gas_consumed, ctxt')
      | Fail errs -> (`Exec_failed errs, ctxt)
    )
    | State_error_to_value errs -> (
      let open Tezos_protocol.Protocol in
      let open Environment in
      let fail_ctor arg = LC.v_ctor "Fail" arg in
      let fail_other () =
        let errs_as_str =
          Format.asprintf "%a"
            (Tezos_client.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs
        in
        let rej = LC.v_ctor "Other" (LC.v_string errs_as_str) in
        fail_ctor rej
      in
      match errs with
      | Ecoproto_error (Script_interpreter.Runtime_contract_error contract_failing) :: rest -> (
        let contract_failing = LT.V_Ct (C_address contract_failing) in
        match rest with
        | Ecoproto_error (Script_interpreter.Reject (_,x,_)) :: _ -> (
          let code = Tezos_state.canonical_to_ligo x in
          let code_ty = Michelson_backend.storage_retreival_dummy_ty in
          let v = LT.V_Michelson (Ty_code { code ; code_ty ; ast_ty = Ast_aggregated.t_int () }) in
          let rej = LC.v_ctor "Rejected" (LC.v_pair (v,contract_failing)) in
          (fail_ctor rej, ctxt)
        )
        | Ecoproto_error (Script_interpreter.Bad_contract_parameter _addr) :: _ -> (
          (fail_other () , ctxt)
        )
        | _ -> 
          (fail_other (), ctxt)
      )
      | (Ecoproto_error (Contract_storage.Balance_too_low (contract_too_low,contract_balance,spend_request))) :: _ -> (
        let contract_too_low : LT.mcontract = Michelson_backend.contract_to_contract contract_too_low in
        let contract_too_low = LT.V_Ct (C_address contract_too_low) in
        let contract_balance,spend_request =
          let contract_balance = Michelson_backend.tez_to_z contract_balance in
          let spend_request = Michelson_backend.tez_to_z spend_request in
          LT.V_Ct (C_mutez contract_balance), LT.V_Ct (C_mutez spend_request)
        in
        let rej_data = LC.v_record [ ("contract_too_low",contract_too_low) ; ("contract_balance",contract_balance)  ; ("spend_request",spend_request)] in
        let rej = LC.v_ctor "Balance_too_low" rej_data in
        (fail_ctor rej, ctxt)
      )
      | _ -> (fail_other () , ctxt)
    )
    | Get_storage (loc, calltrace, addr, ty_expr) ->
      let addr = trace_option ~raise (corner_case ()) @@ LC.get_address addr in
      let (storage',ty) = Tezos_state.get_storage ~raise ~loc ~calltrace ctxt addr in
      let storage = storage'
        |> Tezos_protocol.Protocol.Michelson_v1_primitives.strings_of_prims
        |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())
      in
      let ret = Michelson_to_value.decompile_to_untyped_value ~raise ~bigmaps:ctxt.transduced.bigmaps ty storage in
      let ret = Michelson_to_value.decompile_value ~raise ~bigmaps:ctxt.transduced.bigmaps ret ty_expr in
      (ret, ctxt)
    | Get_balance (loc, calltrace, addr) ->
      let addr = trace_option ~raise (corner_case ()) @@ LC.get_address addr in
      let balance = Tezos_state.get_balance ~raise ~loc ~calltrace ctxt addr in
      let mutez = Michelson_backend.int_of_mutez balance in
      let balance = LT.V_Ct (C_mutez mutez) in
      (balance, ctxt)
    | Get_storage_of_address (loc, calltrace, addr) ->
      let addr = trace_option ~raise (corner_case ()) @@ LC.get_address addr in
      let (storage',ty) = Tezos_state.get_storage ~raise ~loc ~calltrace ctxt addr in
      let storage = storage'
        |> Tezos_protocol.Protocol.Michelson_v1_primitives.strings_of_prims
        |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())
      in
      let ast_ty =
        trace_option ~raise (Errors.generic_error loc "Not supported (yet) when the provided account has been fetched from Test.get_last_originations" ) @@
          List.Assoc.find ~equal:(Tezos_state.equal_account) ctxt.internals.storage_tys addr
      in
      let ret = LT.V_Michelson (Ty_code { code = storage ; code_ty = ty ; ast_ty }) in
      (ret, ctxt)
    | Check_obj_ligo e ->
      let _ = trace ~raise Main_errors.self_ast_aggregated_tracer @@ Self_ast_aggregated.expression_obj e in
      ((), ctxt)
    | Get_size (contract_code) -> (
      match contract_code with
      | LT.V_Michelson (LT.Contract contract_code) ->
         let s = Ligo_compile.Of_michelson.measure ~raise contract_code in
         (LT.V_Ct (C_int (Z.of_int s)), ctxt)
      | _ -> raise.raise @@ Errors.generic_error Location.generated
                              "Trying to measure a non-contract"
    )
    | Compile_contract_from_file (source_file, entry_point, views) ->
      let options = Compiler_options.set_entry_point options entry_point in
      let options = Compiler_options.set_views options views in
      let contract_code =
        Michelson_backend.compile_contract ~raise ~add_warning ~options source_file entry_point views in
      let size =
        let s = Ligo_compile.Of_michelson.measure ~raise contract_code in
        LT.V_Ct (C_int (Z.of_int s))
      in
      let contract_code = Tezos_micheline.Micheline.(inject_locations (fun _ -> ()) (strip_locations contract_code)) in
      let contract = LT.V_Michelson (LT.Contract contract_code) in
      ((contract,size), ctxt)
    | Run (loc, f, v) ->
      let open Ligo_interpreter.Types in
      let subst_lst = Michelson_backend.make_subst_ast_env_exp ~raise f.env f.orig_lambda in
      let Ast_aggregated.{ type1 = in_ty ; type2 = out_ty } = trace_option ~raise (Errors.generic_error loc "Trying to run a non-function?") @@
                            Ast_aggregated.get_t_arrow f.orig_lambda.type_expression in
      let func_typed_exp = Michelson_backend.make_function in_ty out_ty f.arg_binder f.body subst_lst in
      let _ = trace ~raise Main_errors.self_ast_aggregated_tracer @@ Self_ast_aggregated.expression_obj func_typed_exp in
      let options = Compiler_options.make ~raw_options:Compiler_options.default_raw_options () in
      let func_code = Michelson_backend.compile_value ~raise ~options func_typed_exp in
      let { code = arg_code ; _ } = Michelson_backend.compile_simple_value ~raise ~ctxt ~loc v in_ty in
      let input_ty,_ = Ligo_run.Of_michelson.fetch_lambda_types ~raise func_code.expr_ty in
      let options = Michelson_backend.make_options ~raise ~param:input_ty (Some ctxt) in
      let runres = Ligo_run.Of_michelson.run_function ~raise ~options func_code.expr func_code.expr_ty arg_code in
      let (expr_ty,expr) = match runres with | Success x -> x | Fail x -> raise.raise @@ Errors.target_lang_failwith loc x in
      let expr, expr_ty =
        clean_locations expr, clean_locations expr_ty in
      let ret = LT.V_Michelson (Ty_code { code = expr ; code_ty = expr_ty ; ast_ty = f.body.type_expression }) in
      (ret, ctxt)
    | Eval (loc, v, expr_ty) ->
      let value = Michelson_backend.compile_simple_value ~raise ~ctxt ~loc v expr_ty in
      (LT.V_Michelson (Ty_code value), ctxt)
    | Compile_contract (loc, v, _ty_expr) ->
       let compiled_expr, compiled_expr_ty = match v with
         | LT.V_Func_val { arg_binder ; body ; orig_lambda ; env ; rec_name } ->
            let subst_lst = Michelson_backend.make_subst_ast_env_exp ~raise env orig_lambda in
            let Ast_aggregated.{ type1 = in_ty ; type2 = out_ty } =
              trace_option ~raise (Errors.generic_error loc "Trying to run a non-function?") @@
                Ast_aggregated.get_t_arrow orig_lambda.type_expression in
            let compiled_expr =
              Michelson_backend.compile_contract_ ~raise ~options subst_lst arg_binder rec_name in_ty out_ty body in
            let expr = clean_locations compiled_expr.expr in
            (* TODO-er: check the ignored second component: *)
            let expr_ty = clean_locations compiled_expr.expr_ty in
            (expr, expr_ty)
         | _ ->
            raise.raise @@ Errors.generic_error loc "Contract does not reduce to a function value?" in
        let (param_ty, storage_ty) =
        match Self_michelson.fetch_contract_ty_inputs compiled_expr_ty with
        | Some (param_ty, storage_ty) -> (param_ty, storage_ty)
        | _ -> raise.raise @@ Errors.generic_error loc "Compiled expression has not the correct input of contract" in
      let open Tezos_utils in
      let param_ty = clean_locations param_ty in
      let storage_ty = clean_locations storage_ty in
      let expr = clean_locations compiled_expr in
      let contract = Michelson.contract param_ty storage_ty expr [] in
      (LT.V_Michelson (Contract contract), ctxt)
    | Decompile (code, code_ty, ast_ty) ->
      let ret = Michelson_to_value.decompile_to_untyped_value ~raise ~bigmaps:ctxt.transduced.bigmaps code_ty code in
      let ret = Michelson_to_value.decompile_value ~raise ~bigmaps:ctxt.transduced.bigmaps ret ast_ty in
      (ret, ctxt)
    | To_contract (loc, v, entrypoint, _ty_expr) -> (
      match v with
      | LT.V_Ct (LT.C_address address) ->
         let contract : LT.constant_val =
           LT.C_contract { address ; entrypoint } in
         (LT.V_Ct contract, ctxt)
      | _ ->
         raise.raise @@ Errors.generic_error loc
                          "Should be caught by the typer"
    )
    | Check_storage_address (loc, addr, ty) ->
      let ligo_ty =
        trace_option ~raise (Errors.generic_error loc "Not supported (yet) when the provided account has been fetched from Test.get_last_originations" ) @@
          List.Assoc.find ~equal:(Tezos_state.equal_account) ctxt.internals.storage_tys addr in
      let _,ty = trace_option ~raise (Errors.generic_error loc "Argument expected to be a typed_address" ) @@
                    Ast_aggregated.get_t_typed_address ty in
      let () = trace_option ~raise (Errors.generic_error loc "Storage type does not match expected type") @@
          (Ast_aggregated.Helpers.assert_type_expression_eq (ligo_ty, ty)) in
      ((), ctxt)
    | Inject_script (loc, calltrace, code, storage, amt) ->
      Tezos_state.originate_contract ~raise ~loc ~calltrace ctxt (code, storage) amt
    | Set_source source ->
      let source = trace_option ~raise (corner_case ()) @@ LC.get_address source in
      ((), {ctxt with internals = { ctxt.internals with source }})
    | Set_baker baker ->
      let baker = trace_option ~raise (corner_case ()) @@ LC.get_address baker in
      ((), {ctxt with internals = { ctxt.internals with baker }})
    | Get_voting_power (loc, calltrace, key_hash) ->
      let vp = Tezos_state.get_voting_power ~raise ~loc ~calltrace ctxt key_hash in
      ((LT.V_Ct (LT.C_nat (Z.of_int32 vp))), ctxt)
    | Get_total_voting_power (loc, calltrace) ->
      let tvp = Tezos_state.get_total_voting_power ~raise ~loc ~calltrace ctxt in
      ((LT.V_Ct (LT.C_nat (Z.of_int32 tvp))), ctxt)
    | Get_bootstrap (loc,x) -> (
      let x = trace_option ~raise (corner_case ()) @@ LC.get_int x in
      match List.nth ctxt.internals.bootstrapped (Z.to_int x) with
      | Some x -> (LT.V_Ct (C_address x), ctxt)
      | None -> raise.raise (Errors.generic_error loc "This bootstrap account do not exist")
    )
    | Michelson_equal (loc,a,b) ->
      let { code ; _ } : LT.typed_michelson_code = trace_option ~raise (Errors.generic_error loc "Can't compare contracts") @@
        LC.get_michelson_expr a in
      let { code = code' ; _ } : LT.typed_michelson_code = trace_option ~raise (Errors.generic_error loc "Can't compare contracts") @@
        LC.get_michelson_expr b in
      (Caml.(=) code code', ctxt)
    | Get_last_originations () ->
      let aux (src, lst) =
        let src = LC.v_address src in
        let lst = LT.V_List (List.map ~f:LC.v_address lst) in
        (src, lst)
      in
      let v = LT.V_Map (List.map ~f:aux ctxt.transduced.last_originations) in
      (v,ctxt)
    | Sha256 b -> (
      let b = Tezos_protocol.Protocol.Environment.Raw_hashes.sha256 b in
      let v = LT.V_Ct (LT.C_bytes b) in
      (v, ctxt)
    )
    | Sha512 b -> (
      let b = Tezos_protocol.Protocol.Environment.Raw_hashes.sha512 b in
      let v = LT.V_Ct (LT.C_bytes b) in
      (v, ctxt)
    )
    | Blake2b b -> (
      let b = Tezos_protocol.Protocol.Environment.Raw_hashes.blake2b b in
      let v = LT.V_Ct (LT.C_bytes b) in
      (v, ctxt)
    )
    | Keccak b -> (
      let b = Tezos_protocol.Protocol.Environment.Raw_hashes.keccak256 b in
      let v = LT.V_Ct (LT.C_bytes b) in
      (v, ctxt)
    )
    | Sha3 b -> (
      let b = Tezos_protocol.Protocol.Environment.Raw_hashes.sha3_256 b in
      let v = LT.V_Ct (LT.C_bytes b) in
      (v, ctxt)
    )
    | Hash_key k -> (
      let kh = Tezos_protocol.Protocol.Environment.Signature.Public_key.hash k in
      let v = LT.V_Ct (LT.C_key_hash kh) in
      (v, ctxt)
    )
    | Implicit_account (loc, kh) -> (
      let address = Tezos_protocol.Protocol.Environment.Signature.Public_key_hash.to_b58check kh in
      let address = Tezos_state.implicit_account ~raise ~loc address in
      let v = LT.V_Ct (LT.C_contract { address ; entrypoint = None }) in
      (v, ctxt)
    )
    | Check_signature (k, s, b) -> (
      let b = Tezos_protocol.Protocol.Environment.Signature.check k s b in
      let v = LC.v_bool b in
      (v, ctxt)
    )
    | Pairing_check l -> (
      let check = match l with
        | [] -> true
        | pairs ->
           Bls12_381.(
               Pairing.miller_loop pairs |> Pairing.final_exponentiation_opt
               |> Option.map ~f:Fq12.(eq one))
           |> Option.value ~default:false in
      (LC.v_bool check, ctxt)
    )
    | Add_account (loc, sk, pk) -> (
      let pkh = Tezos_protocol.Protocol.Environment.Signature.Public_key.hash pk in
      Tezos_state.add_account ~raise ~loc sk pk pkh;
      ((), ctxt)
    )
    | New_account () -> (
      let (sk, pk) = Tezos_state.new_account () in
      let value = LC.v_pair ((V_Ct (C_string sk)), (V_Ct (C_key pk))) in
      (value, ctxt)
    )
    | Baker_account (acc, opt) -> (
      let tez = trace_option ~raise (corner_case ()) @@ LC.get_option opt in
      let tez = Option.map ~f:(fun v -> trace_option ~raise (corner_case ()) @@ LC.get_mutez v) tez in
      let tez = Option.map ~f:(fun t -> Z.to_int64 t) tez in
      let sk, pk = trace_option ~raise (corner_case ()) @@ LC.get_pair acc in
      let sk = trace_option ~raise (corner_case ()) @@ LC.get_string sk in
      let pk = trace_option ~raise (corner_case ()) @@ LC.get_key pk in
      let next_baker_accounts = (sk, pk, tez) :: ctxt.internals.next_baker_accounts in
      let ctxt = { ctxt with internals = { ctxt.internals with next_baker_accounts } } in
      ((),ctxt)
    )
    | Register_delegate (loc, calltrace, pkh) -> (
      let ctxt = Tezos_state.register_delegate ~raise ~loc ~calltrace ctxt pkh in
      let value = LC.v_unit () in
      (value, ctxt)
    )
    | Bake_until_n_cycle_end (loc, calltrace, n) -> (
      let ctxt = Tezos_state.bake_until_n_cycle_end ~raise ~loc ~calltrace ctxt (Z.to_int n) in
      let value = LC.v_unit () in
      (value, ctxt)
    )
    | Register_constant (loc, calltrace, code) -> (
      let (hash, ctxt) = Tezos_state.register_constant ~raise ~loc ~calltrace ~source:ctxt.internals.source ~value:code ctxt in
      (hash, ctxt)
    )
    | Constant_to_Michelson (loc, calltrace, code) -> (
      let code = Tezos_state.parse_constant ~raise ~loc ~calltrace code in
      (code, ctxt)
    )
end

type 'a t =
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Call : 'a Command.t -> 'a t
  | Return : 'a -> 'a t
  | Fail_ligo : Errors.interpreter_error -> 'a t
  | Try_or : 'a t * 'a t -> 'a t

let rec eval
  : type a.
    raise:Errors.interpreter_error raise ->
    options:Compiler_options.t ->  
    a t ->
    state ->
    execution_trace ref option ->
    a * Tezos_state.context
  = fun ~raise ~options e state log ->
  match e with
  | Bind (e', f) ->
    let (v, tezos_context) = eval ~raise ~options e' state log in
    let state = { state with tezos_context } in
    eval ~raise ~options (f v) state log
  | Call command -> Command.eval ~raise ~options command state log
  | Return v -> (v, state.tezos_context)
  | Fail_ligo err -> raise.raise err
  | Try_or (e', handler) ->
    try_with
      (eval ~options e' state log)
      (function
            `Main_interpret_target_lang_error _
          | `Main_interpret_target_lang_failwith _
          | `Main_interpret_meta_lang_eval _
          | `Main_interpret_meta_lang_failwith _ ->
            eval ~raise ~options handler state log
          | e -> raise.raise e)

let fail err : 'a t = Fail_ligo err
let return (x: 'a) : 'a t = Return x
let call (command : 'a Command.t) : 'a t = Call command
let try_or (c : 'a t) (handler : 'a t) : 'a t = Try_or (c, handler)
let ( let>> ) o f = Bind (call o, f)
let ( let* ) o f = Bind (o, f)

let rec bind_list = function
  | [] -> return []
  | hd::tl ->
    let* hd = hd in
    let* tl = bind_list tl in
    return @@ hd :: tl

let bind_map_list f lst = bind_list (List.map ~f:f lst)

let bind_fold_list f init lst =
  let aux x y =
    let* x = x in
    f x y
  in
  List.fold_left ~f:aux ~init:(return init) lst

let bind_fold_right_list f init lst =
  let aux y x =
    let* x = x in
    f y x
  in
  List.fold_right ~f:aux ~init:(return init) lst

let rec iter_while f lst =
  match lst with
  | [] ->
     return None
  | (x :: xs) ->
     let* b = f x in
     match b with
     | None ->
        iter_while f xs
     | Some x ->
        return (Some x)