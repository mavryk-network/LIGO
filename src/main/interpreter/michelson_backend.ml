module Location = Simple_utils.Location
module Var      = Simple_utils.Var
open Simple_utils.Trace
open Simple_utils.Option

module Tezos_protocol = Tezos_protocol_011_PtHangz2

let int_of_mutez t = Z.of_int64 @@ Memory_proto_alpha.Protocol.Alpha_context.Tez.to_mutez t
let string_of_contract t = Format.asprintf "%a" Tezos_protocol.Protocol.Alpha_context.Contract.pp t
let string_of_key_hash t = Format.asprintf "%a" Tezos_crypto.Signature.Public_key_hash.pp t
let string_of_key t = Format.asprintf "%a" Tezos_crypto.Signature.Public_key.pp t
let string_of_signature t = Format.asprintf "%a" Tezos_crypto.Signature.pp t
let bytes_of_bls12_381_g1 t = Bls12_381.G1.to_bytes t
let bytes_of_bls12_381_g2 t = Bls12_381.G2.to_bytes t
let bytes_of_bls12_381_fr t = Bls12_381.Fr.to_bytes t

module Tezos_eq = struct
  (* behavior should be equivalent to the one in the tezos codebase *)
  let nat_shift_left x y =
    if Z.compare y (Z.of_int 256) > 0 then None
    else
      let y = Z.to_int y in
      Some (Z.shift_left x y)

  let nat_shift_right x y =
    if Z.compare y (Z.of_int 256) > 0 then None
    else
      let y = Z.to_int y in
      Some (Z.shift_right x y)

  let int_ediv x y =
      try
        let (q, r) = Z.ediv_rem x y in
        Some (q, r)
      with _ -> None

  let timestamp_add : Z.t -> Z.t-> Z.t =
    fun tz n ->
      let open Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp in
      let t = of_zint tz in
      add_delta t (Memory_proto_alpha.Protocol.Alpha_context.Script_int.of_zint n) |> to_zint

  let timestamp_sub : Z.t -> Z.t-> Z.t =
    fun tz n ->
      let open Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp in
      let t = of_zint tz in
      add_delta t (Memory_proto_alpha.Protocol.Alpha_context.Script_int.of_zint n) |> to_zint

  let mutez_add : Z.t -> Z.t -> Z.t option = fun x y ->
    let open Memory_proto_alpha.Protocol.Alpha_context.Tez in
    let open Option in
    try
      let x = Z.to_int64 x in
      let y = Z.to_int64 y in
      let* x = of_mutez x in
      let* y = of_mutez y in
      match x +? y with
      | Ok t -> some @@ Z.of_int64 (to_mutez t)
      | _ -> None
    with
      Z.Overflow -> None

  let mutez_sub : Z.t -> Z.t -> Z.t option = fun x y ->
    let open Memory_proto_alpha.Protocol.Alpha_context.Tez in
    let open Option in
    try
      let x = Z.to_int64 x in
      let y = Z.to_int64 y in
      let* x = of_mutez x in
      let* y = of_mutez y in
      match x -? y with
      | Ok t -> some @@ Z.of_int64 (to_mutez t)
      | _ -> None
    with
      Z.Overflow -> None

end

let create_chest_key (chest:bytes) (time:int) : bytes =
  let open Tezos_crypto in
  let chest = Data_encoding.Binary.of_bytes_exn Timelock.chest_encoding chest in
  Data_encoding.Binary.to_bytes_exn Timelock.chest_key_encoding @@ Timelock.create_chest_key chest ~time

let create_chest (payload:Bytes.t) (time:int) : _ =
  let open Tezos_crypto in
  let (chest, chest_key) = Timelock.create_chest_and_chest_key ~payload ~time in
  let chest_key_bytes =  Data_encoding.Binary.to_bytes_exn Timelock.chest_key_encoding chest_key in
  let chest_bytes = Data_encoding.Binary.to_bytes_exn Timelock.chest_encoding chest in
  (chest_bytes, chest_key_bytes)

let compile_contract ~raise ~add_warning ~protocol_version source_file entry_point declared_views =
  let open Ligo_compile in
  let syntax = "auto" in
  let options = Compiler_options.make ~protocol_version () in
  let michelson,prog_typed = Build.build_contract ~raise ~add_warning ~options syntax entry_point source_file in
  let views = Build.build_views ~raise ~add_warning ~options syntax entry_point (declared_views,prog_typed) source_file in
  Of_michelson.build_contract ~raise ~disable_typecheck:false michelson views

let clean_location_with v x =
  let open Tezos_micheline.Micheline in
  inject_locations (fun _ -> v) (strip_locations x)

let clean_locations e t =
  clean_location_with () e, clean_location_with () t

let add_ast_env ?(name = Location.wrap (Var.fresh ())) env binder body =
  let open Ast_aggregated in
  let aux (let_binder , expr) (e : expression) =
    if Var.compare let_binder.Location.wrap_content binder.Location.wrap_content <> 0 && Var.compare let_binder.wrap_content name.wrap_content <> 0 then
      e_a_let_in let_binder expr e { inline = false ; no_mutation = true ; view = false ; public = false }
    else
      e in
  let typed_exp' = List.fold_right ~f:aux ~init:body env in
  typed_exp'

let make_options ~raise ?param ctxt =
  let open Ligo_run.Of_michelson in
  let open Ligo_interpreter.Types in
  let default = { now = None ;
                  amount = "" ;
                  balance = "" ;
                  sender = None ;
                  source = None ;
                  parameter_ty = param } in
  match ctxt with
  | None ->
     make_dry_run_options ~raise default
  | Some (ctxt: Tezos_state.context) ->
    let source = ctxt.internals.source in
    let tezos_context = Tezos_state.get_alpha_context ~raise ctxt in
    let tezos_context = Memory_proto_alpha.Protocol.Alpha_context.Gas.set_limit tezos_context (Memory_proto_alpha.Protocol.Alpha_context.Gas.Arith.integral_exn (Z.of_int 800000)) in
    let timestamp = Timestamp.of_zint (Z.of_int64 (Proto_alpha_utils.Time.Protocol.to_seconds (Tezos_state.get_timestamp ctxt))) in
    {
      tezos_context ;
      source ;
      payer = source ;
      self = source ;
      amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.of_mutez_exn 100000000L ;
      chain_id = Memory_proto_alpha.Protocol.Environment.Chain_id.zero;
      balance = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero ;
      now = timestamp ;
    }

let run_expression_unwrap ~raise ?ctxt ?(loc = Location.generated) (c_expr : Stacking.compiled_expression) =
  let options = make_options ~raise ctxt in
  let runres = Ligo_run.Of_michelson.run_expression ~raise ~options c_expr.expr c_expr.expr_ty in
  match runres with
  | Success (expr_ty, expr) ->
     let expr, expr_ty = clean_locations expr expr_ty in
     (expr, expr_ty)
  | Fail _ ->
     raise.raise @@ Errors.generic_error loc "Running failed"

let compile_value ~raise aggregated_exp =
  let open Ligo_compile in
  let options = Compiler_options.make () in
  let mini_c_exp = Of_aggregated.compile_expression ~raise aggregated_exp in
  Of_mini_c.compile_expression ~raise ~options mini_c_exp

let compile_type ~raise type_exp =
  let open Ligo_compile in
  let ty = Of_aggregated.compile_type ~raise type_exp in
  Of_mini_c.compile_type ty

let compile_contract_ ~raise ~protocol_version subst_lst arg_binder rec_name in_ty out_ty aggregated_exp =
  let open Ligo_compile in
  let options = Compiler_options.make ~protocol_version () in
  let aggregated_exp' = add_ast_env subst_lst arg_binder aggregated_exp in
  let aggregated_exp = match rec_name with
    | None -> Ast_aggregated.e_a_lambda { result = aggregated_exp'; binder = arg_binder } in_ty out_ty
    | Some fun_name -> Ast_aggregated.e_a_recursive { fun_name ; fun_type  = (Ast_aggregated.t_function in_ty out_ty ()) ; lambda = { result = aggregated_exp';binder = arg_binder } } in
  let mini_c = Of_aggregated.compile_expression ~raise aggregated_exp in
  Of_mini_c.compile_contract ~raise ~options mini_c

let make_function in_ty out_ty arg_binder body subst_lst =
  let typed_exp' = add_ast_env subst_lst arg_binder body in
  Ast_aggregated.e_a_lambda {result=typed_exp'; binder=arg_binder} in_ty out_ty

let rec val_to_ast ~raise ~loc : Ligo_interpreter.Types.value ->
                          Ast_aggregated.type_expression ->
                          _ =
  fun v ty ->
  let open Ligo_interpreter.Types in
  let open Ast_aggregated in
  match v with
  | V_Ct C_unit ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected unit but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_unit ty) in
     e_a_unit ()
  | V_Ct (C_bool b) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected bool but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_bool ty) in
     e_a_bool b
  | V_Ct (C_int x) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected int but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_int ty) in
     e_a_int x
  | V_Ct (C_nat x) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected nat but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_nat ty) in
     e_a_nat x
  | V_Ct (C_mutez x) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected mutez but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_mutez ty) in
     e_a_mutez x
  | V_Ct (C_timestamp t) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected timestamp but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_timestamp ty) in
     e_a_timestamp t
  | V_Ct (C_string s) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected string but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_string ty) in
     e_a_string (Simple_utils.Ligo_string.standard s)
  | V_Ct (C_bytes b) -> (
    match get_t_bytes ty with
    | Some () -> e_a_bytes b
    | None -> (
      match get_t_chest ty with
      | Some () -> e_a_bytes b
      | None -> (
        match get_t_chest_key ty with
        | Some () -> e_a_bytes b
        | None -> raise.raise (Errors.generic_error loc (Format.asprintf "Expected bytes, chest, or chest_key but got %a" Ast_aggregated.PP.type_expression ty))
        )
    )
  )
  | V_Ct (C_address a) when is_t_address ty ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected address but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_address ty) in
     let x = string_of_contract a in
     e_a_address x
  | V_Ct (C_address _) ->
     raise.raise @@ (Errors.generic_error loc (Format.asprintf "Expected address but got %a" Ast_aggregated.PP.type_expression ty))
  | V_Ct (C_contract c) when is_t_contract ty ->
     let ty = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected contract but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_contract ty) in
     let x = string_of_contract c.address in
     (* TODO-er: if we want support for entrypoints, this should be fixed: *)
     let t = match c.entrypoint with
     | None -> e_a_contract (e_a_address x) ty
     | Some e ->
        e_a_contract_entrypoint (e_a_string (Ligo_string.Standard ("%" ^ e))) (e_a_address x) ty in
     t
  | V_Ct (C_contract _) ->
     raise.raise @@ (Errors.generic_error loc (Format.asprintf "Expected contract but got %a" Ast_aggregated.PP.type_expression ty))
  | V_Ct (C_key_hash kh) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected key_hash but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_key_hash ty) in
     let x = string_of_key_hash kh in
     e_a_key_hash x
  | V_Ct (C_key k) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected key but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_key ty) in
     let x = string_of_key k in
     e_a_key x
  | V_Ct (C_signature s) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected signature but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_signature ty) in
     let x = string_of_signature s in
     e_a_signature x
  | V_Ct (C_bls12_381_g1 b) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected bls12_381_g1 but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_bls12_381_g1 ty) in
     let x = bytes_of_bls12_381_g1 b in
     e_a_bls12_381_g1 x
  | V_Ct (C_bls12_381_g2 b) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected bls12_381_g2 but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_bls12_381_g2 ty) in
     let x = bytes_of_bls12_381_g2 b in
     e_a_bls12_381_g2 x
  | V_Ct (C_bls12_381_fr b) ->
     let () = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected bls12_381_fr but got %a" Ast_aggregated.PP.type_expression ty))
                 (get_t_bls12_381_fr ty) in
     let x = bytes_of_bls12_381_fr b in
     e_a_bls12_381_fr x
  | V_Construct (ctor, arg) when is_t_option ty ->
     let ty' = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected option but got %a" Ast_aggregated.PP.type_expression ty)) @@ get_t_option ty in
     if String.equal ctor "Some" then
       let arg = val_to_ast ~raise ~loc arg ty' in
       e_a_some arg
     else if String.equal ctor "None" then
       e_a_none ty'
     else
       raise.raise @@ Errors.generic_error loc "Expected either None or Some"
  | V_Construct (ctor, arg) when is_t_sum ty ->
     let map_ty = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected sum type but got %a" Ast_aggregated.PP.type_expression ty)) @@ get_t_sum ty in
     let {associated_type=ty';michelson_annotation=_;decl_pos=_} = LMap.find (Label ctor) map_ty.content in
     let arg = val_to_ast ~raise ~loc arg ty' in
     e_a_constructor ctor arg ty
  | V_Construct _ ->
     raise.raise @@ Errors.generic_error loc (Format.asprintf "Expected sum type but got %a" Ast_aggregated.PP.type_expression ty)
  | V_Func_val v ->
     make_ast_func ~raise ?name:v.rec_name v.env v.arg_binder v.body v.orig_lambda
  | V_Michelson (Ty_code { code = expr ; code_ty = expr_ty ; ast_ty = ty_exp }) ->
     let mini_c = trace ~raise Main_errors.main_decompile_michelson @@ Stacking.Decompiler.decompile_value expr_ty expr in
     trace ~raise Main_errors.main_decompile_mini_c @@ Spilling.decompile mini_c ty_exp
  | V_Record map when is_t_record ty ->
     let map_ty = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected record type but got %a" Ast_aggregated.PP.type_expression ty)) @@  get_t_record ty in
     make_ast_record ~raise ~loc map_ty map
  | V_Record _ ->
     raise.raise @@ Errors.generic_error loc (Format.asprintf "Expected record type but got %a" Ast_aggregated.PP.type_expression ty)
  | V_List l ->
     let ty = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected list but got %a" Ast_aggregated.PP.type_expression ty)) @@ get_t_list ty in
     make_ast_list ~raise ~loc ty l
  | V_Set l ->
     let ty = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected set but got %a" Ast_aggregated.PP.type_expression ty)) @@ get_t_set ty in
     make_ast_set ~raise ~loc ty l
  | V_Map kv when is_t_big_map ty ->
     let (key_ty, value_ty) = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected big_map but got %a" Ast_aggregated.PP.type_expression ty)) @@ get_t_big_map ty in
     make_ast_big_map ~raise ~loc key_ty value_ty kv
  | V_Map kv when is_t_map ty ->
     let (key_ty, value_ty) = trace_option ~raise (Errors.generic_error loc (Format.asprintf "Expected map but got %a" Ast_aggregated.PP.type_expression ty)) @@ get_t_map ty in
     make_ast_map~raise ~loc key_ty value_ty kv
  | V_Map _ ->
     raise.raise @@ Errors.generic_error loc (Format.asprintf "Expected map or big_map but got %a" Ast_aggregated.PP.type_expression ty)
  | V_Ligo _ ->
     raise.raise @@ Errors.generic_error loc "Cannot be abstracted: ligo"
  | V_Michelson (Contract _) ->
     raise.raise @@ Errors.generic_error loc "Cannot be abstracted: michelson-contract"
  | V_Mutation _ ->
     raise.raise @@ Errors.generic_error loc "Cannot be abstracted: mutation"
  | V_Failure _ ->
     raise.raise @@ Errors.generic_error loc "Cannot be abstracted: failure"

and make_ast_func ~raise ?name env arg body orig =
  let open Ast_aggregated in
  let env = make_subst_ast_env_exp ~raise env orig in
  let typed_exp' = add_ast_env ?name:name env arg body in
  let lambda = { result=typed_exp' ; binder=arg} in
  let typed_exp' = match name with
    | None ->
       let in_ty,out_ty =
         get_t_function_exn orig.type_expression in
       e_a_lambda lambda in_ty out_ty
    | Some fun_name ->
       e_a_recursive {fun_name ;
                      fun_type = orig.type_expression ;
                      lambda } in
  typed_exp'

and make_ast_record ~raise ~loc map_ty map =
  let open Ligo_interpreter.Types in
  let kv_list = Ast_aggregated.Helpers.kv_list_of_t_record_or_tuple ~layout:map_ty.layout map_ty.content in
  let kv_list = List.map ~f:(fun (l, ty) -> let value = LMap.find l map in let ast = val_to_ast ~raise ~loc value ty.associated_type in (l, ast)) kv_list in
  Ast_aggregated.ez_e_a_record ~layout:map_ty.layout kv_list

and make_ast_list ~raise ~loc ty l =
  let l = List.map ~f:(fun v -> val_to_ast ~raise ~loc v ty) l in
  List.fold_right l ~f:Ast_aggregated.e_a_cons ~init:(Ast_aggregated.e_a_nil ty)

and make_ast_set ~raise ~loc ty l =
  let l = List.dedup_and_sort ~compare:Ligo_interpreter.Combinators.compare_value l in
  let l = List.map ~f:(fun v -> val_to_ast ~raise ~loc v ty) l in
  List.fold_right l ~f:Ast_aggregated.e_a_set_add ~init:(Ast_aggregated.e_a_set_empty ty)

and make_ast_big_map ~raise ~loc key_ty value_ty kv =
  let kv = List.dedup_and_sort ~compare:(fun (k, _) (k', _) -> Ligo_interpreter.Combinators.compare_value k k') kv in
  let kv = List.map ~f:(fun (k, v) ->
                let k = val_to_ast ~raise ~loc k key_ty in
                let v = val_to_ast ~raise ~loc v value_ty in
                (k, v)) kv in
  List.fold_right kv ~f:(fun (k, v) r -> Ast_aggregated.e_a_big_map_add k v r) ~init:(Ast_aggregated.e_a_big_map_empty key_ty value_ty)

and make_ast_map ~raise ~loc key_ty value_ty kv =
  let kv = List.dedup_and_sort ~compare:(fun (k, _) (k', _) -> Ligo_interpreter.Combinators.compare_value k k') kv in
  let kv = List.map ~f:(fun (k, v) ->
                let k = val_to_ast ~raise ~loc k key_ty in
                let v = val_to_ast ~raise ~loc v value_ty in
                (k, v)) kv in
  List.fold_right kv ~f:(fun (k, v) r -> Ast_aggregated.e_a_map_add k v r) ~init:(Ast_aggregated.e_a_map_empty key_ty value_ty)

and compile_simple_value ~raise ?ctxt ~loc : Ligo_interpreter.Types.value ->
                      Ast_aggregated.type_expression ->
                      Ligo_interpreter.Types.typed_michelson_code =
  fun v ty ->
  let typed_exp = val_to_ast ~raise ~loc v ty in
  let _ = trace ~raise Main_errors.self_ast_aggregated_tracer @@ Self_ast_aggregated.expression_obj typed_exp in
  let compiled_exp = compile_value ~raise typed_exp in
  let expr, _ = run_expression_unwrap ~raise ?ctxt ~loc compiled_exp in
  (* TODO-er: check the ignored second component: *)
  let expr_ty = clean_location_with () compiled_exp.expr_ty in
  { code = expr ; code_ty = expr_ty ; ast_ty = typed_exp.type_expression }

and make_subst_ast_env_exp ~raise env expr =
  let open Ligo_interpreter.Types in
  let get_fv expr = List.map ~f:(fun v -> v.Location.wrap_content) @@
   snd @@ Self_ast_aggregated.Helpers.Free_variables.expression expr in
  let rec aux (fv) acc = function
    | [] -> acc
    | Expression { name; item ; no_mutation = _ } :: tl ->
       let binder = Location.unwrap name in
       if List.mem fv binder ~equal:Var.equal then
         let expr = val_to_ast ~raise ~loc:name.location item.eval_term item.ast_type in
         let expr_fv = get_fv expr in
         let fv = List.remove_element ~compare:Var.compare binder fv in
         let fv = List.dedup_and_sort ~compare:Var.compare (fv @ expr_fv) in
         aux fv ((name, expr) :: acc) tl
       else
         aux fv acc tl in
  aux (get_fv expr) [] env

let get_literal_type : Ast_aggregated.literal -> Ast_aggregated.type_expression =
  fun t ->
  let open Ast_aggregated in
  match t with
  | (Literal_unit) -> t_unit ()
  | (Literal_int _) -> t_int ()
  | (Literal_nat _) -> t_nat ()
  | (Literal_mutez _) -> t_mutez ()
  | (Literal_string _) -> t_string ()
  | (Literal_bytes _) -> t_bytes ()
  | (Literal_timestamp _) -> t_timestamp ()
  | (Literal_address _) -> t_address ()
  | (Literal_signature _) -> t_signature ()
  | (Literal_key _) -> t_key ()
  | (Literal_key_hash _) -> t_key_hash ()
  | (Literal_chain_id _) -> t_chain_id ()
  | (Literal_operation _) -> t_operation ()
  | (Literal_bls12_381_g1 _) -> t_bls12_381_g1 ()
  | (Literal_bls12_381_g2 _) -> t_bls12_381_g2 ()
  | (Literal_bls12_381_fr _) -> t_bls12_381_fr ()

let compile_literal ~raise ~loc : Ast_aggregated.literal -> _ =
  fun v ->
  let open Ligo_interpreter.Types in
  let type_lit = get_literal_type v in
  let typed_exp = Ast_aggregated.e_a_literal v type_lit in
  let compiled_exp = compile_value ~raise typed_exp in
  let expr, expr_ty = run_expression_unwrap ~raise ~loc compiled_exp in
  (expr, expr_ty, typed_exp.type_expression)

let storage_retreival_dummy_ty = Tezos_utils.Michelson.prim "int"

let run_michelson_code ~raise ~loc (ctxt : Tezos_state.context) code func_ty arg arg_ty =
  let open Ligo_interpreter.Types in
  let { code = a ; code_ty = b ; _ } = compile_simple_value ~raise ~loc arg arg_ty in
  let func_ty = compile_type ~raise func_ty in
  let open Tezos_micheline in
  let (code, errs) = Micheline_parser.tokenize code in
  let func = (match errs with
              | _ :: _ -> raise.raise (Errors.generic_error Location.generated "Could not parse")
              | [] ->
                 let (code, errs) = Micheline_parser.parse_expression ~check:false code in
                 match errs with
                 | _ :: _ -> raise.raise (Errors.generic_error Location.generated "Could not parse")
                 | [] ->
                    let code = Micheline.strip_locations code in
                    (* hmm *)
                    let code = Micheline.inject_locations (fun _ -> ()) code in
                    match code with
                    | Seq (_, s) ->
                       Tezos_utils.Michelson.(seq ([i_push b a] @ s))
                    | _ ->
                       raise.raise (Errors.generic_error Location.generated "Could not parse")
             ) in
  let r = Ligo_run.Of_michelson.run_expression ~raise func func_ty in
  match r with
  | Success (ty, value) ->
      Michelson_to_value.decompile_to_untyped_value ~raise ~bigmaps:ctxt.transduced.bigmaps ty value
  | _ ->
     raise.raise (Errors.generic_error loc "Could not execute Michelson function")

let run_raw_michelson_code ~raise ~loc code ty =
  let ty = compile_type ~raise ty in
  let open Tezos_micheline in
  let (code, errs) = Micheline_parser.tokenize code in
  let code = (match errs with
              | _ :: _ -> raise.raise (Errors.generic_error Location.generated "Could not parse")
              | [] ->
                 let (code, errs) = Micheline_parser.parse_expression ~check:false code in
                 match errs with
                 | _ :: _ -> raise.raise (Errors.generic_error Location.generated "Could not parse")
                 | [] ->
                    let code = Micheline.strip_locations code in
                    (* hmm *)
                    let code = Micheline.inject_locations (fun _ -> ()) code in
                    match code with
                    | Seq (_, s) ->
                       Tezos_utils.Michelson.(seq s)
                    | _ ->
                       raise.raise (Errors.generic_error Location.generated "Could not parse")
             ) in
  let r = Ligo_run.Of_michelson.run_expression ~raise code ty in
  match r with
  | Success (ty, value) ->
    let code = Micheline.map_node (fun _ -> ()) (fun x -> x) value in
    let code_ty = Micheline.map_node (fun _ -> ()) (fun x -> x) ty in
    (code_ty, code)
  | _ ->
     raise.raise (Errors.generic_error loc "Could not execute Michelson function")
