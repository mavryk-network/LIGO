open Simple_utils.Trace
open Proto_alpha_utils
module Tezos_alpha_test_helpers = Tezos_013_PtJakart_test_helpers
open Errors
open Ligo_interpreter_exc
open Ligo_interpreter.Types
open Ligo_interpreter.Combinators
module Tezos_protocol = Tezos_protocol_013_PtJakart
module Tezos_raw_protocol = Tezos_raw_protocol_013_PtJakart
module Tezos_protocol_parameters = Tezos_protocol_013_PtJakart_parameters

type r = Errors.interpreter_error raise

type bootstrap_contract =
  int * unit Tezos_utils.Michelson.michelson * unit Tezos_utils.Michelson.michelson * Ast_aggregated.type_expression * Ast_aggregated.type_expression
type baker_account =
  string * Signature.Public_key.t * int64 option
type block = Tezos_alpha_test_helpers.Block.t
type last_originations = (Memory_proto_alpha.Protocol.Alpha_context.Contract.t * Memory_proto_alpha.Protocol.Alpha_context.Contract.t list) list
type storage_tys = (Tezos_protocol.Protocol.Alpha_context.Contract.t * Ast_aggregated.type_expression) list
type parameter_tys = (Tezos_protocol.Protocol.Alpha_context.Contract.t * Ast_aggregated.type_expression) list
type state_error = Tezos_error_monad.TzCore.error list
type tezos_op = Tezos_raw_protocol.Alpha_context.packed_operation

(*
  The threaded context has three parts:
  - Raw: Tezos state as represented in the tezos code-base (same types)
  - Transduced: data extracted from the raw context and transduced to a more suitable form
    after each baking operation
  - Interpreter internal state: data that can't be extracted from the raw context but still needed by LIGO testing framework
*)
type context = {
  raw : raw ;
  transduced : transduced ;
  internals : internals ;
}
and raw = block
and transduced = {
  last_originations : last_originations ; (* newly orginated contracts caused by the last baking operation *)
  bigmaps : bigmaps ; (* context bigmaps state as ligo values *)
}
and internals = {
  protocol_version : Environment.Protocols.t ;
  baker_policy : Tezos_alpha_test_helpers.Block.baker_policy ; (* baker to be used for the next transfer/origination *)
  source : Memory_proto_alpha.Protocol.Alpha_context.Contract.t ; (* source to be used for the next transfer/origination *)
  next_bootstrapped_contracts : bootstrap_contract list ; (* next contracts to be injected as boostrap accounts in the next state reset *)
  next_baker_accounts : baker_account list ; (* next contracts to be injected as boostrap accounts in the next state reset *)
  bootstrapped : Memory_proto_alpha.Protocol.Alpha_context.Contract.t list ; (* addresses of boostrapped contracts *)
  storage_tys : storage_tys ; (* contract storage ligo types of all originated contracts *)
  parameter_tys : parameter_tys ; (* contract parameter ligo types of bootstrapped contracts *)
}

let contexts : context list ref = ref []

(* Some getters    TODO: might be moved to tezos-X-test-helpers ? *)
let get_timestamp (ctxt : context) =
  ctxt.raw.header.shell.timestamp
let get_balance ~raise ~loc ~calltrace (ctxt :context) addr =
  Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Tezos_alpha_test_helpers.Context.Contract.balance (B ctxt.raw) addr
let contract_exists : context ->  Memory_proto_alpha.Protocol.Alpha_context.Contract.t -> bool = fun ctxt contract ->
  let info = Lwt_main.run @@
    Tezos_raw_protocol.Alpha_services.Contract.info ~normalize_types:true Tezos_alpha_test_helpers.Block.rpc_ctxt ctxt.raw contract in
  Trace.tz_result_to_bool info

let get_voting_power ~raise ~loc ~calltrace (ctxt :context) key_hash =
  let vp =
    Tezos_alpha_test_helpers.Context.get_voting_power (B ctxt.raw) key_hash in
  Trace.trace_alpha_shell_tzresult_lwt ~raise (throw_obj_exc loc calltrace) vp

let get_total_voting_power ~raise ~loc ~calltrace (ctxt :context) =
  let tvp =
    Tezos_alpha_test_helpers.Context.get_total_voting_power (B ctxt.raw) in
  Trace.trace_alpha_shell_tzresult_lwt ~raise (throw_obj_exc loc calltrace) tvp

let equal_account = Memory_proto_alpha.Protocol.Alpha_context.Contract.equal
let compare_account = Memory_proto_alpha.Protocol.Alpha_context.Contract.compare

type ligo_repr = unit Tezos_utils.Michelson.michelson
type canonical_repr = Tezos_raw_protocol.Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical
let ligo_to_canonical : raise:r -> loc:Location.t -> calltrace:calltrace -> ligo_repr -> canonical_repr Data_encoding.lazy_t =
  fun ~raise ~loc ~calltrace x ->
    let open Tezos_micheline.Micheline in
    let x = inject_locations (fun _ -> 0) (strip_locations x) in
    let x = strip_locations x in
    let x = Trace.trace_alpha_tzresult ~raise (throw_obj_exc loc calltrace) @@
      Tezos_protocol.Protocol.Michelson_v1_primitives.prims_of_strings x
    in
    Tezos_protocol.Protocol.Alpha_context.Script.lazy_expr x
let canonical_to_ligo : canonical_repr -> ligo_repr =
  fun x ->
  x |> Tezos_protocol.Protocol.Michelson_v1_primitives.strings_of_prims
    |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())

let parse_constant ~raise ~loc ~calltrace code =
  let open Tezos_micheline in
  let open Tezos_micheline.Micheline in
  let (code, errs) = Micheline_parser.tokenize code in
  let code = (match errs with
              | _ :: _ -> raise.raise (throw_obj_exc loc calltrace @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
              | [] ->
                 let (code, errs) = Micheline_parser.parse_expression ~check:false code in
                 match errs with
                 | _ :: _ -> raise.raise (throw_obj_exc loc calltrace @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
                 | [] -> map_node (fun _ -> ()) (fun x -> x) code
             ) in
  code


(* REMITODO: NOT STATE RELATED, move out ? *)
let get_contract_rejection_data :
  state_error -> (Memory_proto_alpha.Protocol.Alpha_context.Contract.t * unit Tezos_utils.Michelson.michelson) option =
  fun errs ->
    let open Tezos_protocol.Protocol in
    let open Script_interpreter in
    let open Environment in
    match errs with
    | [ Ecoproto_error (Runtime_contract_error contract) ; Ecoproto_error (Reject (_,x,_)) ] ->
      let x = canonical_to_ligo x in
      Some (contract,x)
    | _ -> None

let set_big_map ~raise (ctxt : context) id version k_ty v_ty =
  let open Tezos_micheline.Micheline in
  let key_type = strip_locations k_ty in
  let key_type = Proto_alpha_utils.Trace.trace_alpha_tzresult ~raise
                   (fun _ -> generic_error Location.generated "Cannot extract key type") @@
                   Tezos_protocol.Protocol.Michelson_v1_primitives.prims_of_strings key_type in
  let value_type = strip_locations v_ty in
  let value_type = Proto_alpha_utils.Trace.trace_alpha_tzresult ~raise
                     (fun _ -> generic_error Location.generated "Cannot extract value type") @@
                     Tezos_protocol.Protocol.Michelson_v1_primitives.prims_of_strings value_type in
  let data : Ligo_interpreter.Types.bigmap_data = { key_type ; value_type ; version } in
  let transduced = { ctxt.transduced with bigmaps = List.Assoc.add ctxt.transduced.bigmaps ~equal:Int.equal id data } in
  { ctxt with transduced }

let get_storage ~raise ~loc ~calltrace ctxt addr =
  let st_v = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Tezos_protocol.Protocol.Alpha_services.Contract.storage Tezos_alpha_test_helpers.Block.rpc_ctxt ctxt.raw addr
  in
  let st_ty = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Tezos_protocol.Protocol.Alpha_services.Contract.script Tezos_alpha_test_helpers.Block.rpc_ctxt ctxt.raw addr
  in
  let x = Trace.trace_alpha_tzresult ~raise (throw_obj_exc loc calltrace) @@
    Memory_proto_alpha.Protocol.Script_repr.force_decode st_ty.code
  in
  let { storage_type ; _  } : Tezos_protocol.Protocol.Script_ir_translator.toplevel =
    (* Feels wrong :'( *)
    let (alpha_context,_,_) =
      let open Tezos_raw_protocol in
      Trace.trace_alpha_tzresult_lwt ~raise (fun _ -> corner_case ()) @@
        Alpha_context.prepare
          ~level:ctxt.raw.header.shell.level
          ~predecessor_timestamp:ctxt.raw.header.shell.timestamp
          ~timestamp:(get_timestamp ctxt)
          ctxt.raw.context
    in
    fst @@ Trace.trace_alpha_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
      Tezos_protocol.Protocol.Script_ir_translator.parse_toplevel alpha_context ~legacy:false x
  in
  let storage_type = Tezos_micheline.Micheline.(inject_locations (fun _ -> ()) (strip_locations storage_type)) in
  let storage_type = Tezos_micheline.Micheline.strip_locations storage_type in
  let storage_type = canonical_to_ligo storage_type in
  (st_v, storage_type)

let get_alpha_context ~raise ctxt =
  let (alpha_context,_,_) =
    let open Tezos_raw_protocol in
    Trace.trace_alpha_tzresult_lwt ~raise (fun _ -> corner_case ()) @@
      Alpha_context.prepare
        ~level:ctxt.raw.header.shell.level
        ~predecessor_timestamp:ctxt.raw.header.shell.timestamp
        ~timestamp:(get_timestamp ctxt)
        ctxt.raw.context in
  alpha_context

let unwrap_baker ~raise ~loc ~calltrace : Memory_proto_alpha.Protocol.Alpha_context.Contract.t -> Tezos_crypto.Signature.Public_key_hash.t  =
  fun x ->
    Trace.trace_option ~raise (generic_error ~calltrace loc "The baker is not an implicit account") @@ Memory_proto_alpha.Protocol.Alpha_context.Contract.is_implicit x

let baker_policy ~raise ~loc ~calltrace baker_policy =
  let open Tezos_alpha_test_helpers.Block in
  match baker_policy with
  | `By_round i -> By_round i
  | `By_account a -> By_account (unwrap_baker ~raise ~loc ~calltrace a)
  | `Excluding l -> Excluding (List.map ~f:(unwrap_baker ~raise ~loc ~calltrace) l)

let unwrap_source ~raise ~loc ~calltrace : Memory_proto_alpha.Protocol.Alpha_context.Contract.t -> Memory_proto_alpha.Protocol.Alpha_context.Contract.t  =
  fun x ->
    let _ = Trace.trace_option ~raise (generic_error ~calltrace loc "The source address is not an implicit account") @@ Memory_proto_alpha.Protocol.Alpha_context.Contract.is_implicit x in
    x
let implicit_account ~raise ~loc ~calltrace : string -> Tezos_protocol.Protocol.Alpha_context.Contract.t  =
  fun x ->
    Trace.trace_alpha_tzresult ~raise (fun _ -> generic_error ~calltrace loc "Cannot convert to implicit account") @@ Tezos_protocol.Protocol.Alpha_context.Contract.of_b58check x

let script_of_compiled_code ~raise ~loc ~calltrace (contract : unit Tezos_utils.Michelson.michelson) (storage : unit Tezos_utils.Michelson.michelson) : Tezos_protocol.Protocol.Alpha_context.Script.t  =
  let open! Tezos_protocol.Protocol.Alpha_context.Script in
  let code = ligo_to_canonical ~raise ~loc ~calltrace contract in
  let storage = ligo_to_canonical ~raise ~loc ~calltrace storage in
  { code ; storage }

let extract_origination_from_result :
  type a .
    Memory_proto_alpha.Protocol.Alpha_context.Contract.t ->
    a Tezos_protocol.Protocol.Apply_results.contents_result ->
    last_originations =
  fun src x ->
  let open Tezos_raw_protocol in
  match x with
  | Manager_operation_result { operation_result = Applied (Transaction_result _) ; internal_operation_results ; balance_updates=_ } ->
    let aux (x:Apply_results.packed_internal_manager_operation_result) =
      match x with
      | Internal_manager_operation_result ({source ; _},Applied (Origination_result x)) -> [(source, x.originated_contracts)]
      | _ -> []
    in
    List.concat @@ List.map ~f:aux internal_operation_results
  | Manager_operation_result { operation_result = Applied (Origination_result x) ; internal_operation_results=_ ; balance_updates=_} ->
    [(src, x.originated_contracts)]
  | _ -> []

let extract_lazy_storage_diff_from_result :
  type a .
    a Tezos_raw_protocol.Apply_results.contents_result ->
    Tezos_raw_protocol.Alpha_context.Lazy_storage.diffs option list =
  fun x ->
  let open Tezos_raw_protocol in
  match x with
  | Manager_operation_result { operation_result = Applied (Transaction_result (Transaction_to_contract_result y)) ; internal_operation_results ; balance_updates=_ } ->
    let aux (x:Apply_results.packed_internal_manager_operation_result) =
      match x with
      | Internal_manager_operation_result ({source = _ ; _},Applied (Origination_result x)) -> [x.lazy_storage_diff]
      | Internal_manager_operation_result ({source = _ ; _},Applied (Transaction_result (Transaction_to_contract_result x))) -> [x.lazy_storage_diff]
      | _ -> []
    in
    (List.concat @@ List.map ~f:aux internal_operation_results) @ [y.lazy_storage_diff]
  | Manager_operation_result { operation_result = Applied (Origination_result x) ; internal_operation_results=_ ; balance_updates=_ } ->
    [x.lazy_storage_diff]
  | _ -> []

let get_last_originations : Memory_proto_alpha.Protocol.Alpha_context.Contract.t -> Tezos_protocol.Protocol.operation_receipt -> last_originations =
  fun top_src x ->
    let open Tezos_raw_protocol in
    match x with
    | No_operation_metadata -> []
    | Operation_metadata { contents } -> (
      let rec aux : type a . last_originations -> a Apply_results.contents_result_list -> last_originations =
        fun acc x ->
          match x with
          | Cons_result (hd, tl) -> (
            let x = extract_origination_from_result top_src hd in
            aux (acc @ x) tl
          )
          | Single_result x -> (
            let x = extract_origination_from_result top_src x in
            x @ acc
          )
      in
      aux [] contents
    )

let get_lazy_storage_diffs : Tezos_protocol.Protocol.operation_receipt ->
                             Tezos_raw_protocol.Alpha_context.Lazy_storage.diffs option list =
  fun x ->
    let open Tezos_raw_protocol in
    match x with
    | No_operation_metadata -> []
    | Operation_metadata { contents } -> (
      let rec aux : type a . _ -> a Apply_results.contents_result_list -> _ =
        fun acc x ->
          match x with
          | Cons_result (hd, tl) -> (
            let x = extract_lazy_storage_diff_from_result hd in
            aux (acc @ x) tl
          )
          | Single_result x -> (
            let x = extract_lazy_storage_diff_from_result x in
            x @ acc
          )
      in
      aux [] contents
    )

let convert_lazy_storage_diffs (lazy_storage_diffs : Tezos_raw_protocol.Alpha_context.Lazy_storage.diffs) =
  let enc = Data_encoding.Binary.to_bytes_exn Tezos_raw_protocol.Alpha_context.Lazy_storage.encoding lazy_storage_diffs in
  Data_encoding.Binary.of_bytes_exn Tezos_raw_protocol.Lazy_storage_diff.encoding enc

let upd_bigmaps : raise:r -> bigmaps -> Tezos_raw_protocol.Apply_results.packed_operation_metadata -> bigmaps = fun ~raise bigmaps op ->
  let lazy_storage_diffs = get_lazy_storage_diffs op in
  let lazy_storage_diffs = List.concat @@ List.filter_opt lazy_storage_diffs in
  let lazy_storage_diffs = convert_lazy_storage_diffs lazy_storage_diffs in
  let get_id id = Z.to_int (Tezos_raw_protocol.Lazy_storage_kind.Big_map.Id.unparse_to_z id) in
  List.fold_right lazy_storage_diffs
    ~init:bigmaps
    ~f:(
      fun it bigmaps ->
        match it with
        | Item (Big_map, id, Remove) ->
            List.Assoc.remove bigmaps ~equal:Int.equal (get_id id)
        | Item (Big_map, id, Update {init=Alloc {key_type;value_type};updates}) ->
            let kv_diff = List.map ~f:(fun {key;value;key_hash=_} -> (key, value)) updates in
            let aux (kv : (value * value) list) (key, value) =
              let key_value = Michelson_to_value.conv ~raise ~bigmaps key_type key in
              match value with
              | None -> List.Assoc.remove kv ~equal:equal_value key_value
              | Some value ->
                let value_value = Michelson_to_value.conv ~raise ~bigmaps value_type value in
                List.Assoc.add kv ~equal:equal_value key_value value_value in
            let state = List.fold kv_diff ~init:[] ~f:aux in
            let data = {key_type;value_type;version = state} in
            List.Assoc.add bigmaps ~equal:Int.equal (get_id id) data
        | Item (Big_map, id, Update {init=Copy {src};updates}) ->
            let kv_diff = List.map ~f:(fun {key;value;key_hash=_} -> (key, value)) updates in
            let data = List.Assoc.find_exn bigmaps ~equal:Int.equal (get_id src) in
            let state = data.version in
            let aux (kv : (value * value) list) (key, value) =
              let key_value = Michelson_to_value.conv ~raise ~bigmaps data.key_type key in
              match value with
              | None -> List.Assoc.remove kv ~equal:equal_value key_value
              | Some value ->
                let value_value = Michelson_to_value.conv ~raise ~bigmaps data.value_type value in
                List.Assoc.add kv ~equal:equal_value key_value value_value in
            let state = List.fold kv_diff ~init:state ~f:aux in
            let data = { data with version = state } in
            List.Assoc.add bigmaps ~equal:Int.equal (get_id id) data
        | Item (Big_map, id, Update {init=Existing;updates}) ->
            let kv_diff = List.map ~f:(fun {key;value;key_hash=_} -> (key, value)) updates in
            let data = List.Assoc.find_exn bigmaps ~equal:Int.equal (get_id id) in
            let state = data.version in
            let aux (kv : (value * value) list) (key, value) =
              let key_value = Michelson_to_value.conv ~raise ~bigmaps data.key_type key in
              match value with
              | None -> List.Assoc.remove kv ~equal:equal_value key_value
              | Some value ->
                let value_value = Michelson_to_value.conv ~raise ~bigmaps data.value_type value in
                List.Assoc.add kv ~equal:equal_value key_value value_value in
            let state = List.fold kv_diff ~init:state ~f:aux in
            let data = { data with version = state } in
            List.Assoc.add bigmaps ~equal:Int.equal (get_id id) data
        | _  -> bigmaps
    )

(* upd_context_of_receipts *)
let upd_transduced_data : raise:r -> context -> Tezos_raw_protocol.Apply_results.packed_operation_metadata -> transduced =
  fun ~raise ctxt op_data ->
    let last_originations = get_last_originations ctxt.internals.source op_data in
    let bigmaps = upd_bigmaps ~raise ctxt.transduced.bigmaps op_data in
    { last_originations ; bigmaps }

(* result of baking an operation *)
type add_operation_outcome =
  | Success of (context * Z.t(* gas consumed *))
  | Fail of state_error

let get_last_operation_result (incr : Tezos_alpha_test_helpers.Incremental.t) =
  match Tezos_alpha_test_helpers.Incremental.rev_tickets incr with
  | [] -> failwith "Tried to get last operation result in empty block"
  | hd :: _ -> hd

let get_single_tx_result (x : Tezos_raw_protocol.Apply_results.packed_operation_metadata) =
  match x with
  | Operation_metadata ({contents = Single_result y} : _ Tezos_raw_protocol.Apply_results.operation_metadata)  -> (
    match y with
    | Manager_operation_result { operation_result = Applied (
          Transaction_result (Transaction_to_contract_result { consumed_gas ; _ })
        | Transaction_result (Transaction_to_tx_rollup_result { consumed_gas ; _ })
        | Origination_result { consumed_gas ; _ }
        | Delegation_result { consumed_gas ; _ }
        | Register_global_constant_result { consumed_gas ; _ } )
      ; _ } -> Some consumed_gas
    | _ -> None
  )
  | _ -> None

let get_consumed_gas x =
  let fp_to_z (fp : Memory_proto_alpha.Protocol.Alpha_context.Gas.Arith.fp) : z =
    let open Data_encoding in
    (match Binary.to_bytes_opt Memory_proto_alpha.Protocol.Alpha_context.Gas.Arith.z_fp_encoding fp with Some x -> x | None -> failwith "failed decoding gas")
    |> Binary.of_bytes_exn z
  in
  match get_single_tx_result x with
  | Some x -> Z.((fp_to_z x) / (of_int 1000))
  | None -> Z.zero

let bake_ops : raise:r -> loc:Location.t -> calltrace:calltrace -> context -> (Tezos_alpha_test_helpers.Incremental.t -> tezos_op) list -> add_operation_outcome =
  fun ~raise ~loc ~calltrace ctxt operation ->
    let open Tezos_alpha_test_helpers in
    (* First check if baker is going to be successfully selected *)
    let _ = Trace.trace_tzresult_lwt ~raise (fun _ -> raise.raise (generic_error ~calltrace loc "Baker cannot bake. Enough rolls? Enough cycles passed?")) @@ Block.(get_next_baker ~policy:ctxt.internals.baker_policy ctxt.raw) in
    let incr = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
                 Incremental.begin_construction ~policy:ctxt.internals.baker_policy ctxt.raw in
    let aux incr op = Lwt_main.run @@ Incremental.add_operation incr (op incr) in
    match List.fold_result ~f:aux ~init:incr operation with
    | Ok incr ->
       let last_operation = get_last_operation_result incr in
       let consum = get_consumed_gas last_operation in
       let raw = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
                   Incremental.finalize_block incr in
       let transduced = upd_transduced_data ~raise ctxt last_operation in
       Success ({ ctxt with raw ; transduced }, consum)
    | Error errs -> Fail errs

let bake_op  : raise:r -> loc:Location.t -> calltrace:calltrace -> context -> tezos_op -> add_operation_outcome =
  fun ~raise ~loc ~calltrace ctxt op -> bake_ops ~raise ~loc ~calltrace ctxt [fun _ -> op]

let bake_until_n_cycle_end ~raise ~loc ~calltrace (ctxt : context) n =
  let open Tezos_alpha_test_helpers in
  let raw = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@ Block.bake_until_n_cycle_end ~policy:ctxt.internals.baker_policy n ctxt.raw in
  { ctxt with raw }

let register_delegate ~raise ~loc ~calltrace (ctxt : context) pkh =
  let open Tezos_alpha_test_helpers in
  let contract = Tezos_raw_protocol.Alpha_context.Contract.implicit_contract pkh in
  let operation = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@ Op.delegation (B ctxt.raw) contract (Some pkh) in
  match bake_op ~raise ~loc ~calltrace ctxt operation with
  | Success (ctxt,_) ->
    ctxt
  | Fail errs -> raise.raise (target_lang_error loc calltrace errs)

let register_constant ~raise ~loc ~calltrace (ctxt : context) ~source ~value =
  let open Tezos_alpha_test_helpers in
  let value = ligo_to_canonical ~raise ~loc ~calltrace value in
  let hash = Trace.trace_alpha_tzresult ~raise (throw_obj_exc loc calltrace) @@ Tezos_protocol.Protocol.Script_repr.force_bytes value in
  let hash = Tezos_protocol.Protocol.Script_expr_hash.hash_bytes [hash] in
  let hash = Format.asprintf "%a" Tezos_protocol.Protocol.Script_expr_hash.pp hash in
  let operation = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@ Op.register_global_constant (B ctxt.raw) ~source ~value in
  match bake_op ~raise ~loc ~calltrace ctxt operation with
  | Success (ctxt,_) -> (hash, ctxt)
  | Fail errs -> raise.raise (target_lang_error loc calltrace errs)

let read_file_constants ~raise fn =
  try
    let buf = In_channel.read_all fn in
    let json = Yojson.Basic.from_string buf in
    json |> Yojson.Basic.Util.to_list |> List.map ~f:Yojson.Basic.Util.to_string
  with Sys_error _ -> raise.Trace.raise (`Main_cannot_open_global_constants fn)
     | Yojson.Json_error s -> raise.Trace.raise (`Main_cannot_parse_global_constants (fn, s))

let register_file_constants ~raise ~loc ~calltrace fn (ctxt : context) ~source =
  let open Tezos_alpha_test_helpers in
  let string_to_constant constant =
    let constant = parse_constant ~raise ~loc ~calltrace constant in
    ligo_to_canonical ~raise ~loc ~calltrace constant in
  let constant_to_hash constant =
    let hash = Trace.trace_alpha_tzresult ~raise (throw_obj_exc loc calltrace) @@ Tezos_protocol.Protocol.Script_repr.force_bytes constant in
    let hash = Tezos_protocol.Protocol.Script_expr_hash.hash_bytes [hash] in
    Format.asprintf "%a" Tezos_protocol.Protocol.Script_expr_hash.pp hash in
  let constants = read_file_constants ~raise fn in
  let constants = List.map ~f:string_to_constant constants in
  let hashes = List.map ~f:constant_to_hash constants in
  let aux constant ctxt =
    let op = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@ Op.register_global_constant (B ctxt.raw) ~source ~value:constant in
    match bake_op ~raise ~loc ~calltrace ctxt op with
    | Success (ctxt,_) -> ctxt
    | Fail errs -> raise.raise (target_lang_error loc calltrace errs) in
  let ctxt = List.fold_right ~f:aux ~init:ctxt constants in
  (hashes, ctxt)

let add_account ~raise ~loc ~calltrace sk pk pkh : unit =
  let open Tezos_alpha_test_helpers in
  let sk = Trace.trace_tzresult ~raise (fun _ -> Errors.generic_error ~calltrace loc "Cannot parse secret key") @@ Tezos_crypto.Signature.Secret_key.of_b58check sk in
  let account = Account.{ sk ; pk ; pkh } in
  Account.add_account account

let get_account ~raise ~loc ~calltrace mc : string * Signature.public_key =
  let open Tezos_alpha_test_helpers in
  let pkh = Trace.trace_option ~raise (generic_error ~calltrace loc "The account is not an implicit account") @@ Memory_proto_alpha.Protocol.Alpha_context.Contract.is_implicit mc in
  let account = Trace.trace_tzresult_lwt ~raise (fun _ -> Errors.generic_error ~calltrace loc "Cannot find account") @@ Account.find pkh in
  let sk = Signature.Secret_key.to_b58check account.sk in
  let pk = account.pk in
  (sk, pk)

let new_account : unit -> string * Signature.public_key = fun () ->
  let open Tezos_alpha_test_helpers.Account in
  let account = new_account () in
  let sk = Signature.Secret_key.to_b58check account.sk in
  (sk, account.pk)

let sign_message ~raise ~loc ~calltrace (packed_payload : bytes) sk : Signature.t =
  let open Tezos_crypto in
  let sk = Trace.trace_tzresult ~raise (throw_obj_exc loc calltrace) @@ Signature.Secret_key.of_b58check sk in
  let signed_data = Signature.sign sk packed_payload in
  signed_data

let transfer ~raise ~loc ~calltrace (ctxt:context) ?entrypoint dst parameter amt : add_operation_outcome =
  let open Tezos_alpha_test_helpers in
  let source = unwrap_source ~raise ~loc ~calltrace ctxt.internals.source in
  let parameters = ligo_to_canonical ~raise ~loc ~calltrace parameter in
  let operation : Tezos_raw_protocol.Alpha_context.packed_operation = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    (* TODO: fee? *)
    let amt = Int64.of_int (Z.to_int amt) in
    let gas_limit = (Memory_proto_alpha.Protocol.Alpha_context.Gas.Arith.integral_of_int_exn 999_999) in
    Op.transaction ~gas_limit ~fee:(Test_tez.of_int 1) ~parameters ?entrypoint (B ctxt.raw) source dst (Test_tez.of_mutez_exn amt)
  in
  bake_op ~raise ~loc ~calltrace ctxt operation

let originate_contract : raise:r -> loc:Location.t -> calltrace:calltrace -> context -> value * value -> Z.t -> value * context =
  fun ~raise ~loc ~calltrace ctxt (contract, storage) amt ->
    let contract = trace_option ~raise (corner_case ()) @@ get_michelson_contract contract in
    let { code = storage ; ast_ty = ligo_ty ; _ } = trace_option ~raise (corner_case ()) @@ get_michelson_expr storage in
    let open Tezos_alpha_test_helpers in
    let source = unwrap_source ~raise ~loc ~calltrace ctxt.internals.source in
    let amt = try Some (Test_tez.of_mutez_exn (Int64.of_int (Z.to_int amt))) with _ -> None in
    let script = script_of_compiled_code ~raise ~loc ~calltrace contract storage in
    let (operation, dst) = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
      (* TODO : fee ? *)
      Op.contract_origination (B ctxt.raw) source ?credit:amt ~fee:(Test_tez.of_int 1) ~script
    in
    match bake_op ~raise ~loc ~calltrace ctxt operation with
    | Success (ctxt,_) ->
      let addr = v_address dst in
      let storage_tys = (dst, ligo_ty) :: (ctxt.internals.storage_tys) in
      (addr, {ctxt with internals = { ctxt.internals with storage_tys}})
    | Fail errs -> raise.raise (target_lang_error loc calltrace errs)

let get_bootstrapped_contract ~raise (n : int) =
  (* TODO-er: this function repeats work each time called... improve *)
  let rec foldnat s e = function
      0 -> e
    | k -> foldnat s (s e) (k - 1) in
  let open Tezos_raw_protocol.Contract_repr in
  let origination_nonce =
    let open Memory_proto_alpha.Protocol.Origination_nonce in
    let initial = initial (Tezos_crypto.Operation_hash.hash_bytes [Bytes.of_string "Un festival de GADT."]) in
    foldnat incr initial n
  in
  let contract = to_b58check (originated_contract origination_nonce) in
  let contract = Tezos_protocol.Protocol.Alpha_context.Contract.of_b58check contract in
  Trace.trace_alpha_tzresult ~raise (fun _ -> generic_error Location.generated "Error parsing address") @@ contract

let init ?rng_state ?commitments ?(initial_balances = []) ?(baker_accounts = []) ?(consensus_threshold=0)
    ?min_proposal_quorum ?bootstrap_contracts ?level ?cost_per_byte
    ?liquidity_baking_subsidy ?endorsing_reward_per_slot
    ?baking_reward_bonus_per_slot ?baking_reward_fixed_portion ?origination_size
    ?blocks_per_cycle n =
  let open Tezos_alpha_test_helpers in
  let accounts = Account.generate_accounts ?rng_state ~initial_balances n in
  let contracts =
    List.map
      ~f:(fun (a, _) -> Tezos_raw_protocol.Alpha_context.Contract.implicit_contract Account.(a.pkh))
      accounts
  in
  let baker_accounts = List.map baker_accounts ~f:(fun (sk, pk, amt) ->
                            let pkh = Signature.Public_key.hash pk in
                            let amt = match amt with
                              | None -> Tez.of_mutez_exn 4_000_000_000_000L
                              | Some v -> Tez.of_mutez_exn  v in
                            Account.({ sk ; pk ; pkh}), amt) in
  let () = List.iter baker_accounts ~f:(fun (acc, _) ->
               Account.(add_account acc)) in
  let accounts = accounts @ baker_accounts in
  let raw = Block.genesis
    ?commitments
    ~consensus_threshold
    ?min_proposal_quorum
    ?bootstrap_contracts
    ?level
    ?cost_per_byte
    ?liquidity_baking_subsidy
    ?endorsing_reward_per_slot
    ?baking_reward_bonus_per_slot
    ?baking_reward_fixed_portion
    ?origination_size
    ?blocks_per_cycle
    accounts in
  (raw, contracts)

let init_ctxt ~raise ?(loc=Location.generated) ?(calltrace=[]) ?(initial_balances=[]) ?(baker_accounts = []) ?(n=2) protocol_version bootstrapped_contracts =
  let open Tezos_raw_protocol in
  let rng_state = Caml.Random.State.make (Caml.Array.make 1 0) in
  let () = (* check baker initial balance if the default amount is changed *)
    match initial_balances with
    | [] -> () (* if empty list: will be defaulted with coherent values*)
    | baker::_ -> (
      let max = Tezos_protocol_parameters.Default_parameters.constants_test.tokens_per_roll in
      if (Tez.(<) (Alpha_context.Tez.of_mutez_exn baker) max) then raise.raise (Errors.not_enough_initial_accounts loc max) else ()
    )
  in
  (* DEPRECATED FOR NOW, delegate should become optional and this argument must be passed to init
     grep for DEPRECATED to find a commented test in ligo_interpreter_tests.ml
  let bootstrap_contracts =
    List.map
      ~f:(
        fun (mutez, contract, storage, _, _) : Alpha_context.Parameters.bootstrap_contract ->
          let script = script_of_compiled_code ~raise ~loc ~calltrace contract storage in
          {
            delegate = Signature.Public_key_hash.zero ;
            amount = Alpha_context.Tez.of_mutez_exn (Int64.of_int mutez) ;
            script ;
          }
      )
      bootstrapped_contracts
  in
  *)
  let storage_tys =
    List.mapi
      ~f:(fun i (_, _, _, _, storage_ty) -> let contract = get_bootstrapped_contract ~raise i in (contract, storage_ty))
      bootstrapped_contracts
  in
  let parameter_tys =
    List.mapi
      ~f:(fun i (_, _, _, parameter_ty, _) -> let contract = get_bootstrapped_contract ~raise i in (contract, parameter_ty))
      bootstrapped_contracts
  in
  let baker_accounts = List.map ~f:(fun (sk, pk, amt) ->
                            let sk =  Trace.trace_tzresult ~raise (fun _ -> Errors.generic_error ~calltrace loc "Cannot parse secret key") @@ Tezos_crypto.Signature.Secret_key.of_b58check sk in
                            sk, pk, amt) baker_accounts in
  let r, acclst = init ~rng_state ~level:(Int32.of_int_exn 0) ~initial_balances ~baker_accounts n in
  let init_raw_ctxt = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@ r in
  match acclst with
  | baker::source::_ ->
    let baker = unwrap_baker ~raise ~loc ~calltrace baker in
    let transduced = { last_originations = [] ; bigmaps= [] } in
    let internals = { protocol_version ; baker_policy = By_account baker ; source ; next_bootstrapped_contracts = [] ; next_baker_accounts = [] ; storage_tys ; parameter_tys ; bootstrapped = acclst } in
    { raw = init_raw_ctxt ; transduced ; internals }
  | _ ->
    raise.raise (bootstrap_not_enough loc)
