open Main_errors
open Tezos_utils
open Proto_alpha_utils
open Trace


let check_view_restrictions ~raise : Stacking.compiled_expression list -> unit = fun views_mich ->
  (* From Tezos changelog on views: 
    CREATE_CONTRACT, SET_DELEGATE and TRANSFER_TOKENS cannot be used at the top-level of a
    view because they are stateful, and SELF because the entry-point does not make sense in a view.
    However, CREATE_CONTRACT, SET_DELEGATE and TRANSFER_TOKENS remain available in lambdas defined inside a view. (MR !3737)
  *)
  let open Tezos_micheline.Micheline in
  let rec iter_prim_mich : inside_lambda:bool -> (inside_lambda:bool -> 'loc * 'p -> unit) -> ('loc,'p) node -> unit = fun ~inside_lambda f m ->
    match m with
    | Int _ | String _ | Bytes _ -> ()
    | Seq (_,x) -> List.iter ~f:(iter_prim_mich ~inside_lambda f) x
    | Prim (loc,p,lst,_) ->
      f ~inside_lambda (loc,p) ;
      let inside_lambda = if String.equal p "LAMBDA" then true else false in
      List.iter ~f:(iter_prim_mich ~inside_lambda f) lst
  in
  let iter_prim_mich = iter_prim_mich ~inside_lambda:false in
  let f ~inside_lambda : Mini_c.meta * string -> unit = fun ( {location;_} , prim_str ) ->
    match prim_str, inside_lambda with
    | "SELF" , (true | false) -> raise.raise (`Main_view_rule_violated location)
    | ("CREATE_CONTRACT" | "SET_DELEGATE" | "TRANSFER_TOKENS") , false -> raise.raise (`Main_view_rule_violated location)
    | _ -> ()
  in
  List.iter ~f:(fun m -> iter_prim_mich f m.expr) views_mich

let parse_constant ~raise code =
  let open Tezos_micheline in
  let open Tezos_micheline.Micheline in
  let (code, errs) = Micheline_parser.tokenize code in
  let code = (match errs with
              | _ :: _ -> raise.raise (unparsing_michelson_tracer @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
              | [] ->
                 let (code, errs) = Micheline_parser.parse_expression ~check:false code in
                 match errs with
                 | _ :: _ -> raise.raise (unparsing_michelson_tracer @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
                 | [] -> map_node (fun _ -> ()) (fun x -> x) code
             ) in
  Trace.trace_alpha_tzresult ~raise unparsing_michelson_tracer @@
    Memory_proto_alpha.node_to_canonical code

let dummy : Stacking.meta =
  { location = Location.dummy;
    env = [];
    binder = None }

(* should preserve locations, currently wipes them *)
let build_contract ~raise ~add_warning :
  protocol_version:Environment.Protocols.t ->
  ?disable_typecheck:bool ->
  ?constants:string list ->
  Stacking.compiled_expression ->
  (Ast_typed.expression_variable * Stacking.compiled_expression) list -> _ Michelson.michelson  =
    fun ~protocol_version ?(disable_typecheck= false) ?(constants = []) compiled views ->
      let views =
        List.map
          ~f:(fun (name, view) ->
            let (view_param_ty, ret_ty) = trace_option ~raise (main_view_not_a_function name) @@ (* remitodo error specific to views*)
              Self_michelson.fetch_views_ty view.expr_ty
            in
            (Ast_typed.ValueVar.to_name_exn name, view_param_ty, ret_ty, view.expr)
          )
          views
      in
      let (param_ty, storage_ty) = trace_option ~raise (main_entrypoint_not_a_function) @@
        Self_michelson.fetch_contract_ty_inputs compiled.expr_ty in
      let expr = compiled.expr in
      let contract =
        Michelson.lcontract
          dummy
          dummy param_ty
          dummy storage_ty
          dummy expr
          dummy views in
      if disable_typecheck then
        contract
      else
        let contract' =
          Trace.trace_tzresult_lwt ~raise (typecheck_contract_tracer contract)
            (Memory_proto_alpha.prims_of_strings contract) in
        let environment = Proto_alpha_utils.Memory_proto_alpha.dummy_environment () in
        (* Parse constants *)
        let constants = List.map ~f:(parse_constant ~raise) constants in
        (* Update the Tezos context by registering the global constants *)
        let tezos_context = List.fold_left constants ~init:environment.tezos_context
                    ~f:(fun ctxt cnt ->
                      let (ctxt, _, _) = Trace.trace_alpha_tzresult_lwt ~raise (typecheck_contract_tracer contract) @@
                                           Proto_alpha_utils.Memory_proto_alpha.register_constant ctxt cnt in
                      ctxt) in
        let environment = { environment with tezos_context } in
        (* Type-check *)
        let () =
          if Environment.Protocols.(not @@ equal protocol_version in_use) then
            Trace.warn_on_tzresult_lwt ~add_warning (fun errs -> `Michelson_typecheck_failed_with_different_protocol (protocol_version, errs)) @@
              Proto_alpha_utils.Memory_proto_alpha.typecheck_contract ~environment contract'
          else
            let _ : (_,_) Micheline.Micheline.node =
              Trace.trace_tzresult_lwt ~raise (typecheck_contract_tracer contract) @@
                Proto_alpha_utils.Memory_proto_alpha.typecheck_contract ~environment contract'
            in
            ()
        in
        contract

let measure ~raise = fun m ->
  Trace.trace_tzresult_lwt ~raise (main_could_not_serialize) @@
    Proto_alpha_utils.Measure.measure m
