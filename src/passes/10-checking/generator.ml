open Simple_utils.Trace
module Errors = Errors
open Errors
module Signature = Context.Signature
module Elaboration = Context.Elaboration
module O = Ast_typed
open O.Combinators
open Ligo_prim

let default_entrypoint = "main"
let default_entrypoint_var = Value_var.of_input_var default_entrypoint

let program_sig_ ~raise : Signature.t -> (Signature.item * Signature.item) option =
  fun sig_ ->
  let f s = match s with
    | Signature.S_value (var, ty, entry) when entry -> Some (var, ty)
    | _ -> None in
  let open Simple_utils.Option in
  let* entries = List.Ne.of_list_opt @@ List.filter_map ~f sig_ in
  let parameter_type, storage_type =
    match Ast_typed.Misc.parameter_from_entrypoints entries with
    | Error (`Not_entry_point_form ep_type) ->
      raise.error
        (corner_case
        @@ Format.asprintf
             "Not an entrypoint form: %a"
             Ast_typed.PP.type_expression
             ep_type)
    | Error (`Storage_does_not_match (ep_1, storage_1, ep_2, storage_2)) ->
      raise.error
        (corner_case
        @@ Format.asprintf
             "@[<hv>Storage types do not match for different entrypoints:@.- %a : %a@.- %a : %a@]"
             Value_var.pp ep_1
             Ast_typed.PP.type_expression storage_1
             Value_var.pp ep_2
             Ast_typed.PP.type_expression storage_2)
    | Ok (p, s) -> p, s
  in
  let type_binder = Type_var.fresh ~name:"parameter" () in
  let parameter_type_decl = Signature.S_type (type_binder, parameter_type) in
  let contract_type = O.Misc.build_entry_type parameter_type storage_type in
  let contract_decl = Signature.S_value (default_entrypoint_var, contract_type, false) in
  return (parameter_type_decl, contract_decl)

let make_main_signature ~raise (sig_ : Signature.t) =
  sig_ @ match program_sig_ ~raise sig_ with
  | None -> []
  | Some (type_decl, main_decl) -> [type_decl; main_decl]
