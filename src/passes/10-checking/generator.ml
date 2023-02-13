module Errors = Errors
open Errors
module Signature = Context.Signature
module O = Ast_typed
module C = Computation
open O.Combinators
open Ligo_prim

let default_entrypoint = "main"
let default_entrypoint_var = Value_var.of_input_var ~loc:Location.generated default_entrypoint
let default_views = "views"
let default_views_var = Value_var.of_input_var ~loc:Location.generated default_entrypoint

let program_sig_ : Signature.t -> ((Signature.item * Signature.item) option, _, _) C.t =
  fun sig_ ->
  let is_entry s = match s with
    | Signature.S_value (var, ty, attr) when attr.entry -> Some (var, ty)
    | _ -> None in
  let is_view s = match s with
    | Signature.S_value (var, ty, attr) when attr.view -> Some (var, ty)
    | _ -> None in
  let open C.Let_syntax in
  match List.Ne.of_list_opt @@ List.filter_map ~f:is_entry sig_ with
  | None -> return None
  | Some entries ->
    let%bind parameter_type, storage_type =
      match Type.parameter_from_entrypoints entries with
      | Error (`Not_entry_point_form ep_type) ->
        C.raise
          (corner_case
           @@ Format.asprintf
             "Not an entrypoint form: %a"
             Type.pp
             ep_type)
      | Error (`Storage_does_not_match (ep_1, storage_1, ep_2, storage_2)) ->
        C.raise
          (corner_case
           @@ Format.asprintf
             "@[<hv>Storage types do not match for different entrypoints:@.- %a : %a@.- %a : %a@]"
             Value_var.pp ep_1
             Type.pp storage_1
             Value_var.pp ep_2
             Type.pp storage_2)
      | Ok (p, s) -> return (p, s)
    in
    let type_binder = Type_var.fresh ~name:"parameter" ~loc:Location.generated () in
    let parameter_type_decl = Signature.S_type (type_binder, parameter_type) in
    let contract_type = Type.build_entry_type parameter_type storage_type in
    let contract_decl = Signature.S_value (default_entrypoint_var, contract_type, Context.Attr.default) in
    return (Some (parameter_type_decl, contract_decl))
 
let make_main_signature (sig_ : Signature.t) =
  let open C.Let_syntax in
  match%bind program_sig_ sig_ with
  | None -> return sig_
  | Some (type_decl, main_decl) -> return @@ sig_ @  [type_decl; main_decl]
