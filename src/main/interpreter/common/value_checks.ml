open Ligo_interpreter
open Types

let json_check ~raise (b : bytes) =
  ignore b; ignore raise;
  ()

type protocolInfo = { sha256hash : string option; uri : Uri.t }

let uri_check (b : bytes) =
  let valid_protocols = ["http"; "https"; "ipfs"; "tezos-storage"] in
  let open Simple_utils.Option in
  let s = Bytes.to_string b in
  let uri = Uri.of_string @@ s in
  let* scheme = Uri.scheme uri in
  if (List.mem ~equal:String.equal valid_protocols scheme) then
    return { sha256hash = None; uri }
  else if (String.equal "sha256" scheme) then
    let uri_path = Uri.path uri in
    let uri_path = String.chop_prefix_if_exists uri_path ~prefix:"/" in
    let sha256hash = Uri.host uri in
    let uri = Uri.(of_string @@ pct_decode uri_path) in
    let* scheme = Uri.scheme uri in
    if (List.mem ~equal:String.equal valid_protocols scheme) then
      return { sha256hash ; uri }
    else
      None
  else
    None

let tzip16_check ~(raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise) ~loc (v : value) =
  let open Simple_utils.Trace in
  let open Simple_utils.Option in
  (* all these errors are okay as we check in Self_ast_aggregated.is_metdata_tzip16_type_valid that the
     type is a big_map *)
  let values = trace_option ~raise (Errors.generic_error loc "Expected storage value to be a record") @@
    Combinators.get_record v in
  let metadata = trace_option ~raise (Errors.generic_error loc "Could not find metadata in storage value") @@
    Ligo_prim.(Record.find_opt values Label.(of_string "metadata")) in
  let values = trace_option ~raise (Errors.generic_error loc "Expected metdata value to be a map") @@
    Combinators.get_map metadata in
  let values = List.map ~f:(fun (k, v) ->
      let k = trace_option ~raise (Errors.generic_error loc "Expected metdata key to be a string") @@
        Combinators.get_string k in
      let v = trace_option ~raise (Errors.generic_error loc "Expected metdata value to be a string") @@
        Combinators.get_bytes v in
      (k, v)) values in
  match List.find ~f:(fun (k, _) -> String.equal k "") values with
  | None ->
    raise.warning (`Metadata_no_empty_key loc)
  | Some (_, root) -> (
    match uri_check root with
    | Some { uri ; _ } -> (
        let protocol = trace_option ~raise (Errors.generic_error loc "Could not find protocol scheme in URI metdata") @@
          Uri.scheme uri in
        match protocol with
        | "tezos-storage" -> (
            match Uri.host uri with
            | Some _ -> ()
            | None ->
              let location = Uri.path uri in
              let location = String.chop_prefix_if_exists location ~prefix:"/" in
              let () = trace_option ~raise (Errors.generic_error loc "Slash ('/') not allowed in URI metdata") @@
                some_if (not @@ String.contains location '/') () in
              let location = Uri.pct_decode location in
              match List.find ~f:(fun (k, _) -> String.equal k location) values with
              | Some (_, json) -> json_check ~raise json
              | None -> raise.warning (`Metadata_tezos_storage_not_found (loc, location))
          )
        | _ -> ()
      )
      | None -> raise.warning (`Metadata_not_valid_URI (loc, Bytes.to_string root))
  )


let all_check ~raise ~(options : Compiler_options.middle_end) ~type_ ~loc (v : value) =
  if not options.no_metadata_check
  then
    match Self_ast_aggregated.find_storage_metadata_opt type_ with
    | Some metadata when Self_ast_aggregated.is_metadata_tzip16_type_valid metadata ->
      tzip16_check ~raise ~loc v
    | _ -> ()
  else ()
