open Ligo_interpreter
open Types

let json_check ~loc (b : bytes) =
  let open Simple_utils.Result in
  try
    let _ = Yojson.Basic.from_string Bytes.(to_string b) in
    Ok ()
  with
  | Yojson.Json_error e -> fail (`Metadata_invalid_JSON (loc, e))
  | _ -> Ok ()


type protocolInfo =
  { sha256hash : string option
  ; uri : Uri.t
  ; scheme : string
  }

let uri_check (b : bytes) =
  let valid_protocols = [ "http"; "https"; "ipfs"; "tezos-storage" ] in
  let open Simple_utils.Option in
  let s = Bytes.to_string b in
  let uri = Uri.of_string @@ s in
  let* scheme = Uri.scheme uri in
  if List.mem ~equal:String.equal valid_protocols scheme
  then return { sha256hash = None; uri; scheme }
  else if String.equal "sha256" scheme
  then (
    let uri_path = Uri.path uri in
    let uri_path = String.chop_prefix_if_exists uri_path ~prefix:"/" in
    let sha256hash = Uri.host uri in
    let uri = Uri.(of_string @@ pct_decode uri_path) in
    let* scheme = Uri.scheme uri in
    if List.mem ~equal:String.equal valid_protocols scheme
    then return { sha256hash; uri; scheme }
    else None)
  else None


let destruct_values ~raise ~loc (v : value) =
  let open Simple_utils.Trace in
  (* all these errors are okay as we check in Self_ast_aggregated.is_metdata_tzip16_type_valid that the
     type is a big_map *)
  let values =
    trace_option ~raise (Errors.generic_error loc "Expected storage value to be a record")
    @@ Combinators.get_record v
  in
  let metadata =
    trace_option
      ~raise
      (Errors.generic_error loc "Could not find metadata in storage value")
    @@ Ligo_prim.(Record.find_opt values Label.(of_string "metadata"))
  in
  let values =
    trace_option ~raise (Errors.generic_error loc "Expected metdata value to be a map")
    @@ Combinators.get_map metadata
  in
  let values =
    List.map
      ~f:(fun (k, v) ->
        let k =
          trace_option
            ~raise
            (Errors.generic_error loc "Expected metdata key to be a string")
          @@ Combinators.get_string k
        in
        let v =
          trace_option
            ~raise
            (Errors.generic_error loc "Expected metdata value to be a string")
          @@ Combinators.get_bytes v
        in
        k, v)
      values
  in
  values


let tzip16_check
    ~(raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise)
    ~loc
    (v : value)
  =
  let open Simple_utils.Result in
  let values = destruct_values ~raise ~loc v in
  let* _, root =
    of_option ~error:(`Metadata_no_empty_key loc)
    @@ List.find ~f:(fun (k, _) -> String.equal k "") values
  in
  let* { uri; scheme; _ } =
    of_option ~error:(`Metadata_not_valid_URI (loc, Bytes.to_string root))
    @@ uri_check root
  in
  match scheme with
  | "tezos-storage" ->
    (match Uri.host uri with
    | None ->
      (* In case of empty host, the context is current contract (and thus current storage value) *)
      let location = Uri.path uri in
      let location = String.chop_prefix_if_exists location ~prefix:"/" in
      let* () =
        of_option ~error:(`Metadata_slash_not_valid_URI (loc, location))
        @@ Simple_utils.Option.some_if (not @@ String.contains location '/') ()
      in
      let location = Uri.pct_decode location in
      let* _, json =
        of_option ~error:(`Metadata_tezos_storage_not_found (loc, location))
        @@ List.find ~f:(fun (k, _) -> String.equal k location) values
      in
      json_check ~loc json
    | Some _ -> return ())
  | _ -> return ()


let all_check ~raise ~(options : Compiler_options.middle_end) ~type_ ~loc (v : value) =
  if not options.no_metadata_check
  then (
    match Self_ast_aggregated.find_storage_metadata_opt type_ with
    | Some metadata when Self_ast_aggregated.is_metadata_tzip16_type_valid metadata ->
      let open Simple_utils.Trace in
      (match tzip16_check ~raise ~loc v with
      | Ok () -> ()
      | Error w -> raise.warning w)
    | _ -> ())
  else ()
