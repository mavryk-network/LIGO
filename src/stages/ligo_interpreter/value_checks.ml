open Types

let json_check (b : bytes) =
  let open Simple_utils.Option in
  ignore b;
  return ()

type protocolInfo = { sha256hash : string option; uri : Uri.t }

let uri_check (b : bytes) =
  let open Simple_utils.Option in
  let s = Bytes.to_string b in
  let uri = Uri.of_string @@ s in
  let* scheme = Uri.scheme uri in
  let valid_protocols = ["http"; "https"; "ipfs"; "tezos-storage"] in
  if (List.mem ~equal:String.equal valid_protocols scheme) then
    return { sha256hash = None; uri }
  else if (String.equal "sha256" scheme) then
    let uri_path = Uri.path uri in
    let uri_path = String.chop_prefix_if_exists uri_path ~prefix:"/" in
    let sha256hash = Uri.host uri in
    let uri = Uri.(of_string @@ pct_decode uri_path) in
    return { sha256hash ; uri }
  else
    None

let tzip16_check (v : value) =
  let open Simple_utils.Option in
  let* values = Combinators.get_record v in
  let* metadata = Ligo_prim.(Record.find_opt values Label.(of_string "metadata")) in
  let* values = Combinators.get_map metadata in
  let* values = all @@ List.map ~f:(fun (k, v) ->
      let* k = Combinators.get_string k in
      let* v = Combinators.get_bytes v in
      return (k, v)) values in
  let* _, root = List.find ~f:(fun (k, _) -> String.equal k "") values in
  let* { uri ; _ } = uri_check root in
  let* protocol = Uri.scheme uri in
  match protocol with
  | "tezos-storage" -> (
    match Uri.host uri with
    | Some _ ->
      return ()
    | None ->
      let location = Uri.path uri in
      let location = String.chop_prefix_if_exists location ~prefix:"/" in
      let* () = some_if (not @@ String.contains location '/') () in
      let location = Uri.pct_decode location in
      let* _, json = List.find ~f:(fun (k, _) -> String.equal k location) values in
      let* () = json_check json in
      return ()
  )
  | _ ->
    return ()

let all_check ~(options : Compiler_options.middle_end) ~type_ (v : value) =
  let open Simple_utils.Option in
  if not options.no_metadata_check
  then
    match Self_ast_aggregated.find_storage_metadata_opt type_ with
    | Some metadata when Self_ast_aggregated.is_metadata_tzip16_type_valid metadata ->
      tzip16_check v
    | _ -> return ()
  else return ()
