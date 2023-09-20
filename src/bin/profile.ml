module LigoRC = Cli_helpers.LigoRC

let get_auth_token ~ligorc_path ligo_registry =
  let ligorc = LigoRC.read ~ligorc_path in
  let registry_key = LigoRC.registry_key ligo_registry in
  let token = LigoRC.get_token ~registry_key ligorc in
  match token with
  | Some token -> Ok token
  | None -> Error ()


let setup ~ligorc_path ~ligo_registry =
  let ( let* ) = Caml.Result.bind in
  let* token, _ =
    match get_auth_token ~ligorc_path ligo_registry with
    | Error () -> Error ("You're not logged in", "")
    | Ok token -> Ok (token, "")
  in
  let prompt () =
    let open Lwt_result.Syntax in
    let* email = Prompt.prompt ~msg:"Email: " in
    let* fullname = Prompt.prompt ~msg:"Full name (optional. Press ENTER to skip): " in
    Lwt_result.return (email, fullname)
  in
  let setup_profile ~token ~email ~fullname =
    let open Registry.Profile in
    match Lwt_main.run @@ create ~token ~ligo_registry ~email ~fullname with
    | Ok () ->
      Ok
        ( "Your profile has been created. To verify your address, we have sent you an\n\
           email. Please follow the instructions there. You may run this command again\n\
           after verifying your address."
        , "" )
    | Error Invalid_input -> Error ("Invalid input", "")
    | Error Invalid_token -> Error (Printf.sprintf "Session is no longer valid", "")
    | Error Server_error ->
      Error ("Registry is having issues right now. Please try again later", "")
  in
  match Lwt_main.run @@ prompt () with
  | Ok (email, fullname) -> setup_profile ~token ~email ~fullname
  | Error (Prompt.Unknown_error e) -> Error (Exn.to_string e, "")
  | Error Prompt.Cancelled -> Error ("Aborted", "")
  | Error Prompt.Not_tty -> Error ("Not an interactive terminal", "")
