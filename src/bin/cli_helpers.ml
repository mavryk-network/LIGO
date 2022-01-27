module Constants = struct
  type command = (string * string array)
  let ligo_install_path = "./.ligo"
  let esy = "esy"
  let windows = "Win32"
  let esy_add = fun ~package_name ~cache_path -> 
    ("", [|"esy"; "add"; package_name; "--prefix-path"; cache_path|])
  let esy_install = fun ~cache_path -> 
    ("", [|"esy"; "install"; "--prefix-path"; cache_path|])
  let where = fun ~cmd -> 
    ("", [|"where"; "/q"; cmd|])
  let which = fun ~cmd -> 
    ("", [|"which"; cmd|])
end


let return_good ?output_file v = 
  let fmt : Format.formatter = match output_file with
    | Some file_path -> Format.formatter_of_out_channel @@ Out_channel.create file_path
    | None -> Format.std_formatter in
  Format.fprintf fmt "%s\n" v; Format.pp_print_flush fmt ()

let return_bad v : unit = (
  if Char.(v.[String.length v - 1] = '\n') then
    Format.eprintf "%s" v
  else
    Format.eprintf "%s\n" v;
    Format.pp_print_flush Format.err_formatter ();
  )
let return_with_warn ~warn warns f =
  if not (String.length (String.strip warns) = 0) && warn then
    begin
      Format.eprintf "%s\n" warns;
      Format.pp_print_flush Format.err_formatter ()
    end;
  f ()

type return = Done | Compileur_Error | Exception of exn
let return_result : return:return ref -> ?warn:bool -> ?output_file:string ->(unit -> ('value, _) result) -> unit =
  fun ~return ?(warn=false) ?output_file f ->
    try 
      match f () with
      | Ok    (v,w) -> return:=Done; return_with_warn ~warn w (fun () -> return_good ?output_file v)
      | Error (e,w) -> return:=Compileur_Error; return_with_warn ~warn w (fun () -> return_bad e)
    with exn -> return := Exception exn;;

type command = (string * string array)

(* Checks if executable is present *)
let does_command_exist (cmd : string) =
  let cmd = 
    if String.equal Sys.os_type Constants.windows then
      Constants.where ~cmd
    else
      Constants.which ~cmd in
  let exit = Lwt_process.exec cmd in
  let status = Lwt_main.run exit in
  match status with
    WEXITED 0 -> Ok true
  | WEXITED 1 -> Ok false
  | _ -> Error "unknown error"

(* Runs a commands in a separate process *)
let run_command (cmd : command) =
  let status = Lwt_process.with_process_none ~stdout:`Keep ~stderr:`Keep cmd 
    (fun p -> Lwt.map  
      (fun status -> 
        match status with
          Caml.Unix.WEXITED 0 -> Ok ()
        | _ -> Error ("unknown error"))
        p#status) in
  Lwt_main.run status