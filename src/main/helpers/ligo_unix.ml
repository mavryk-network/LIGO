[@@@alert "-deprecated"]
(* because the alternative is to use core_kernel to include caml_unix which breaks things further on windows *)
include Caml.Unix
let ls_dir directory = Array.to_list (Caml.Sys.readdir directory)
let home_directory () =
  match Caml.Sys.getenv_opt "HOME" with
  | Some home -> home
  | None when Caml.Sys.win32-> (match Caml.Sys.getenv_opt "USERPROFILE", Caml.Sys.getenv "HOMEDRIVE", Caml.Sys.getenv "HOMEPATH" with
                          | Some home, _, _ -> home
                          | Some _, home_drive, home_path -> home_drive ^ home_path
                          | _ -> "")
  | None -> ""
