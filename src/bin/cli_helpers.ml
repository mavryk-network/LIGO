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
    with exn -> return := Exception exn;

