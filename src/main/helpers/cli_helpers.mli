module Constants : sig 
  type command = (string * string array)
  val esy : string
  val ligo_install_path : string
  val esy_add : package_name:string -> cache_path:string -> command
  val esy_install : cache_path:string -> command
  val git_clone : project_url:string  -> project_name:string  -> command
  val git_checkout : dir_path:string  -> ref:string  -> command
end

type return = Done | Compileur_Error | Exception of exn
val return_result : return:return ref -> ?show_warnings:bool -> ?output_file:string -> (unit -> (string*string,string*string) result) -> unit

type command = (string * string array)

(* Checks if executable is present *)
val does_command_exist : string -> (bool, string) result

(* Runs a commands in a separate process *)
val run_command : command -> (unit, string) result