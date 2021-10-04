(* PREPROCESSING *)

(* Directories and files *)

type file_path = string
type dirs = file_path list (* #include and #import *)

type module_name = string
type module_resolutions = (module_name * file_path) list

module Make (File : File.S) (Comments : Comments.S) :
  sig
    (* Directories and files *)

    type nonrec file_path = file_path
    type nonrec dirs = dirs

    type nonrec module_name = module_name
    type nonrec module_resolutions = module_resolutions

    (* Results *)

    module Errors = Errors

    type success = Preprocessor.API.success
    type nonrec result  = (success, Errors.t) result

    (* Preprocessing various sources *)

    val from_file    : dirs -> module_resolutions -> file_path  -> result
    val from_string  : dirs -> module_resolutions -> string     -> result
    val from_channel : dirs -> module_resolutions -> in_channel -> result

    (* Aliases *)

    val preprocess_file    : dirs -> module_resolutions -> file_path  -> result
    val preprocess_string  : dirs -> module_resolutions -> string     -> result
    val preprocess_channel : dirs -> module_resolutions -> in_channel -> result
  end

(* For further passes *)

module type FILE =
  sig
    include File.S
    val input              : file_path option
    val dirs               : dirs
    val module_resolutions : module_resolutions
  end
