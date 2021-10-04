(* Interfacing the preprocessor *)

(* Directories and files *)

type file_path = string
type dirs = file_path list (* #include and #import *)

type module_name = string
type module_resolutions = (module_name * file_path) list

(* Results *)

module Errors = Preprocessing_shared.Errors

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
