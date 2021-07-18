(* This module is meant to be used by clients of the library to create
   standalone preprocessors tailored to their conventions. It is also
   internally used by PreprocMain.ml with default settings, for
   testing purposes. *)

module Make (CLI : CLI.S) :
  sig
    val check_cli  : unit -> unit
    val config     : API.config
    val preprocess : unit -> API.result
  end
