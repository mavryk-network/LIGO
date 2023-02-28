module Trace = Simple_utils.Trace

val of_string_opt
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> deprecated:bool
  -> Syntax_types.s_syntax
  -> string option
  -> Syntax_types.t

val to_string : Syntax_types.t -> string
val to_ext : Syntax_types.t -> string
val of_ext_opt : deprecated:bool -> string option -> Syntax_types.t option
