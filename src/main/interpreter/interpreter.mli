type interpreter_error = Errors.interpreter_error

val eval : raise:interpreter_error Simple_utils.Trace.raise ->
  steps:int -> options:Compiler_options.t -> Ast_aggregated.expression -> Ligo_interpreter.Types.value

val eval_test : raise:interpreter_error Simple_utils.Trace.raise
    -> steps:int
    -> options:Compiler_options.t
    -> Ast_typed.program
    -> (string * Ligo_interpreter.Types.value) list
