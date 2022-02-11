type interpreter_error = Errors.interpreter_error

val eval_test : raise:interpreter_error Simple_utils.Trace.raise
    -> steps:int
    -> options:Compiler_options.t
    -> protocol_version:Environment.Protocols.t
    -> Ast_typed.program
    -> (string * Ligo_interpreter.Types.value) list
