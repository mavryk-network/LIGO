type interpreter_error = Errors.interpreter_error

val eval_test : raise:interpreter_error Simple_utils.Trace.raise
    -> steps:int
    -> options:Compiler_options.t
    -> protocol_version:Environment.Protocols.t
    -> Ast_typed.module_
    -> (Ast_aggregated.module_variable * Ligo_interpreter.Types.value) list

val test_lib : raise:interpreter_error Simple_utils.Trace.raise -> options:Compiler_options.t -> Environment.lib
