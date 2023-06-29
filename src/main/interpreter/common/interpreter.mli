module Value_checks = Value_checks

type interpreter_error = Errors.interpreter_error

val eval_expression
  :  raise:(interpreter_error, Main_warnings.all) Simple_utils.Trace.raise
  -> steps:int
  -> options:Compiler_options.t
  -> ?self_pass:bool
  -> Ast_typed.program
  -> Ast_typed.expression
  -> bool * Ligo_interpreter.Types.value

val eval_test
  :  raise:(interpreter_error, Main_warnings.all) Simple_utils.Trace.raise
  -> steps:int
  -> options:Compiler_options.t
  -> Ast_typed.program
  -> bool * Ligo_interpreter.Types.toplevel_env

module Monad = Execution_monad

val compile_value
  :  raise:(interpreter_error, _) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> loc:Simple_utils.Location.t
  -> Monad.LT.value
  -> Ast_aggregated.type_expression
  -> Monad.LT.typed_michelson_code
