open Simple_utils.Trace
open Main_errors

let decompile_type ~raise : Mini_c.type_expression -> Ast_aggregated.type_expression = fun t ->
  trace ~raise spilling_tracer @@ Spilling.decompile_type t