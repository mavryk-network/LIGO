open Simple_utils.Trace
open Main_errors

let decompile_type ~raise : Ast_aggregated.type_expression -> Ast_typed.type_expression = fun t ->
  trace ~raise aggregation_tracer @@ Aggregation.decompile_type t
