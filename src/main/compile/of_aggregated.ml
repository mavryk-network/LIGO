open Simple_utils.Trace
open Ast_aggregated
open Expand_pattern
open Main_errors

module SMap = Map.Make(String)

let compile_expression ~raise : Ast_aggregated.expression -> Ast_pattern_expanded.expression = fun e ->
  compile e
