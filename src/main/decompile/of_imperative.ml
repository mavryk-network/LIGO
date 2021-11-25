open Ast_imperative
open Helpers
open File_metadata
open Trace

let decompile ~raise ?dialect (m : module_) syntax : _  =
  let syntax = trace ~raise Main_errors.meta_tracer @@ syntax_to_variant ?dialect syntax None in
  specialise_and_print syntax m

let decompile_expression (e : expression) syntax : _  =
  specialise_and_print_expression syntax e
