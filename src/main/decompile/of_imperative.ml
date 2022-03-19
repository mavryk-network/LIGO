open Ast_imperative
open Helpers

let decompile ~raise ?dialect (m : module_) syntax : _  =
  let syntax = Syntax.of_string_opt ~raise ?dialect syntax None in
  specialise_and_print syntax m

let decompile_expression (e : expression) syntax : _  =
  specialise_and_print_expression syntax e
