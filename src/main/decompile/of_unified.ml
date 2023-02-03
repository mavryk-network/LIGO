open Helpers
open Ast_unified

let decompile ~raise (m : program) syntax : _ =
  let syntax = Syntax.of_string_opt ~raise syntax None in
  specialise_and_print syntax m


let decompile_expression (e : expr) syntax : _ =
  specialise_and_print_expression syntax e
