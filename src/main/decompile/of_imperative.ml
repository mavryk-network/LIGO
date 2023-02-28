open Helpers
open Ast_imperative

let decompile ~raise ~deprecated (m : program) syntax : _ =
  let syntax = Syntax.of_string_opt ~raise ~deprecated syntax None in
  specialise_and_print syntax m


let decompile_expression (e : expression) syntax : _ =
  specialise_and_print_expression syntax e
