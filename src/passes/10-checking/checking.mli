
module I = Ast_core
module O = Ast_typed

module Errors = Errors
open Errors
open Simple_utils.Trace


val type_program     : raise:typer_error raise -> options:Compiler_options.middle_end -> ?env:Environment.t -> I.module_ -> O.program
val type_declaration : raise:typer_error raise -> options:Compiler_options.middle_end -> ?env:Environment.t -> I.declaration Location.wrap -> O.declaration Location.wrap
val type_expression  : raise:typer_error raise -> options:Compiler_options.middle_end -> ?env:Environment.t -> ?tv_opt:O.type_expression -> I.expression -> O.expression


val untype_expression : O.expression -> I.expression
val untype_declaration : O.declaration -> I.declaration
val untype_declaration_loc : O.declaration_loc -> I.declaration_loc
val untype_program : O.program -> I.module_

val assert_type_expression_eq : raise:typer_error raise -> Location.t -> O.type_expression * O.type_expression -> unit
