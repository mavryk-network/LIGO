module CST = Cst.Jsligo
module AST = Ast_imperative

module Compiler   = Compiler
module Errors = Errors

let compile_program     = Compiler.compile_program
let compile_expression = Compiler.compile_expression

let decompile_program   _ = failwith "Nah"
let decompile_expression _ = failwith "Nah"

let decompile_pattern_to_string _p = failwith "Nah"
