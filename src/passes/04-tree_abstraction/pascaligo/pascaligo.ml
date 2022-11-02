module CST = Cst.Pascaligo
module AST = Ast_imperative

module Compiler   = Compiler
module Errors = Errors

let compile_program = Compiler.compile_program
let compile_expression = Compiler.compile_expression ~attr:[]

let decompile_declarations _ = failwith "Nah"
let decompile_expression _ = failwith "Nah"

let decompile_pattern_to_string _ = failwith "Nah"