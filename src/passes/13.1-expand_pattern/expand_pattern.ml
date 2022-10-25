let compile : Ast_aggregated.expression -> Ast_pattern_expanded.expression 
= fun e ->
(* let () = Format.eprintf "before compiling pattern -> %a" Ast_aggregated.PP.expression e in *)
Compiler.compile_expression e

let decompile : Ast_pattern_expanded.expression -> Ast_aggregated.expression = Decompiler.decompile_expression