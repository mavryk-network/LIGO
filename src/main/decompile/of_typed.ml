let decompile_expression
    ~(options : Compiler_options.middle_end)
    (e : Ast_typed.expression)
    : Ast_core.expression
  =
  Checking.untype_expression ~options e
