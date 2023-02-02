let specialise_and_print_pascaligo p =
  ignore p ; failwith "TODO: deprecate decompilation?"
  (* let ast = Self_ast_imperative.decompile_imperative p in
  let cst = Tree_abstraction.Pascaligo.decompile_declarations ast in
  let source =
    Parsing.Pascaligo.pretty_print
      Parsing.Pascaligo.CST.{ decl = cst; eof = Lexing_pascaligo.Token.ghost_eof }
  in
  source *)


let specialise_and_print_expression_pascaligo expression =
  ignore expression ; failwith "TODO: deprecate decompilation?"
  (* let ast = Self_ast_imperative.decompile_imperative_expression expression in
  let cst = Tree_abstraction.Pascaligo.decompile_expression ast in
  let source = Parsing.Pascaligo.pretty_print_expression cst in
  source *)


let specialise_and_print_cameligo m =
  ignore m ; failwith "TODO: deprecate decompilation?"
  (* let cst = Tree_abstraction.Cameligo.decompile_program m in
  let source = Parsing.Cameligo.pretty_print cst in
  source *)


let specialise_and_print_expression_cameligo expression =
  ignore expression ; failwith "TODO: deprecate decompilation?"
  (* let cst = Tree_abstraction.Cameligo.decompile_expression expression in
  let source = Parsing.Cameligo.pretty_print_expression cst in
  source *)


let specialise_and_print_jsligo m =
  ignore m ; failwith "TODO: deprecate decompilation?"
  (* let ast = Self_ast_imperative.decompile_imperative m in
  let cst = Tree_abstraction.Jsligo.decompile_program ast in
  let source = Parsing.Jsligo.pretty_print cst in
  source *)


let specialise_and_print_expression_jsligo expression =
  ignore expression ; failwith "TODO: deprecate decompilation?"
  (* let ast = Self_ast_imperative.decompile_imperative_expression expression in
  let cst = Tree_abstraction.Jsligo.decompile_expression ast in
  let b = Buffer.create 100 in
  List.fold
    ~f:(fun all x ->
      let source = Parsing.Jsligo.pretty_print_expression x in
      Buffer.add_buffer all source;
      b)
    ~init:b
    cst *)


let specialise_and_print (syntax : Syntax_types.t) source : Buffer.t =
  let specialise_and_print =
    match syntax with
    | PascaLIGO -> specialise_and_print_pascaligo
    | CameLIGO -> specialise_and_print_cameligo
    | JsLIGO -> specialise_and_print_jsligo
  in
  specialise_and_print source


let specialise_and_print_expression (syntax : Syntax_types.t) source =
  let specialise_and_print =
    match syntax with
    | PascaLIGO -> specialise_and_print_expression_pascaligo
    | CameLIGO -> specialise_and_print_expression_cameligo
    | JsLIGO -> specialise_and_print_expression_jsligo
  in
  specialise_and_print source
