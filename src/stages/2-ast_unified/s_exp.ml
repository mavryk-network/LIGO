open Types
open Simple_utils.Function

let rec sexp_of_ty_expr (x : ty_expr) : Sexp.t =
  Types.sexp_of_ty_expr_ sexp_of_ty_expr x.fp
and sexp_of_pattern (x : pattern) : Sexp.t =
  Types.sexp_of_pattern_ sexp_of_pattern sexp_of_ty_expr x.fp
and sexp_of_instruction (x : instruction) : Sexp.t =
  Types.sexp_of_instruction_ sexp_of_instruction sexp_of_expr sexp_of_pattern sexp_of_statement x.fp
and sexp_of_statement (x : statement) : Sexp.t =
  Types.sexp_of_statement_ sexp_of_statement sexp_of_instruction sexp_of_declaration x.fp
and sexp_of_declaration (x : declaration) : Sexp.t =
  Types.sexp_of_declaration_ sexp_of_declaration sexp_of_expr sexp_of_ty_expr sexp_of_pattern sexp_of_mod_expr x.fp
and sexp_of_mod_expr (x : mod_expr) : Sexp.t =
  Types.sexp_of_mod_expr_ sexp_of_mod_expr sexp_of_statement sexp_of_declaration x.fp
and sexp_of_expr : expr -> Sexp.t = fun e ->
  Types.sexp_of_expr_
    sexp_of_expr
    sexp_of_ty_expr
    sexp_of_pattern
    sexp_of_statement
    sexp_of_mod_expr
    e.fp


let rec ty_expr_of_sexp (s : Sexp.t) : ty_expr =
  { fp = (Types.ty_expr__of_sexp ty_expr_of_sexp s) }
and pattern_of_sexp (s : Sexp.t) : pattern =
  { fp = Types.pattern__of_sexp pattern_of_sexp ty_expr_of_sexp s }
and instruction_of_sexp (s : Sexp.t) : instruction =
  { fp = Types.instruction__of_sexp instruction_of_sexp expr_of_sexp pattern_of_sexp statement_of_sexp s }
and statement_of_sexp (s : Sexp.t) : statement =
  { fp = Types.statement__of_sexp statement_of_sexp instruction_of_sexp declaration_of_sexp s }
and declaration_of_sexp (s : Sexp.t) : declaration =
  { fp = Types.declaration__of_sexp declaration_of_sexp expr_of_sexp ty_expr_of_sexp pattern_of_sexp mod_expr_of_sexp s }
and mod_expr_of_sexp (s : Sexp.t) : mod_expr =
  { fp = Types.mod_expr__of_sexp mod_expr_of_sexp statement_of_sexp declaration_of_sexp s }
and expr_of_sexp : Sexp.t -> expr = fun s ->
  let fp = Types.expr__of_sexp
    expr_of_sexp
    ty_expr_of_sexp
    pattern_of_sexp
    statement_of_sexp
    mod_expr_of_sexp
    s
  in
  { fp }

let expr_of_string = expr_of_sexp <@ Sexp.of_string
