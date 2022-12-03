open Types

module Catamorphism = struct
  (*
    These are all the functions you must specify when implementing a pass.
    If a pass only act on patterns then you only need to fill field
    `pattern` and leave the others to the default value, i.e. the identity function
  *)
  type fold =
    { expr : (expr, ty_expr, pattern, statement, mod_expr) expression_ -> expr
    ; ty_expr : ty_expr ty_expr_ -> ty_expr
    ; pattern : (pattern, ty_expr) pattern_ -> pattern
    ; statement : (statement, instruction, declaration) statement_ -> statement
    ; mod_expr : (mod_expr, statement, declaration) mod_expr_ -> mod_expr
    ; instruction : (instruction, expr, pattern, statement) instruction_ -> instruction
    ; declaration :
        (declaration, expr, ty_expr, pattern, mod_expr) declaration_ -> declaration
    ; program : (program_entry, declaration, instruction) program_entry_ -> program_entry
    }

  let defaults =
    { expr = (function| x -> { fp = x })
    ; ty_expr = (function | x -> { fp = x })
    ; pattern = (function | x -> { fp = x })
    ; statement = (function | x -> { fp = x })
    ; mod_expr = (function | x -> { fp = x })
    ; instruction = (function | x -> { fp = x })
    ; declaration = (function | x -> { fp = x })
    ; program = (function | x -> { fp = x })
    }

  let rec cata_expr ~(f : fold) (x : expr) : expr =
    let self = cata_expr ~f in
    let rec cata_ty_expr (x : ty_expr) : ty_expr =
      f.ty_expr (map_ty_expr_ cata_ty_expr x.fp)
    and cata_pattern (x : pattern) : pattern =
      f.pattern (map_pattern_ cata_pattern cata_ty_expr x.fp)
    and cata_instruction (x : instruction) : instruction =
      f.instruction
        (map_instruction_ cata_instruction self cata_pattern cata_statement x.fp)
    and cata_statement (x : statement) : statement =
      f.statement (map_statement_ cata_statement cata_instruction cata_declaration x.fp)
    and cata_declaration (x : declaration) : declaration =
      f.declaration
        (map_declaration_
          cata_declaration
          self
          cata_ty_expr
          cata_pattern
          cata_mod_expr
          x.fp)
    and cata_mod_expr (x : mod_expr) : mod_expr =
      f.mod_expr (map_mod_expr_ cata_mod_expr cata_statement cata_declaration x.fp)
    in
    f.expr (map_expr_ self cata_ty_expr cata_pattern cata_statement cata_mod_expr x.fp)


  let rec cata_program_entry ~(f : fold) (x : program_entry) : program_entry =
    let self = cata_program_entry ~f in
    let rec cata_ty_expr (x : ty_expr) : ty_expr =
      f.ty_expr (map_ty_expr_ cata_ty_expr x.fp)
    and cata_expr (x : expr) : expr =
      f.expr
        (map_expr_ cata_expr cata_ty_expr cata_pattern cata_statement cata_mod_expr x.fp)
    and cata_pattern (x : pattern) : pattern =
      f.pattern (map_pattern_ cata_pattern cata_ty_expr x.fp)
    and cata_instruction (x : instruction) : instruction =
      f.instruction
        (map_instruction_ cata_instruction cata_expr cata_pattern cata_statement x.fp)
    and cata_statement (x : statement) : statement =
      f.statement (map_statement_ cata_statement cata_instruction cata_declaration x.fp)
    and cata_declaration (x : declaration) : declaration =
      f.declaration
        (map_declaration_
          cata_declaration
          cata_expr
          cata_ty_expr
          cata_pattern
          cata_mod_expr
          x.fp)
    and cata_mod_expr (x : mod_expr) : mod_expr =
      f.mod_expr (map_mod_expr_ cata_mod_expr cata_statement cata_declaration x.fp)
    in
    f.program (map_program_entry_ self cata_declaration cata_instruction x.fp)

  (* we could factorize cata_expr and cata_program ; but I feel like those function are exactly those
    we would like to generate someday, so I keep them as such *)
  let cata_program ~(f : fold) (x : program) : program =
    List.map x ~f:(cata_program_entry ~f)
end


module Anamorphism = struct
  type unfold =
  { expr : expr -> (expr, ty_expr, pattern, statement, mod_expr) expression_
  ; ty_expr : ty_expr -> ty_expr ty_expr_
  ; pattern : pattern -> (pattern, ty_expr) pattern_
  ; statement : statement -> (statement, instruction, declaration) statement_
  ; mod_expr : mod_expr -> (mod_expr, statement, declaration) mod_expr_
  ; instruction : instruction -> (instruction, expr, pattern, statement) instruction_
  ; declaration : declaration -> (declaration, expr, ty_expr, pattern, mod_expr) declaration_
  ; program : program_entry -> (program_entry, declaration, instruction) program_entry_
  }
  (* todo if needed *)
end