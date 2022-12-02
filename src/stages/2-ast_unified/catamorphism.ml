open Types
open Combinators

(* These are all the functions you must specify when implementing a pass.
   If you want a pass that modifies only patterns (for example),
   you should only touch 'fp' and leave the others to the default value,
   i.e. the identity function *)
type folders =
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


let rec cata_expr ~(f : folders) (x : expr) : expr =
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


let rec cata_program_entry ~(f : folders) (x : program_entry) : program_entry =
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

let cata_program ~(f : folders) (x : program) : program =
  List.map x ~f:(cata_program_entry ~f)
