open Types
open Combinators

let rec cata_expr
    ~(f_expr : (_, _, _, _, _) expr_ -> expr)
    ~(f_ty_expr : _ ty_expr_ -> ty_expr)
    ~(f_pattern : (_, _) pattern_ -> pattern)
    ~(f_statement : (_, _, _) statement_ -> statement)
    ~(f_mod_expr : (_, _, _) mod_expr_ -> mod_expr)
    ~(f_instruction : (_, _, _, _) instruction_ -> instruction)
    ~(f_declaration : (_, _, _, _, _) declaration_ -> declaration)
    (x : expr)
    : expr
  =
  let self =
    cata_expr
      ~f_expr
      ~f_ty_expr
      ~f_pattern
      ~f_statement
      ~f_mod_expr
      ~f_instruction
      ~f_declaration
  in
  let rec cata_ty_expr (x : ty_expr) : ty_expr =
    f_ty_expr (map_ty_expr_ cata_ty_expr x.fp)
  and cata_pattern (x : pattern) : pattern =
    f_pattern (map_pattern_ cata_pattern cata_ty_expr x.fp)
  and cata_instruction (x : instruction) : instruction =
    f_instruction
      (map_instruction_ cata_instruction self cata_pattern cata_statement x.fp)
  and cata_statement (x : statement) : statement =
    f_statement (map_statement_ cata_statement cata_instruction cata_declaration x.fp)
  and cata_declaration (x : declaration) : declaration =
    f_declaration
      (map_declaration_
         cata_declaration
         self
         cata_ty_expr
         cata_pattern
         cata_mod_expr
         x.fp)
  and cata_mod_expr (x : mod_expr) : mod_expr =
    f_mod_expr (map_mod_expr_ cata_mod_expr cata_statement cata_declaration x.fp)
  in
  f_expr (map_expr_ self cata_ty_expr cata_pattern cata_statement cata_mod_expr x.fp)


let rec cata_program_entry
    ~(f_program : (_, _, _) program_entry_ -> program_entry)
    ~(f_expr : (_, _, _, _, _) expr_ -> expr)
    ~(f_ty_expr : _ ty_expr_ -> ty_expr)
    ~(f_pattern : (_, _) pattern_ -> pattern)
    ~(f_statement : (_, _, _) statement_ -> statement)
    ~(f_mod_expr : (_, _, _) mod_expr_ -> mod_expr)
    ~(f_instruction : (_, _, _, _) instruction_ -> instruction)
    ~(f_declaration : (_, _, _, _, _) declaration_ -> declaration)
    (x : program_entry)
    : program_entry
  =
  let self =
    cata_program_entry
      ~f_program
      ~f_expr
      ~f_ty_expr
      ~f_pattern
      ~f_statement
      ~f_mod_expr
      ~f_instruction
      ~f_declaration
  in
  let rec cata_ty_expr (x : ty_expr) : ty_expr =
    f_ty_expr (map_ty_expr_ cata_ty_expr x.fp)
  and cata_expr (x : expr) : expr =
    f_expr
      (map_expr_ cata_expr cata_ty_expr cata_pattern cata_statement cata_mod_expr x.fp)
  and cata_pattern (x : pattern) : pattern =
    f_pattern (map_pattern_ cata_pattern cata_ty_expr x.fp)
  and cata_instruction (x : instruction) : instruction =
    f_instruction
      (map_instruction_ cata_instruction cata_expr cata_pattern cata_statement x.fp)
  and cata_statement (x : statement) : statement =
    f_statement (map_statement_ cata_statement cata_instruction cata_declaration x.fp)
  and cata_declaration (x : declaration) : declaration =
    f_declaration
      (map_declaration_
         cata_declaration
         cata_expr
         cata_ty_expr
         cata_pattern
         cata_mod_expr
         x.fp)
  and cata_mod_expr (x : mod_expr) : mod_expr =
    f_mod_expr (map_mod_expr_ cata_mod_expr cata_statement cata_declaration x.fp)
  in
  f_program (map_program_entry_ self cata_declaration cata_instruction x.fp)


let cata_program
    ~(f_program : (_, _, _) program_entry_ -> program_entry)
    ~(f_expr : (_, _, _, _, _) expr_ -> expr)
    ~(f_ty_expr : _ ty_expr_ -> ty_expr)
    ~(f_pattern : (_, _) pattern_ -> pattern)
    ~(f_statement : (_, _, _) statement_ -> statement)
    ~(f_mod_expr : (_, _, _) mod_expr_ -> mod_expr)
    ~(f_instruction : (_, _, _, _) instruction_ -> instruction)
    ~(f_declaration : (_, _, _, _, _) declaration_ -> declaration)
    (x : program)
    : program
  =
  List.map
    x
    ~f:
      (cata_program_entry
         ~f_program
         ~f_expr
         ~f_ty_expr
         ~f_pattern
         ~f_statement
         ~f_mod_expr
         ~f_instruction
         ~f_declaration)


let default_expr : (_, _, _, _, _) expr_ -> expr = function
  | { location = loc; wrap_content } -> make_e ~loc wrap_content


let default_ty_expr : _ ty_expr_ -> ty_expr = function
  | { location = loc; wrap_content } -> make_t ~loc wrap_content


let default_pattern : (_, _) pattern_ -> pattern = function
  | { location = loc; wrap_content } -> make_p ~loc wrap_content


let default_statement : (_, _, _) statement_ -> statement = function
  | { location = loc; wrap_content } -> make_s ~loc wrap_content


let default_mod_expr : (_, _, _) mod_expr_ -> mod_expr = function
  | { location = loc; wrap_content } -> make_m ~loc wrap_content


let default_instruction : (_, _, _, _) instruction_ -> instruction = function
  | { location = loc; wrap_content } -> make_i ~loc wrap_content


let default_declaration : (_, _, _, _, _) declaration_ -> declaration = function
  | { location = loc; wrap_content } -> make_d ~loc wrap_content


let on_types f_ty_expr expr =
  cata_expr
    ~f_ty_expr
    ~f_expr:default_expr
    ~f_pattern:default_pattern
    ~f_statement:default_statement
    ~f_mod_expr:default_mod_expr
    ~f_instruction:default_instruction
    ~f_declaration:default_declaration
    expr