open Types

module Catamorphism = struct
  (* f-algebras *)
  type ('expr
       , 'ty_expr
       , 'pattern
       , 'statement
       , 'mod_expr
       , 'instruction
       , 'declaration
       , 'program_entry)
       fold =
    { expr : ('expr, 'ty_expr, 'pattern, 'statement, 'mod_expr) expression_ -> 'expr
    ; ty_expr : 'ty_expr ty_expr_ -> 'ty_expr
    ; pattern : ('pattern, 'ty_expr) pattern_ -> 'pattern
    ; statement : ('statement, 'instruction, 'declaration) statement_ -> 'statement
    ; mod_expr : ('mod_expr, 'statement, 'declaration) mod_expr_ -> 'mod_expr
    ; instruction :
        ('instruction, 'expr, 'pattern, 'statement) instruction_ -> 'instruction
    ; declaration :
        ('declaration, 'expr, 'ty_expr, 'pattern, 'mod_expr) declaration_ -> 'declaration
    ; program :
        ('program_entry, 'declaration, 'instruction) program_entry_ -> 'program_entry
    }

  (* we could factorize cata_expr and cata_program ; but I feel like those function are exactly those
    we would like to generate from algebras someday, so I keep them as such *)
  let rec cata_expr : type e t p s m i d pe. f:(e, t, p, s, m, i, d, pe) fold -> expr -> e
    =
   fun ~f x ->
    let self = cata_expr ~f in
    let rec cata_ty_expr (x : ty_expr) : t = f.ty_expr (map_ty_expr_ cata_ty_expr x.fp)
    and cata_pattern (x : pattern) : p =
      f.pattern (map_pattern_ cata_pattern cata_ty_expr x.fp)
    and cata_instruction (x : instruction) : i =
      f.instruction
        (map_instruction_ cata_instruction self cata_pattern cata_statement x.fp)
    and cata_statement (x : statement) : s =
      f.statement (map_statement_ cata_statement cata_instruction cata_declaration x.fp)
    and cata_declaration (x : declaration) : d =
      f.declaration
        (map_declaration_
           cata_declaration
           self
           cata_ty_expr
           cata_pattern
           cata_mod_expr
           x.fp)
    and cata_mod_expr (x : mod_expr) : m =
      f.mod_expr (map_mod_expr_ cata_mod_expr cata_statement cata_declaration x.fp)
    in
    f.expr (map_expr_ self cata_ty_expr cata_pattern cata_statement cata_mod_expr x.fp)


  let rec cata_program_entry
      : type e t p s m i d pe. f:(e, t, p, s, m, i, d, pe) fold -> program_entry -> pe
    =
   fun ~f x ->
    let self = cata_program_entry ~f in
    let rec cata_ty_expr (x : ty_expr) : t = f.ty_expr (map_ty_expr_ cata_ty_expr x.fp)
    and cata_expr (x : expr) : e =
      f.expr
        (map_expr_ cata_expr cata_ty_expr cata_pattern cata_statement cata_mod_expr x.fp)
    and cata_pattern (x : pattern) : p =
      f.pattern (map_pattern_ cata_pattern cata_ty_expr x.fp)
    and cata_instruction (x : instruction) : i =
      f.instruction
        (map_instruction_ cata_instruction cata_expr cata_pattern cata_statement x.fp)
    and cata_statement (x : statement) : s =
      f.statement (map_statement_ cata_statement cata_instruction cata_declaration x.fp)
    and cata_declaration (x : declaration) : d =
      f.declaration
        (map_declaration_
           cata_declaration
           cata_expr
           cata_ty_expr
           cata_pattern
           cata_mod_expr
           x.fp)
    and cata_mod_expr (x : mod_expr) : m =
      f.mod_expr (map_mod_expr_ cata_mod_expr cata_statement cata_declaration x.fp)
    in
    f.program (map_program_entry_ self cata_declaration cata_instruction x.fp)


  let cata_program ~f x = List.map x ~f:(cata_program_entry ~f)
end

module Anamorphism = struct
  (* f-algebras *)
  type ('expr
       , 'ty_expr
       , 'pattern
       , 'statement
       , 'mod_expr
       , 'instruction
       , 'declaration
       , 'program_entry)
       unfold =
    { expr : 'expr -> ('expr, 'ty_expr, 'pattern, 'statement, 'mod_expr) expression_
    ; ty_expr : 'ty_expr -> 'ty_expr ty_expr_
    ; pattern : 'pattern -> ('pattern, 'ty_expr) pattern_
    ; statement : 'statement -> ('statement, 'instruction, 'declaration) statement_
    ; mod_expr : 'mod_expr -> ('mod_expr, 'statement, 'declaration) mod_expr_
    ; instruction :
        'instruction -> ('instruction, 'expr, 'pattern, 'statement) instruction_
    ; declaration :
        'declaration -> ('declaration, 'expr, 'ty_expr, 'pattern, 'mod_expr) declaration_
    ; program :
        'program_entry -> ('program_entry, 'declaration, 'instruction) program_entry_
    }
  (* todo if needed *)
end

module Iter = struct
  type iter =
    { expr : (expr, ty_expr, pattern, statement, mod_expr) expression_ -> unit
    ; ty_expr : ty_expr ty_expr_ -> unit
    ; pattern : (pattern, ty_expr) pattern_ -> unit
    ; statement : (statement, instruction, declaration) statement_ -> unit
    ; mod_expr : (mod_expr, statement, declaration) mod_expr_ -> unit
    ; instruction : (instruction, expr, pattern, statement) instruction_ -> unit
    ; declaration : (declaration, expr, ty_expr, pattern, mod_expr) declaration_ -> unit
    ; program : (program_entry, declaration, instruction) program_entry_ -> unit
    }

  let defaults =
    { expr = ignore
    ; ty_expr = ignore
    ; pattern = ignore
    ; statement = ignore
    ; mod_expr = ignore
    ; instruction = ignore
    ; declaration = ignore
    ; program = ignore
    }


  let combine_iteration : iter list -> iter =
   fun iters ->
    let aux acc iter =
      { expr =
          (fun x ->
            acc.expr x;
            iter.expr x)
      ; ty_expr =
          (fun x ->
            acc.ty_expr x;
            iter.ty_expr x)
      ; pattern =
          (fun x ->
            acc.pattern x;
            iter.pattern x)
      ; statement =
          (fun x ->
            acc.statement x;
            iter.statement x)
      ; mod_expr =
          (fun x ->
            acc.mod_expr x;
            iter.mod_expr x)
      ; instruction =
          (fun x ->
            acc.instruction x;
            iter.instruction x)
      ; declaration =
          (fun x ->
            acc.declaration x;
            iter.declaration x)
      ; program =
          (fun x ->
            acc.program x;
            iter.program x)
      }
    in
    List.fold ~init:defaults ~f:aux iters


  let rec iter_expr ~(f : iter) (x : expr) : unit =
    let self = iter_expr ~f in
    let rec iter_ty_expr (x : ty_expr) : unit = iter_ty_expr_ iter_ty_expr x.fp
    and iter_pattern (x : pattern) : unit = iter_pattern_ iter_pattern iter_ty_expr x.fp
    and iter_instruction (x : instruction) : unit =
      iter_instruction_ iter_instruction self iter_pattern iter_statement x.fp
    and iter_statement (x : statement) : unit =
      iter_statement_ iter_statement iter_instruction iter_declaration x.fp
    and iter_declaration (x : declaration) : unit =
      iter_declaration_ iter_declaration self iter_ty_expr iter_pattern iter_mod_expr x.fp
    and iter_mod_expr (x : mod_expr) : unit =
      iter_mod_expr_ iter_mod_expr iter_statement iter_declaration x.fp
    in
    iter_expr_ self iter_ty_expr iter_pattern iter_statement iter_mod_expr x.fp


  let rec iter_program_entry ~(f : iter) (x : program_entry) : unit =
    let rec iter_expr (x : expr) : unit =
      iter_expr_ iter_expr iter_ty_expr iter_pattern iter_statement iter_mod_expr x.fp
    and iter_ty_expr (x : ty_expr) : unit = iter_ty_expr_ iter_ty_expr x.fp
    and iter_pattern (x : pattern) : unit = iter_pattern_ iter_pattern iter_ty_expr x.fp
    and iter_instruction (x : instruction) : unit =
      iter_instruction_ iter_instruction iter_expr iter_pattern iter_statement x.fp
    and iter_statement (x : statement) : unit =
      iter_statement_ iter_statement iter_instruction iter_declaration x.fp
    and iter_declaration (x : declaration) : unit =
      iter_declaration_
        iter_declaration
        iter_expr
        iter_ty_expr
        iter_pattern
        iter_mod_expr
        x.fp
    and iter_mod_expr (x : mod_expr) : unit =
      iter_mod_expr_ iter_mod_expr iter_statement iter_declaration x.fp
    in
    iter_program_entry_ (iter_program_entry ~f) iter_declaration iter_instruction x.fp


  let iter_program ~f lst = List.iter ~f:(iter_program_entry ~f) lst
end