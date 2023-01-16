open Types

module Catamorphism = struct
  (* f-algebras *)
  type ('expr
       , 'ty_expr
       , 'pattern
       , 'statement
       , 'block
       , 'mod_expr
       , 'instruction
       , 'declaration
       , 'program_entry)
       fold =
    { expr : ('expr, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_ -> 'expr
    ; ty_expr : 'ty_expr ty_expr_ -> 'ty_expr
    ; pattern : ('pattern, 'ty_expr) pattern_ -> 'pattern
    ; statement : ('statement, 'instruction, 'declaration) statement_ -> 'statement
    ; block : ('block, 'statement) block_ -> 'block
    ; mod_expr : ('mod_expr, 'program_entry) mod_expr_ -> 'mod_expr
    ; instruction :
        ('instruction, 'expr, 'pattern, 'statement, 'block) instruction_ -> 'instruction
    ; declaration :
        ('declaration, 'expr, 'ty_expr, 'pattern, 'mod_expr) declaration_ -> 'declaration
    ; program :
        ('program_entry, 'declaration, 'instruction) program_entry_ -> 'program_entry
    }

  (* TODO: cleanup, write separated cata.;*)
  let rec cata_ty_expr : type t. f:(t ty_expr_ -> t) -> ty_expr -> t =
   fun ~f x -> map_ty_expr_ (cata_ty_expr ~f) x.fp |> f


  (* we could factorize cata_expr and cata_program ; but I feel like those function are exactly those
    we would like to generate from algebras someday, so I keep them as such *)
  let rec cata_expr
      : type e t p s b m i d pe. f:(e, t, p, s, b, m, i, d, pe) fold -> expr -> e
    =
   fun ~f x ->
    let self = cata_expr ~f in
    let rec cata_ty_expr (x : ty_expr) : t = map_ty_expr_ cata_ty_expr x.fp |> f.ty_expr
    and cata_pattern (x : pattern) : p =
      map_pattern_ cata_pattern cata_ty_expr x.fp |> f.pattern
    and cata_instruction (x : instruction) : i =
      map_instruction_ cata_instruction self cata_pattern cata_statement cata_block x.fp
      |> f.instruction
    and cata_statement (x : statement) : s =
      map_statement_ cata_statement cata_instruction cata_declaration x.fp |> f.statement
    and cata_block (x : block) : b = map_block_ cata_block cata_statement x.fp |> f.block
    and cata_declaration (x : declaration) : d =
      map_declaration_ cata_declaration self cata_ty_expr cata_pattern cata_mod_expr x.fp
      |> f.declaration
    and cata_mod_expr (x : mod_expr) : m =
      map_mod_expr_ cata_mod_expr cata_program_entry x.fp |> f.mod_expr
    and cata_program_entry (x : program_entry) : pe =
      map_program_entry_ cata_program_entry cata_declaration cata_instruction x.fp
      |> f.program
    in
    map_expr_ self cata_ty_expr cata_pattern cata_block cata_mod_expr x.fp |> f.expr


  let rec cata_program_entry
      : type e t p s b m i d pe.
        f:(e, t, p, s, b, m, i, d, pe) fold -> program_entry -> pe
    =
   fun ~f x ->
    let self = cata_program_entry ~f in
    let rec cata_ty_expr (x : ty_expr) : t = map_ty_expr_ cata_ty_expr x.fp |> f.ty_expr
    and cata_expr (x : expr) : e =
      map_expr_ cata_expr cata_ty_expr cata_pattern cata_block cata_mod_expr x.fp
      |> f.expr
    and cata_pattern (x : pattern) : p =
      map_pattern_ cata_pattern cata_ty_expr x.fp |> f.pattern
    and cata_instruction (x : instruction) : i =
      map_instruction_
        cata_instruction
        cata_expr
        cata_pattern
        cata_statement
        cata_block
        x.fp
      |> f.instruction
    and cata_statement (x : statement) : s =
      map_statement_ cata_statement cata_instruction cata_declaration x.fp |> f.statement
    and cata_block (x : block) : b = map_block_ cata_block cata_statement x.fp |> f.block
    and cata_declaration (x : declaration) : d =
      map_declaration_
        cata_declaration
        cata_expr
        cata_ty_expr
        cata_pattern
        cata_mod_expr
        x.fp
      |> f.declaration
    and cata_mod_expr (x : mod_expr) : m =
      map_mod_expr_ cata_mod_expr self x.fp |> f.mod_expr
    in
    map_program_entry_ self cata_declaration cata_instruction x.fp |> f.program


  let cata_program ~f x = List.map x ~f:(cata_program_entry ~f)
end

module Anamorphism = struct
  (* f-algebras *)
  type ('expr
       , 'ty_expr
       , 'pattern
       , 'statement
       , 'block
       , 'mod_expr
       , 'instruction
       , 'declaration
       , 'program_entry)
       unfold =
    { expr : 'expr -> ('expr, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_
    ; ty_expr : 'ty_expr -> 'ty_expr ty_expr_
    ; pattern : 'pattern -> ('pattern, 'ty_expr) pattern_
    ; statement : 'statement -> ('statement, 'instruction, 'declaration) statement_
    ; block : 'block -> ('block, 'statement) block_
    ; mod_expr : 'mod_expr -> ('mod_expr, 'program_entry) mod_expr_
    ; instruction :
        'instruction -> ('instruction, 'expr, 'pattern, 'statement, 'block) instruction_
    ; declaration :
        'declaration -> ('declaration, 'expr, 'ty_expr, 'pattern, 'mod_expr) declaration_
    ; program :
        'program_entry -> ('program_entry, 'declaration, 'instruction) program_entry_
    }

  let rec ana_ty_expr : type t. f:(t -> t ty_expr_) -> t -> ty_expr =
   fun ~f x -> { fp = f x |> map_ty_expr_ (ana_ty_expr ~f) }


  let rec ana_expr
      : type e t p s b m i d pe. f:(e, t, p, s, b, m, i, d, pe) unfold -> e -> expr
    =
   fun ~f x ->
    let self = ana_expr ~f in
    let rec ana_ty_expr (x : t) : ty_expr =
      { fp = f.ty_expr x |> map_ty_expr_ ana_ty_expr }
    and ana_pattern (x : p) : pattern =
      { fp = f.pattern x |> map_pattern_ ana_pattern ana_ty_expr }
    and ana_instruction (x : i) : instruction =
      { fp =
          f.instruction x
          |> map_instruction_ ana_instruction self ana_pattern ana_statement ana_block
      }
    and ana_statement (x : s) : statement =
      { fp = f.statement x |> map_statement_ ana_statement ana_instruction ana_declaration
      }
    and ana_block (x : b) : block =
      { fp = f.block x |> map_block_ ana_block ana_statement }
    and ana_declaration (x : d) : declaration =
      { fp =
          f.declaration x
          |> map_declaration_ ana_declaration self ana_ty_expr ana_pattern ana_mod_expr
      }
    and ana_mod_expr (x : m) =
      { fp = f.mod_expr x |> map_mod_expr_ ana_mod_expr ana_program_entry }
    and ana_program_entry (x : pe) =
      { fp =
          f.program x
          |> map_program_entry_ ana_program_entry ana_declaration ana_instruction
      }
    in
    { fp = f.expr x |> map_expr_ self ana_ty_expr ana_pattern ana_block ana_mod_expr }


  let rec ana_program_entry
      : type e t p s b m i d pe.
        f:(e, t, p, s, b, m, i, d, pe) unfold -> pe -> program_entry
    =
   fun ~f x ->
    let self = ana_program_entry ~f in
    let rec ana_ty_expr (x : t) : ty_expr =
      { fp = f.ty_expr x |> map_ty_expr_ ana_ty_expr }
    and ana_expr (x : e) : expr =
      { fp = f.expr x |> map_expr_ ana_expr ana_ty_expr ana_pattern ana_block ana_mod_expr
      }
    and ana_pattern (x : p) : pattern =
      { fp = f.pattern x |> map_pattern_ ana_pattern ana_ty_expr }
    and ana_instruction (x : i) : instruction =
      { fp =
          f.instruction x
          |> map_instruction_ ana_instruction ana_expr ana_pattern ana_statement ana_block
      }
    and ana_statement (x : s) : statement =
      { fp = f.statement x |> map_statement_ ana_statement ana_instruction ana_declaration
      }
    and ana_block (x : b) : block =
      { fp = f.block x |> map_block_ ana_block ana_statement }
    and ana_declaration (x : d) : declaration =
      { fp =
          f.declaration x
          |> map_declaration_
               ana_declaration
               ana_expr
               ana_ty_expr
               ana_pattern
               ana_mod_expr
      }
    and ana_mod_expr (x : m) = { fp = f.mod_expr x |> map_mod_expr_ ana_mod_expr self } in
    { fp = f.program x |> map_program_entry_ self ana_declaration ana_instruction }


  let ana_program ~f x = List.map x ~f:(ana_program_entry ~f)
end

module Iter = struct
  type iter =
    { expr : (expr, ty_expr, pattern, block, mod_expr) expression_ -> unit
    ; ty_expr : ty_expr ty_expr_ -> unit
    ; pattern : (pattern, ty_expr) pattern_ -> unit
    ; statement : (statement, instruction, declaration) statement_ -> unit
    ; block : (block, statement) block_ -> unit
    ; mod_expr : (mod_expr, program_entry) mod_expr_ -> unit
    ; instruction : (instruction, expr, pattern, statement, block) instruction_ -> unit
    ; declaration : (declaration, expr, ty_expr, pattern, mod_expr) declaration_ -> unit
    ; program : (program_entry, declaration, instruction) program_entry_ -> unit
    }

  let defaults =
    { expr = ignore
    ; ty_expr = ignore
    ; pattern = ignore
    ; statement = ignore
    ; block = ignore
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
      ; block =
          (fun x ->
            acc.block x;
            iter.block x)
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
      iter_instruction_ iter_instruction self iter_pattern iter_statement iter_block x.fp
    and iter_statement (x : statement) : unit =
      iter_statement_ iter_statement iter_instruction iter_declaration x.fp
    and iter_block (x : block) : unit = iter_block_ iter_block iter_statement x.fp
    and iter_declaration (x : declaration) : unit =
      iter_declaration_ iter_declaration self iter_ty_expr iter_pattern iter_mod_expr x.fp
    and iter_mod_expr (x : mod_expr) : unit =
      iter_mod_expr_ iter_mod_expr iter_program_entry x.fp
    and iter_program_entry (x : program_entry) : unit =
      iter_program_entry_ iter_program_entry iter_declaration iter_instruction x.fp
    in
    iter_expr_ self iter_ty_expr iter_pattern iter_block iter_mod_expr x.fp


  let rec iter_program_entry ~(f : iter) (x : program_entry) : unit =
    let rec iter_expr (x : expr) : unit =
      iter_expr_ iter_expr iter_ty_expr iter_pattern iter_block iter_mod_expr x.fp
    and iter_ty_expr (x : ty_expr) : unit = iter_ty_expr_ iter_ty_expr x.fp
    and iter_pattern (x : pattern) : unit = iter_pattern_ iter_pattern iter_ty_expr x.fp
    and iter_instruction (x : instruction) : unit =
      iter_instruction_
        iter_instruction
        iter_expr
        iter_pattern
        iter_statement
        iter_block
        x.fp
    and iter_statement (x : statement) : unit =
      iter_statement_ iter_statement iter_instruction iter_declaration x.fp
    and iter_block (x : block) : unit = iter_block_ iter_block iter_statement x.fp
    and iter_declaration (x : declaration) : unit =
      iter_declaration_
        iter_declaration
        iter_expr
        iter_ty_expr
        iter_pattern
        iter_mod_expr
        x.fp
    and iter_mod_expr (x : mod_expr) : unit =
      iter_mod_expr_ iter_mod_expr (iter_program_entry ~f) x.fp
    in
    iter_program_entry_ (iter_program_entry ~f) iter_declaration iter_instruction x.fp


  let iter_program ~f lst = List.iter ~f:(iter_program_entry ~f) lst
end