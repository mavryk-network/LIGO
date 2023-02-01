open Ast_unified

(* it's preferable to use the defined recursion schemes for code transformations
   but  *)
type 'a code_transformation = 'a -> 'a

(* to dynamically check if reduction happened, we will always use iter, this type feels wrong tho *)
type 'a dyn_reduction_check = Iter.iter * (Iter.iter -> 'a -> unit)

type 'a sub_pass =
  { forward : 'a code_transformation
  ; forward_check : 'a dyn_reduction_check
  ; backward : 'a code_transformation
  }

type pass =
  { name : string
  ; expression : expr sub_pass
  ; program : program sub_pass
  }

let combine_checks : 'a dyn_reduction_check list -> 'a -> unit =
 fun checks ->
  let iters = List.map ~f:fst checks in
  let combined_iter = Iter.combine_iteration iters in
  match checks with
  | [] -> fun _ -> ()
  (* we always use iter, so it does not matter which function we take here ... feels wrong *)
  | (_, f) :: _ -> f combined_iter


(* catamorphism mapping expressions to expressions, program to program .. *)
type cata_pass =
  ( expr
  , ty_expr
  , pattern
  , statement
  , block
  , mod_expr
  , instruction
  , declaration
  , program_entry )
  Ast_unified.Catamorphism.fold

type ana_pass =
  ( expr
  , ty_expr
  , pattern
  , statement
  , block
  , mod_expr
  , instruction
  , declaration
  , program_entry )
  Ast_unified.Anamorphism.unfold

let idle_cata_pass : cata_pass =
  { expr = (fun x -> { fp = x })
  ; ty_expr = (fun x -> { fp = x })
  ; pattern = (fun x -> { fp = x })
  ; statement = (fun x -> { fp = x })
  ; block = (fun x -> { fp = x })
  ; mod_expr = (fun x -> { fp = x })
  ; instruction = (fun x -> { fp = x })
  ; declaration = (fun x -> { fp = x })
  ; program = (fun x -> { fp = x })
  }


let idle_ana_pass : ana_pass =
  { expr = (fun { fp } -> fp)
  ; ty_expr = (fun { fp } -> fp)
  ; pattern = (fun { fp } -> fp)
  ; statement = (fun { fp } -> fp)
  ; block = (fun { fp } -> fp)
  ; mod_expr = (fun { fp } -> fp)
  ; instruction = (fun { fp } -> fp)
  ; declaration = (fun { fp } -> fp)
  ; program = (fun { fp } -> fp)
  }

type pass_kind = [ `Cata of cata_pass | `Ana of ana_pass | `Check of Iter.iter | `None ]
let cata_morph
    ~name
    ~(compile : pass_kind)
    ~(decompile : pass_kind)
    ~(reduction_check : Ast_unified.Iter.iter)
    : pass
  =
  let expression =
    let forward expr =
      match compile with
      | `Cata pass -> Catamorphism.(cata_expr ~f:pass expr)
      | `Ana pass -> Anamorphism.(ana_expr ~f:pass expr)
      | `Check pass -> Iter.iter_expr ~f:pass expr ; expr
      | `None -> expr
    in
    let backward expr =
      match decompile with
      | `Cata pass -> Catamorphism.(cata_expr ~f:pass expr)
      | `Ana pass -> Anamorphism.(ana_expr ~f:pass expr)
      | `Check pass -> Iter.iter_expr ~f:pass expr ; expr
      | `None -> expr
    in
    let reduction_check = reduction_check, fun f expr -> Iter.(iter_expr ~f expr) in
    { forward; forward_check = reduction_check; backward }
  in
  let program =
    let forward prg =
      match compile with
      | `Cata pass -> Catamorphism.(cata_program ~f:pass prg)
      | `Ana pass -> Anamorphism.(ana_program ~f:pass prg)
      | `Check pass -> Iter.iter_program ~f:pass prg ; prg
      | `None -> prg
    in
    let backward prg =
      match decompile with
      | `Cata pass -> Catamorphism.(cata_program ~f:pass prg)
      | `Ana pass -> Anamorphism.(ana_program ~f:pass prg)
      | `Check pass -> Iter.iter_program ~f:pass prg ; prg
      | `None -> prg
    in
    let reduction_check = reduction_check, fun f prg -> Iter.(iter_program ~f prg) in
    { forward; forward_check = reduction_check; backward }
  in
  { name; expression; program }
