open Ast_unified

type options = unit (*remove pass it in files *)

(* it's preferable to use the defined recursion schemes for code transformations
   but  *)
type 'a code_transformation = 'a -> 'a
type 'a dyn_reduction_check = Iter.iter * (Iter.iter -> 'a -> unit)
(* to dynamically check if reduction happened, we will always use iter*)

type 'a sub_pass =
  { name : string
  ; forward : 'a code_transformation
  ; forward_check : 'a dyn_reduction_check
  ; backward : 'a code_transformation
  }

type pass =
  { expression : expr sub_pass
  ; program : program sub_pass
  }

let combine_checks : 'a dyn_reduction_check list -> 'a -> unit =
 fun checks ->
  let iters = List.map ~f:fst checks in
  let combined_iter = Iter.combine_iteration iters in
  match checks with
  | [] -> fun _ -> ()
  (* we always use iter, so it does not matter which function we take here ... *)
  | (_, f) :: _ -> f combined_iter


let cata_morph
    ~name
    ~(compile : Ast_unified.Catamorphism.fold)
    ~(decompile : Ast_unified.Catamorphism.fold)
    ~(reduction_check : Ast_unified.Iter.iter)
    : pass
  =
  let expression =
    let forward expr = Catamorphism.(cata_expr ~f:compile expr) in
    let backward expr = Catamorphism.(cata_expr ~f:decompile expr) in
    let reduction_check = reduction_check, fun f expr -> Iter.(iter_expr ~f expr) in
    { name; forward; forward_check = reduction_check; backward }
  in
  let program =
    let forward prg = Catamorphism.(cata_program ~f:compile prg) in
    let backward prg = Catamorphism.(cata_program ~f:decompile prg) in
    let reduction_check = reduction_check, fun f prg -> Iter.(iter_program ~f prg) in
    { name; forward; forward_check = reduction_check; backward }
  in
  { expression; program }
