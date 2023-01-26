open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

let unlinear_pattern pattern : bool =
  let binders =
    fold_pattern_content_
      (fun acc p -> Option.value_map (get_p_var p) ~default:acc ~f:(fun x -> x :: acc))
      Fun.const
      []
      (Location.unwrap pattern)
  in
  is_some @@ List.find_a_dup binders ~compare:Variable.compare


let unlinear_type ty : bool =
  let binders =
    fold_type_expression_content_
      (fun acc p -> Option.value_map (get_t_var p) ~default:acc ~f:(fun x -> x :: acc))
      []
      (Location.unwrap ty)
  in
  is_some @@ List.find_a_dup binders ~compare:Ty_variable.compare


let compile ~raise =
  let pattern : _ pattern_ -> unit = fun p -> if unlinear_pattern p then raise.error (non_linear_pattern p) in
  let ty_expr : _ ty_expr_ -> unit = fun ty -> if unlinear_type ty then raise.error (non_linear_type ty) in
  `Check { Iter.defaults with pattern; ty_expr }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:Iter.defaults