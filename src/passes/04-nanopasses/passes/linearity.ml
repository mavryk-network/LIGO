open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

let unlinear_pattern pattern : bool =
  let binders = get_pattern_binders { fp = pattern } in
  is_some @@ List.find_a_dup binders ~compare:Variable.compare


let unlinear_type compare vars : bool = is_some @@ List.find_a_dup vars ~compare

let compile ~raise =
  let pattern : _ pattern_ -> unit =
   fun p -> if unlinear_pattern p then raise.error (non_linear_pattern p)
  in
  let declaration : _ declaration_ -> unit =
   fun d ->
    match Location.unwrap d with
    | D_type_abstraction { name = _; params = Some args; type_expr = _ } ->
      if List.contains_dup (List.Ne.to_list args) ~compare:Ty_variable.compare
      then raise.error (non_linear_type (`Decl ({ fp = d } : declaration)))
    | _ -> ()
  in
  let ty_expr : _ ty_expr_ -> unit =
   fun ty ->
    match Location.unwrap ty with
    | T_named_fun (args, _) ->
      if unlinear_type Ty_variable.compare (List.filter_map args ~f:(fun x -> x.name))
      then raise.error (non_linear_type (`Ty ty))
    | T_record_raw rows | T_sum_raw rows ->
      if unlinear_type Label.compare (List.map rows ~f:fst)
      then raise.error (non_linear_type (`Ty ty))
    | _ -> ()
  in
  `Check { Iter.defaults with pattern; ty_expr; declaration }


let pass ~raise =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:Iter.defaults
