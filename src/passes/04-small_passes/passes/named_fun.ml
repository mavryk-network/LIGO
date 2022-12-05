
open Ast_unified
open Pass_type

let compile =
  (* let core_compile : fix_type_expr -> fix_type_expr = function
    | `T_Named_fun ((args, f), loc) ->
      let remove_name (t : fix_type_expr Named_fun.fun_type_arg) = t.type_expr in
      let args : fix_type_expr list = List.map ~f:remove_name args in
      (* We have `f`, we have `args = [a1; a2; an]`
       we want T_fun f (T_fun a1 (T_fun a2 an))
       hence the below fold over [a2; a1; f] starting with `an` *)
      let (an, l) : fix_type_expr nseq = List.Ne.rev (f, args) in
      let res = List.fold ~init:an ~f:(fun acc t -> `T_Fun ((t, acc), loc)) l in
      res
    | _ as common -> common
  in *)
  { idle_pass with ty_expr = pass_ty }


let reduction_check = Iter.defaults
let decompile = idle_pass
let pass = cata_morph ~name:__MODULE__ ~compile ~decompile ~reduction_check