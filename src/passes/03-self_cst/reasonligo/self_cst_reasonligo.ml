module Errors = Errors

let all_mapper ~raise ~comments = 
  ignore comments;
  [
    Helpers.fold_to_map () @@ Scoping.peephole ~raise
  ]

let all_module ~raise (init, comments) =
  let all_p = List.map ~f:Helpers.map_module @@ all_mapper ~raise ~comments in
  List.fold ~f:(|>) all_p ~init

let all_expression ~raise init =
  let all_p = List.map ~f:Helpers.map_expression @@ all_mapper ~raise ~comments:[] in
  List.fold ~f:(|>) all_p ~init

let fold_expression = Helpers.fold_expression

let map_expression  = Helpers.map_expression
