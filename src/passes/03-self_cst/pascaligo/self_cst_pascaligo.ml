module Errors = Errors
module Warnings = Warnings

let all_mapper ~raise ~add_warning = [
  Helpers.fold_to_map () @@ Scoping.peephole ~raise ~add_warning
]

let all_module ~raise ~add_warning init =
  let all_p = List.map ~f:Helpers.map_module @@ all_mapper ~raise ~add_warning in
  List.fold ~f:(|>) all_p ~init

let all_expression ~raise ~add_warning init =
  let all_p = List.map ~f:Helpers.map_expression @@ all_mapper ~raise ~add_warning in
  List.fold ~f:(|>) all_p ~init

let fold_expression = Helpers.fold_expression

let map_expression  = Helpers.map_expression
