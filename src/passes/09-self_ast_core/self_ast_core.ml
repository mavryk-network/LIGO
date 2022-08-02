let all_term_mapper = [
]

let all_module ~init =
  let all_p  = List.map ~f:Helpers.map_term_in_declarations all_term_mapper in
  List.fold ~init ~f:(|>) all_p

let all_term ~raise p =
  let all_p = List.map ~f:(Helpers.map_term ~raise) all_term_mapper in
  List.fold ~f:(|>) all_p ~init:p

let fold_map_term = Helpers.fold_map_term
