module Errors = Errors
module Helpers = Helpers

let expression_obj ~raise e = Obj_ligo.check_obj_ligo ~raise e

let all_expression ~raise ~add_warning ~(options : Compiler_options.middle_end) e =
  let e = Helpers.map_expression Polymorphic_replace.expression e in
  let e = if not options.test then Obj_ligo.check_obj_ligo ~raise e else e in
  let e = Purify_assignations.expression ~add_warning e in
  let e = Monomorphisation.mono_polymorphic_expr e in
  let e = Uncurry.uncurry_expression e in
  e

let contract_passes ~raise = [
  Contract_passes.self_typing ~raise ;
  Contract_passes.entrypoint_typing ~raise ;
]

let all_contract ~raise parameter storage prg =
  let contract_type : Contract_passes.contract_type = { parameter ; storage } in
  let all_p = List.map ~f:(fun pass -> Helpers.fold_map_expression pass contract_type) @@ contract_passes ~raise in
  let prg = List.fold ~f:(fun x f -> snd @@ f x) all_p ~init:prg in
  prg
