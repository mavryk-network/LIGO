let to_c_unit ~options ~meta file_path =
  let c_unit  = Of_source.compile ~options ~meta file_path in
  c_unit

let to_imperative ~raise ~add_warning ~options ~meta (c_unit: Buffer.t) file_path =
  let () = ignore options in
  let imperative = Of_c_unit.compile ~raise ~add_warning ~meta c_unit file_path in
  imperative

let to_sugar ~raise ~add_warning ~options ~meta (c_unit: Buffer.t) file_path =
  let imperative = to_imperative ~raise ~add_warning ~options ~meta c_unit file_path in
  let sugar      = Of_imperative.compile ~raise imperative in
  sugar

let to_core ~raise ~add_warning ~options ~meta (c_unit: Buffer.t) file_path =
  let sugar  = to_sugar ~raise ~add_warning ~options ~meta c_unit file_path in
  let core   = Of_sugar.compile sugar in
  core

let type_file ~raise ~add_warning ~options f stx form : Ast_typed.program =
  let meta          = Of_source.extract_meta ~raise stx f in
  let c_unit,_      = Of_source.compile ~raise ~options ~meta f in
  let core          = to_core ~raise ~add_warning ~options ~meta c_unit f in
  let inferred      = Of_core.infer ~raise ~options core in
  let typed         = Of_core.typecheck ~raise ~add_warning ~options form inferred in
  typed

let to_mini_c ~raise ~add_warning ~options f stx env =
  let typed  = type_file ~raise ~add_warning ~options f stx env in
  let mini_c     = Of_typed.compile_program ~raise typed in
  mini_c

let compile_file ~raise ~add_warning ~options f stx ep =
  let typed    = type_file ~raise ~add_warning ~options f stx @@ Contract ep in
  let aggregated = Of_typed.apply_to_entrypoint_contract ~raise typed ep in
  let mini_c     = Of_aggregated.compile_expression ~raise aggregated in
  let michelson  = Of_mini_c.compile_contract ~raise ~options mini_c in
  let contract   = Of_michelson.build_contract ~raise michelson in
  contract

let type_expression_string ~raise ~options syntax expression init_prog =
  let meta              = Of_source.make_meta_from_syntax syntax in
  let c_unit_exp, _     = Of_source.compile_string_without_preproc expression in
  let imperative_exp    = Of_c_unit.compile_expression ~raise ~meta c_unit_exp in
  let sugar_exp         = Of_imperative.compile_expression ~raise imperative_exp in
  let core_exp          = Of_sugar.compile_expression sugar_exp in
  let typed_exp         = Of_core.compile_expression ~raise ~options ~init_prog core_exp in
  typed_exp

let type_contract_string ~raise ~add_warning ~options syntax expression env =
  let meta          = Of_source.make_meta_from_syntax syntax in
  let c_unit, _     = Of_source.compile_string_without_preproc expression in
  let imperative    = Of_c_unit.compile_string ~raise ~add_warning ~meta c_unit in
  let sugar         = Of_imperative.compile ~raise imperative in
  let core          = Of_sugar.compile sugar in
  let inferred      = Of_core.infer ~raise ~options:{options with init_env = env} core in
  let typed         = Of_core.typecheck ~raise ~add_warning ~options:{options with init_env = env} Env inferred in
  typed,core

let type_expression ~raise ~options source_file syntax expression init_prog =
  let meta              = Of_source.make_meta ~raise syntax source_file in (* TODO: should be computed outside *)
  let c_unit_exp, _     = Of_source.compile_string ~raise ~options ~meta expression in
  let imperative_exp    = Of_c_unit.compile_expression ~raise ~meta c_unit_exp in
  let sugar_exp         = Of_imperative.compile_expression ~raise imperative_exp in
  let core_exp          = Of_sugar.compile_expression sugar_exp in
  let typed_exp         = Of_core.compile_expression ~raise ~options ~init_prog core_exp in
  typed_exp

let expression_to_aggregated ~raise ~add_warning ~options source_file syntax expression init_prog =
  let typed_exp  = type_expression ~raise ~add_warning ~options source_file syntax expression init_prog in
  Of_typed.compile_expression ~raise typed_exp

let expression_to_mini_c ~raise ~add_warning ~options source_file syntax expression env =
  let aggregated  = expression_to_aggregated ~raise ~add_warning ~options source_file syntax expression env in
  let mini_c_exp  = Of_aggregated.compile_expression ~raise aggregated in
  mini_c_exp

let compile_expression ~raise ~add_warning ~options source_file syntax expression env =
  let mini_c_exp = expression_to_mini_c ~raise ~add_warning ~options source_file syntax expression env in
  let compiled   = Of_mini_c.compile_expression ~options mini_c_exp in
  compiled


let compile_contract_input ~raise ~add_warning ~options parameter storage source_file syntax init_prog =
  let meta       = Of_source.extract_meta ~raise syntax source_file in
  let (parameter,_),(storage,_) = Of_source.compile_contract_input ~raise ~options ~meta parameter storage in
  let aggregated_prg = Of_typed.compile_program ~raise init_prog in
  let imperative = Of_c_unit.compile_contract_input ~raise ~meta parameter storage in
  let sugar      = Of_imperative.compile_expression ~raise imperative in
  let core       = Of_sugar.compile_expression sugar in
  let typed      = Of_core.compile_expression ~raise ~add_warning ~options ~init_prog core in
  let aggregated = Of_typed.compile_expression_in_context ~raise typed aggregated_prg  in
  let mini_c     = Of_aggregated.compile_expression ~raise aggregated in
  let compiled   = Of_mini_c.compile_expression ~raise ~options mini_c in
  compiled

let pretty_print ~raise ~options ~meta file_path =
  let c_unit,_ = to_c_unit ~raise ~options ~meta file_path in
  Of_c_unit.pretty_print ~raise ~meta c_unit file_path

let pretty_print_cst ~raise ~options ~meta file_path =
  let c_unit,_ = to_c_unit ~raise ~options ~meta file_path in
  Of_c_unit.pretty_print_cst ~raise ~meta c_unit file_path
