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

let type_file ~raise ~add_warning ~(options : Compiler_options.t) f stx form : Ast_typed.program =
  let meta          = Of_source.extract_meta stx in
  let c_unit,_      = Of_source.compile ~raise ~options:options.frontend ~meta f in
  let core          = to_core ~raise ~add_warning ~options ~meta c_unit f in
  let typed         = Of_core.typecheck ~raise ~add_warning ~options form core in
  typed

let core_expression_string ~raise ~add_warning syntax expression =
  let meta              =  Of_source.make_meta_from_syntax syntax in
  let c_unit_exp, _     = Of_source.compile_string_without_preproc expression in
  let imperative_exp    = Of_c_unit.compile_expression ~add_warning ~raise ~meta c_unit_exp in
  let sugar_exp         = Of_imperative.compile_expression ~raise imperative_exp in
  Of_sugar.compile_expression ~raise sugar_exp  

let type_expression_string ~add_warning ~raise ~options syntax expression init_prog =
  let core_exp          = core_expression_string ~raise ~add_warning syntax expression in
  Of_core.compile_expression ~add_warning ~raise ~options ~init_prog core_exp

let type_program_string ~raise ~add_warning ~options syntax expression =
  let meta          = Of_source.make_meta_from_syntax syntax in
  let c_unit, _     = Of_source.compile_string_without_preproc expression in
  let imperative    = Of_c_unit.compile_string ~raise ~add_warning ~meta c_unit in
  let sugar         = Of_imperative.compile ~raise imperative in
  let core          = Of_sugar.compile sugar in
  let typed         = Of_core.typecheck ~raise ~add_warning ~options Env core in
  typed,core

let type_expression ~add_warning ~raise ~options syntax expression init_prog =
  let meta              = Of_source.make_meta syntax in (* TODO: should be computed outside *)
  let c_unit_exp, _     = Of_source.compile_string ~raise ~options:options.Compiler_options.frontend ~meta expression in
  let imperative_exp    = Of_c_unit.compile_expression ~add_warning ~raise ~meta c_unit_exp in
  let sugar_exp         = Of_imperative.compile_expression ~raise imperative_exp in
  let core_exp          = Of_sugar.compile_expression ~raise sugar_exp in
  let typed_exp         = Of_core.compile_expression ~add_warning ~raise ~options ~init_prog core_exp in
  typed_exp

let compile_contract_input ~raise ~add_warning ~options parameter storage syntax init_prog =
  let meta       = Of_source.extract_meta syntax in
  let (parameter,_),(storage,_) = Of_source.compile_contract_input ~raise ~options ~meta parameter storage in
  let aggregated_prg = Of_typed.compile_program ~raise init_prog in
  let imperative = Of_c_unit.compile_contract_input ~add_warning ~raise ~meta parameter storage in
  let sugar      = Of_imperative.compile_expression ~raise imperative in
  let core       = Of_sugar.compile_expression ~raise sugar in
  let typed      = Of_core.compile_expression ~raise ~add_warning ~options ~init_prog core in
  let aggregated = Of_typed.compile_expression_in_context ~raise ~add_warning ~options:options.middle_end typed aggregated_prg  in
  let mini_c     = Of_aggregated.compile_expression ~raise aggregated in
  let compiled   = Of_mini_c.compile_expression ~raise ~options mini_c in
  compiled

let pretty_print ~raise ~options ~meta file_path =
  let c_unit,_ = to_c_unit ~raise ~options ~meta file_path in
  Of_c_unit.pretty_print ~raise ~meta c_unit file_path

let pretty_print_cst ~raise ~options ~meta file_path =
  let c_unit,_ = to_c_unit ~raise ~options ~meta file_path in
  Of_c_unit.pretty_print_cst ~raise ~meta c_unit file_path
