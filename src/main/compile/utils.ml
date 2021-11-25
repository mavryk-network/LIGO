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

let type_file ~raise ~add_warning ~options ~meta ~env f form : Ast_typed.module_fully_typed * Ast_typed.environment =
  let c_unit,_      = Of_source.compile ~raise ~options ~meta f in
  let core          = to_core ~raise ~add_warning ~options ~meta c_unit f in
  let inferred      = Of_core.infer ~raise ~options ~env core in
  let typed,e       = Of_core.typecheck ~raise ~add_warning ~options ~meta ~env form inferred in
  (typed,e)

let to_mini_c ~raise ~add_warning ~options ~meta ~env f form =
  let typed,_  = type_file ~raise ~add_warning ~options ~meta ~env f form in
  let mini_c     = Of_typed.compile ~raise typed in
  mini_c

let compile_file ~raise ~add_warning ~options ~meta ~env f ep =
  let typed,_    = type_file ~raise ~add_warning ~options ~meta ~env f @@ Contract ep in
  let mini_c     = Of_typed.compile ~raise typed in
  let michelson  = Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c ep in
  let contract   = Of_michelson.build_contract ~raise michelson in
  contract

let type_expression_string ~raise ~options ~meta ~env expression =
  let c_unit_exp, _     = Of_source.compile_string_without_preproc expression in
  let imperative_exp    = Of_c_unit.compile_expression ~raise ~meta c_unit_exp in
  let sugar_exp         = Of_imperative.compile_expression ~raise imperative_exp in
  let core_exp          = Of_sugar.compile_expression sugar_exp in
  let typed_exp,e       = Of_core.compile_expression ~raise ~options ~env core_exp in
  (typed_exp,e)

let type_contract_string ~raise ~add_warning ~options ~meta ~env expression =
  let c_unit, _     = Of_source.compile_string_without_preproc expression in
  let imperative    = Of_c_unit.compile_string ~raise ~add_warning ~meta c_unit in
  let sugar         = Of_imperative.compile ~raise imperative in
  let core          = Of_sugar.compile sugar in
  let inferred      = Of_core.infer ~raise ~options ~env core in
  let typed,e       = Of_core.typecheck ~raise ~add_warning ~options ~meta ~env Env inferred in
  (typed,core,e)

let type_expression ~raise ~options ~meta ~env expression =
  let c_unit_exp, _     = Of_source.compile_string ~raise ~options ~meta expression in
  let imperative_exp    = Of_c_unit.compile_expression ~raise ~meta c_unit_exp in
  let sugar_exp         = Of_imperative.compile_expression ~raise imperative_exp in
  let core_exp          = Of_sugar.compile_expression sugar_exp in
  let typed_exp,e       = Of_core.compile_expression ~raise ~options ~env core_exp in
  (typed_exp,e)

let expression_to_mini_c ~raise ~options ~meta ~env expression =
  let (typed_exp,_)  = type_expression ~raise ~options ~meta ~env expression in
  let mini_c_exp     = Of_typed.compile_expression ~raise typed_exp in
  mini_c_exp

let compile_expression ~raise ~options ~meta ~env expression =
  let mini_c_exp = expression_to_mini_c ~raise ~options ~meta ~env expression in
  let compiled   = Of_mini_c.compile_expression ~options mini_c_exp in
  compiled

let compile_and_aggregate_expression ~raise ~options ~meta ~env expression mini_c_prg =
  let mini_c_exp = expression_to_mini_c ~raise ~options ~meta ~env expression in
  let compiled   = Of_mini_c.aggregate_and_compile_expression ~raise ~options mini_c_prg mini_c_exp in
  compiled

let compile_storage ~raise ~options ~meta ~env storage input mini_c_prg =
  let (storage,_),(input,_) = Of_source.compile_contract_input ~raise ~options ~meta storage input in
  let imperative = Of_c_unit.compile_contract_input ~raise ~meta storage input in
  let sugar      = Of_imperative.compile_expression ~raise imperative in
  let core       = Of_sugar.compile_expression sugar in
  let typed,_    = Of_core.compile_expression ~raise ~options ~env core in
  let mini_c     = Of_typed.compile_expression ~raise typed in
  let compiled   = Of_mini_c.aggregate_and_compile_expression ~raise ~options mini_c_prg mini_c in
  compiled

let pretty_print ~raise ~options ~meta file_path =
  let c_unit,_ = to_c_unit ~raise ~options ~meta file_path in
  Of_c_unit.pretty_print ~raise ~meta c_unit file_path

let pretty_print_cst ~raise ~options ~meta file_path =
  let c_unit,_ = to_c_unit ~raise ~options ~meta file_path in
  Of_c_unit.pretty_print_cst ~raise ~meta c_unit file_path
