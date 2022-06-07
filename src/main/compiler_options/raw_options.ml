type raw = {
  (* Formatter *)
  warning_as_error : bool ;

  (* Warnings *)
  warn_unused_rec : bool ;

  (* Frontend *)
  syntax : string ;
  entry_point : string ;
  libraries : string list ;
  project_root : string option ;
  
  (* Tools *)
  with_types : bool ;
  self_pass : bool ;
  
  (* Test framework *)
  test : bool ;
  steps : int ;
  generator : string ;
  cli_expr_inj : string option ;
  
  (* Backend *)
  protocol_version : string ;
  disable_michelson_typechecking : bool ;
  enable_typed_opt : bool ;
  without_run : bool ;
  views : string list ;
  constants : string list ;
  file_constants : string option ;
}

module Default_options = struct 
  (* Formatter *)
  let show_warnings = true
  let warning_as_error = false

  (* Warnings *)
  let warn_unused_rec = false
  
  (* Frontend *)
  let syntax = "auto"
  let dialect = "terse"
  let entry_point = "main"
  let libraries = []
  let project_root = None

  (* Tools *)
  let infer = false
  let with_types = false
  let self_pass = false
  
  (* Test framework *)
  let test = false
  let steps = 1000000
  let generator = "random"
  let cli_expr_inj = None
  
  (* Backend *)
  let protocol_version = "current"
  let disable_michelson_typechecking = false
  let enable_typed_opt = false
  let without_run = false
  let views = []
  let constants = []
  let file_constants = None
end

let make 
  ?(warning_as_error = Default_options.warning_as_error)
  ?(warn_unused_rec = Default_options.warn_unused_rec)
  ?(syntax = Default_options.syntax)
  ?(entry_point = Default_options.entry_point)
  ?(libraries = Default_options.libraries)
  ?(project_root = Default_options.project_root)
  ?(with_types = Default_options.with_types)
  ?(self_pass = Default_options.self_pass)
  ?(test = Default_options.test)
  ?(steps = Default_options.steps)
  ?(generator = Default_options.generator)
  ?(cli_expr_inj = Default_options.cli_expr_inj)
  ?(protocol_version = Default_options.protocol_version)
  ?(disable_michelson_typechecking = Default_options.disable_michelson_typechecking)
  ?(enable_typed_opt = Default_options.enable_typed_opt)
  ?(without_run = Default_options.without_run)
  ?(views = Default_options.views)
  ?(constants = Default_options.constants)
  ?(file_constants = Default_options.file_constants)
  () = 
{
  (* Formatter *)
  warning_as_error ;

  (* Warnings *)
  warn_unused_rec ;
  
  (* Frontend *)
  syntax ;
  entry_point ;
  libraries ;
  project_root ;
  
  (* Tools *)
  with_types ;
  self_pass ;
  
  (* Test framework *)
  test ;
  steps ;
  generator ;
  cli_expr_inj ;
  
  (* Backend *)
  protocol_version ;
  disable_michelson_typechecking ;
  enable_typed_opt ;
  without_run ;
  views ;
  constants ;
  file_constants ;
}

let default =
{
  (* Formatter *)
  warning_as_error = Default_options.show_warnings ;

  (* Warnings *)
  warn_unused_rec = Default_options.warn_unused_rec ;
  
  (* Frontend *)
  syntax = Default_options.syntax ;
  entry_point = Default_options.entry_point ;
  libraries = Default_options.libraries ;
  project_root = Default_options.project_root ;
  
  (* Tools *)
  with_types = Default_options.with_types ;
  self_pass = Default_options.self_pass ;
  
  (* Test framework *)
  test = Default_options.test ;
  steps = Default_options.steps ;
  generator = Default_options.generator ;
  cli_expr_inj = Default_options.cli_expr_inj ;
  
  (* Backend *)
  protocol_version = Default_options.protocol_version ;
  disable_michelson_typechecking = Default_options.disable_michelson_typechecking ;
  enable_typed_opt = Default_options.enable_typed_opt ;
  without_run = Default_options.without_run ;
  views = Default_options.views ;
  constants = Default_options.constants ;
  file_constants = Default_options.file_constants ;
}
