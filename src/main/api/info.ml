open Api_helpers
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let measure_contract source_file entry_point declared_views syntax infer protocol_version display_format werror =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format Formatter.contract_size_format get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      let options = Compiler_options.make ~infer ~protocol_version () in
      let michelson,e =  Build.build_contract ~raise ~add_warning ~options syntax entry_point source_file in
      let views = Build.build_views ~raise ~add_warning ~options syntax entry_point (declared_views,e) source_file in
      let contract = Compile.Of_michelson.build_contract ~raise michelson views in
      Compile.Of_michelson.measure ~raise contract

let list_declarations source_file syntax display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format Formatter.declarations_format get_warnings @@
      fun ~raise ->
      let options       = Compiler_options.make () in
      let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
      let core_prg = Compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit source_file in
      let declarations  = Compile.Of_core.list_declarations core_prg in
      (source_file, declarations)

module SSet = Set.Make(String)
let rec explore_directories dirs parent = 
  let dirs = List.map ~f:(explore_dir parent) dirs in
  let uniq_dirs = List.fold_left 
    ~f:(fun set dirs -> Set.union set @@ SSet.of_list dirs) 
    ~init:SSet.empty dirs in
  SSet.to_list uniq_dirs

and explore_dir segs dir = 
  match (Unix.lstat dir).st_kind with
    Unix.S_DIR -> 
      let dirs = Array.to_list @@ Sys.readdir dir in
      let () = Sys.chdir dir in
      let dirs = explore_directories dirs (dir :: segs) in
      let () = Sys.chdir ".." in
      List.fold_left ~f:(fun acc dir -> dir :: acc) ~init:[] dirs 
  | _ -> [String.concat ~sep:Filename.dir_sep @@ List.rev segs]

let explore_directories dirs =
  let cwd = Sys.getcwd () in
  let dirs = try explore_directories dirs []
  with _ -> dirs in
  let () = Sys.chdir cwd in
  dirs

let get_scope source_file syntax infer protocol_version libs display_format with_types =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format Scopes.Formatter.scope_format get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      let libs = explore_directories libs in
      let options = Compiler_options.make ~infer ~protocol_version ~libs () in
      let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
      let core_prg = Compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit source_file in
      Scopes.scopes ~options ~with_types core_prg
