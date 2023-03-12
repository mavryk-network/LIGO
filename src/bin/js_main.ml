open Js_of_ocaml
module Api = Ligo_api
module Default_options = Compiler_options.Default_options
module Raw_options = Compiler_options.Raw_options

let main source syntax =
  let entry_point = [ "main" ] in
  let views = Default_options.views in
  let syntax_v =
    match
      Syntax.of_ext_opt ~support_pascaligo:Default_options.deprecated (Some syntax)
    with
    | Some v -> v
    | None -> failwith ("Invalid syntax " ^ syntax)
  in
  let protocol_version = "lima" in
  let display_format = Simple_utils.Display.human_readable in
  let michelson_format = `Text in
  let michelson_comments = [ `Source ] in
  let raw_options =
    Raw_options.make
      ~entry_point
      ~syntax
      ~views
      ~protocol_version
      ~disable_michelson_typechecking:true
      ~experimental_disable_optimizations_for_debugging:false
      ~enable_typed_opt:false
      ~no_stdlib:false
      ~warning_as_error:false
      ~no_colour:true
      ~constants:Default_options.constants
      ~file_constants:None
      ~project_root:None
      ~warn_unused_rec:true
      ()
  in
  match
    Api.Compile.contract
      raw_options
      (Api.Compile.Text (source, syntax_v))
      display_format
      michelson_format
      michelson_comments
      ()
  with
  | Ok (a, b) ->
    print_endline a;
    print_endline b;
    a
  | Error (a, b) ->
    print_endline "error";
    print_endline a;
    print_endline b;
    "<failed>"

let test code syntax =
  let entry_point = [ "main" ] in
  let views = Default_options.views in
  let syntax_v =
    match
      Syntax.of_ext_opt ~support_pascaligo:Default_options.deprecated (Some syntax)
    with
    | Some v -> v
    | None -> failwith ("Invalid syntax " ^ syntax)
  in
  let protocol_version = "lima" in
  let display_format = Simple_utils.Display.human_readable in
  let raw_options =
    Raw_options.make
      ~entry_point
      ~syntax
      ~views
      ~protocol_version
      ~disable_michelson_typechecking:true
      ~experimental_disable_optimizations_for_debugging:false
      ~enable_typed_opt:false
      ~no_stdlib:false
      ~warning_as_error:false
      ~no_colour:true
      ~constants:Default_options.constants
      ~file_constants:None
      ~project_root:None
      ~warn_unused_rec:true
      ()
  in
  match
    Api.Run.test raw_options (Build.Source_input.Raw { id = "source_of_text" ^ (Syntax.to_ext syntax_v) ; code }) display_format true ()
  with
  | Ok (a, b) ->
    print_endline a;
    print_endline b;
    a
  | Error (a, b) ->
    print_endline "error";
    print_endline a;
    print_endline b;
    "<failed>"


let _ =
  Js.export
    "ligo"
    (object%js
       method test code syntax =
         let code = Js.to_string code in
         let syntax = Js.to_string syntax in
         let michelson = test code syntax in
         Js.string michelson
       method compile code syntax =
         let code = Js.to_string code in
         let syntax = Js.to_string syntax in
         let michelson = main code syntax in
         Js.string michelson
    end)
