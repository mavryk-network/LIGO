(* This module collects pretty printing stuff that we use in hover
   and some functions for debug printing *)

open Imports

let pp_with_yojson (f : 'a -> Yojson.Safe.t) : 'a Fmt.t =
 fun formatter link -> Yojson.Safe.pretty_print formatter @@ f link


let show_with_yojson (f : 'a -> Yojson.Safe.t) (x : 'a) : string =
  Yojson.Safe.to_string @@ f x


let get_comment syntax =
  let block =
    match syntax with
    | Syntax_types.CameLIGO -> Preprocessing_cameligo.Config.block
    | Syntax_types.JsLIGO -> Preprocessing_jsligo.Config.block
    | Syntax_types.PascaLIGO -> Preprocessing_pascaligo.Config.block
  in
  match block with
  | Some x -> x#opening, x#closing
  | _ -> "", ""


type module_pp_mode =
  { module_keyword : string
  ; import_keyword : string
  ; sign_on_definition : string option
  ; sign_on_import : string option
  ; open_ : string
  ; close : string
  ; semicolon_at_the_end : bool
  }

let cameligo_module =
  { module_keyword = "module"
  ; import_keyword = "module"
  ; sign_on_definition = Some "="
  ; sign_on_import = Some "="
  ; open_ = "struct"
  ; close = "end"
  ; semicolon_at_the_end = false
  }


let jsligo_module =
  { module_keyword = "namespace"
  ; import_keyword = "import"
  ; sign_on_definition = None
  ; sign_on_import = Some "="
  ; open_ = "{"
  ; close = "}"
  ; semicolon_at_the_end = true
  }


let pascaligo_module =
  { module_keyword = "module"
  ; import_keyword = "module"
  ; sign_on_definition = Some "is"
  ; sign_on_import = Some "is"
  ; open_ = "{"
  ; close = "}"
  ; semicolon_at_the_end = false
  }


let pp_type_expression
    :  syntax:Syntax_types.t
    -> [ `Core of Ast_core.type_expression | `Typed of Ast_typed.type_expression ]
    -> ( PPrint.document
       , [ `Exn of exn | `PassesError of Passes.Errors.t ] * string )
       Result.t
  =
 fun ~syntax te ->
  let ast =
    match te with
    | `Core ast -> ast
    | `Typed tast -> Checking.untype_type_expression tast
  in
  let cst =
    try
      match
        Simple_utils.Trace.to_stdlib_result @@ Nanopasses.decompile_ty_expr ~syntax ast
      with
      | Error (err, _warnings) -> `PassesError err
      | Ok (s, _warnings) ->
        (match syntax with
        | JsLIGO -> `JsLIGO_te (Unification_jsligo.Decompile.decompile_type_expression s)
        | CameLIGO ->
          `CameLIGO_te (Unification_cameligo.Decompile.decompile_type_expression s)
        | PascaLIGO ->
          `PascaLIGO_te (Unification_pascaligo.Decompile.decompile_type_expression s))
    with
    | exn -> `Exn exn
  in
  match cst with
  | `CameLIGO_te cst ->
    Ok Parsing.Cameligo.Pretty.(print_type_expr default_environment cst)
  | `JsLIGO_te cst -> Ok Parsing.Jsligo.Pretty.(print_type_expr default_environment cst)
  | `PascaLIGO_te cst ->
    Ok Parsing.Pascaligo.Pretty.(print_type_expr default_environment cst)
  | (`PassesError _ | `Exn _) as err ->
    Error
      ( err
      , match te with
        | `Core cte -> Format.asprintf "%a" Ast_core.PP.type_expression cte
        | `Typed tte -> Format.asprintf "%a" Ast_typed.PP.type_expression tte )


let print_module_with_description
    : module_pp_mode -> string * string -> Scopes.Types.mdef -> string
  =
 fun description (opening_comment, closing_comment) mdef ->
  match mdef.mod_case with
  | Def _ ->
    description.module_keyword
    ^ " "
    ^ mdef.name
    ^ (Option.value ~default:" "
      @@ Option.map ~f:(fun s -> " " ^ s ^ " ") description.sign_on_definition)
    ^ description.open_
    ^ " "
    ^ opening_comment
    ^ " ... " (* TODO: print module*)
    ^ closing_comment
    ^ " "
    ^ description.close
  | Alias module_path_list ->
    description.import_keyword
    ^ " "
    ^ mdef.name
    ^ (Option.value ~default:" "
      @@ Option.map ~f:(fun s -> " " ^ s ^ " ") description.sign_on_import)
    ^ (module_path_list
      |> List.map ~f:(String.split ~on:'#')
      |> List.map ~f:(Fun.flip List.nth_exn 0)
      |> String.concat ~sep:".")


let print_module : Syntax_types.t -> Scopes.Types.mdef -> string = function
  | CameLIGO -> print_module_with_description cameligo_module (get_comment CameLIGO)
  | JsLIGO -> print_module_with_description jsligo_module (get_comment JsLIGO)
  | PascaLIGO -> print_module_with_description pascaligo_module (get_comment PascaLIGO)


(* Functions made for debugging *)

let location_to_string (location : Loc.t) = Format.asprintf "%a" Loc.pp location

let checking_error_to_string (error : Checking.Errors.typer_error) : string =
  let display_format = Simple_utils.Display.Human_readable in
  Format.asprintf
    "%a"
    (Checking.Errors.error_ppformat ~display_format ~no_colour:false)
    error

    let default_line_width_for_formatted_file = 80
    let default_line_width_for_hovers = 60
