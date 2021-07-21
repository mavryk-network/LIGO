open Cmdliner


let vscode_directory =
  let doc = "Output VSCode files at given directory." in
  Arg.(required & opt (some string) None & info ["vscode"] ~doc)

let vim_directory =
  let doc = "Output VIM files at given directory." in
  Arg.(required & opt (some string) None & info ["vim"] ~doc)

let output_file output_directory file s =
  let oc = (open_out @@ Filename.concat output_directory file) in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%s" s;
  close_out oc

let vim_syntax_highlighting dir file textmate = 
  let vim_output = SyntaxHighlighting.VIM.to_vim textmate in
  output_file dir file vim_output;
  `Ok "Success"

let vscode_syntax_highlighting: string -> string -> string -> SyntaxHighlighting.Textmate.t -> string Term.ret = fun dir file syntax textmate ->
  let json = SyntaxHighlighting.Textmate.to_json syntax textmate in
  match json with 
    Ok json -> 
      let s = Yojson.Safe.pretty_to_string json in
      output_file dir file s;
      `Ok "Success" 
  | Error SyntaxHighlighting.Textmate.Referenced_rule_does_not_exist s -> `Error (false, Format.sprintf "Referenced rule '%s' does not exist." s)
  | Error Meta_name_some_but_empty s -> `Error (false, Format.sprintf  "%s.name has no value, but is expected to." s)
  | Error Begin_cant_be_empty s -> `Error (false, Format.sprintf  "%s.begin_ can't be empty." s)
  | Error End_cant_be_empty s -> `Error (false, Format.sprintf  "%s.end_ can't be empty" s)

let ( let* ) o f : string Term.ret  =
  match o with
  | `Error _ as e -> e
  | `Help _ as h -> h
  | `Ok x -> f x

let output: string -> string -> _ Term.ret = fun vim_directory vscode_directory -> 
  if not (Sys.is_directory vscode_directory) then 
    `Error (false, "Not a valid directory to output VSCode files")
  else if not (Sys.is_directory vim_directory) then
    `Error (false, "Not a valid directory to output VIM files")
  else (
    let* _ = vscode_syntax_highlighting vscode_directory "ligo.tmLanguage.json" "ligo" PascaLIGO.syntax_highlighting in
    let* _ = vim_syntax_highlighting vim_directory "ligo.vim" PascaLIGO.syntax_highlighting in
    let* _ = vscode_syntax_highlighting vscode_directory "mligo.tmLanguage.json" "mligo" CameLIGO.syntax_highlighting in
    let* _ = vim_syntax_highlighting vim_directory "mligo.vim" CameLIGO.syntax_highlighting in
    let* _ = vscode_syntax_highlighting vscode_directory "religo.tmLanguage.json" "religo" ReasonLIGO.syntax_highlighting in
    let* _ = vim_syntax_highlighting vim_directory "religo.vim" ReasonLIGO.syntax_highlighting in
    (* print_endline "completed"; *)
    `Ok "Successfully generated syntaxes"  
  )

let generate_syntax_highlighting = 
  let doc = "generate syntax highlighting" in
  let exits = Term.default_exits in
  Term.(ret (const output $ vim_directory $ vscode_directory)), Term.info "LigoSyntaxHighlighting" ~exits ~doc

let () = Term.(exit @@ eval generate_syntax_highlighting)
