open Cmdliner

let (let*) = Result.bind

let output_directory =
  let doc = "Output files at given directory." in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc)

let generate_textmate_syntax_highlighting output_directory func file = 
  let open SyntaxHighlighting.Textmate in
  match func () with 
    Ok s -> 
      let fmt = Format.formatter_of_out_channel (open_out @@ Filename.concat output_directory file) in
      Format.fprintf fmt "%s" s;
      `Ok
  | Error Referenced_rule_does_not_exist s -> `Error (false, Format.sprintf "Referenced rule '%s' does not exist." s)
  | Error Meta_name_some_but_empty s -> `Error (false, Format.sprintf  "%s.name has no value, but is expected to." s)
  | Error Begin_cant_be_empty s -> `Error (false, Format.sprintf  "%s.begin_ can't be empty." s)
  | Error End_cant_be_empty s -> `Error (false, Format.sprintf  "%s.end_ can't be empty" s)

let output output_directory = 
  if not (Sys.is_directory output_directory) then 
    `Error (false, "Not a valid directory")
  else (
    let cameligo_syntax_highlighting = generate_textmate_syntax_highlighting output_directory CameLIGO.syntax_highlighting "mligo.tmLanguage.json" in
    match cameligo_syntax_highlighting with 
      `Error e -> `Error e
    | `Ok ->
      let pascaligo_syntax_highlighting = generate_textmate_syntax_highlighting output_directory PascaLIGO.syntax_highlighting "ligo.tmLanguage.json" in
      match pascaligo_syntax_highlighting with 
        `Error e -> `Error e
      | `Ok ->
        let reasonligo_syntax_highlighting = generate_textmate_syntax_highlighting output_directory ReasonLIGO.syntax_highlighting "religo.tmLanguage.json" in
        match reasonligo_syntax_highlighting with 
          `Error e -> `Error e
        | `Ok ->
            `Ok "Generated syntax highlighting successful")

let generate_syntax_highlighting = 
  let doc = "generate syntax highlighting" in
  let exits = Term.default_exits in
  Term.(ret (const output $ output_directory)), Term.info "LigoSyntaxHighlighting" ~exits ~doc

let () = Term.(exit @@ eval generate_syntax_highlighting)
