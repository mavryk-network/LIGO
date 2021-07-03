open Cmdliner

let (let*) = Result.bind

let output_directory =
  let doc = "Output files at given directory." in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc)

let generate_cameligo_syntax_highlighting output_directory = 
  let open SyntaxHighlighting.Textmate in
  match CameLIGO.syntax_highlighting () with 
    Ok s -> 
      let fmt = Format.formatter_of_out_channel (open_out @@ Filename.concat output_directory "mligo.tmLanguage.json") in
      Format.fprintf fmt "%s" s
  | Error Referenced_rule_does_not_exist s -> Format.printf "Referenced rule '%s' does not exist." s
  | Error Not_a_valid_reference s -> Format.printf "Not a valid reference: '%s'." s
  | Error Meta_name_some_but_empty s -> Format.printf "%s.name has no value, but is expected to." s
  | Error Begin_cant_be_empty s -> Format.printf "%s.begin_ can't be empty." s
  | Error End_cant_be_empty s -> Format.printf "%s.end_ can't be empty" s

let generate_pascaligo_syntax_highlighting output_directory = 
  let open SyntaxHighlighting.Textmate in
  match PascaLIGO.syntax_highlighting () with 
    Ok s -> 
      let fmt = Format.formatter_of_out_channel (open_out @@ Filename.concat output_directory "ligo.tmLanguage.json") in
      Format.fprintf fmt "%s" s
  | Error Referenced_rule_does_not_exist s -> Format.printf "Referenced rule '%s' does not exist." s
  | Error Not_a_valid_reference s -> Format.printf "Not a valid reference: '%s'." s
  | Error Meta_name_some_but_empty s -> Format.printf "%s.name has no value, but is expected to." s
  | Error Begin_cant_be_empty s -> Format.printf "%s.begin_ can't be empty." s
  | Error End_cant_be_empty s -> Format.printf "%s.end_ can't be empty" s

let generate_reasonligo_syntax_highlighting output_directory = 
  let open SyntaxHighlighting.Textmate in
  match ReasonLIGO.syntax_highlighting () with 
    Ok s -> 
      let fmt = Format.formatter_of_out_channel (open_out @@ Filename.concat output_directory "religo.tmLanguage.json") in
      Format.fprintf fmt "%s" s
  | Error Referenced_rule_does_not_exist s -> Format.printf "Referenced rule '%s' does not exist." s
  | Error Not_a_valid_reference s -> Format.printf "Not a valid reference: '%s'." s
  | Error Meta_name_some_but_empty s -> Format.printf "%s.name has no value, but is expected to." s
  | Error Begin_cant_be_empty s -> Format.printf "%s.begin_ can't be empty." s
  | Error End_cant_be_empty s -> Format.printf "%s.end_ can't be empty" s


let output output_directory = 
  if not (Sys.is_directory output_directory) then 
    `Error (false, "Not a valid directory")
  else (
    generate_cameligo_syntax_highlighting output_directory;
    generate_pascaligo_syntax_highlighting output_directory;
    generate_reasonligo_syntax_highlighting output_directory;
    `Ok "Generated syntax highlighting successful")

let generate_syntax_highlighting = 
  let doc = "generate syntax highlighting" in
  let exits = Term.default_exits in
  Term.(ret (const output $ output_directory)), Term.info "LigoSyntaxHighlighting" ~exits ~doc

let () = Term.(exit @@ eval generate_syntax_highlighting)
