let () = Ligo_lsp.Main.run ()

let () =
  let result = Cli.run () in
  exit result

