module C = Configurator.V1

let () =
  C.main ~name:"" (fun _ ->
      C.Flags.write_sexp
        "jsoo-flags.sexp"
        [ Printf.sprintf
            "--source-map-root=%a/ligo-ide"
            (fun () -> Stdlib.Filename.get_temp_dir_name)
            ()
        ])
