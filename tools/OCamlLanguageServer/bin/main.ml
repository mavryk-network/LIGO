

(* Main code
  This is the code that creates an instance of the lsp server class
  and runs it as a task. *)
let run () =
  let s = new Server.lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio (s :> Linol_lwt.Jsonrpc2.server) in
  let task = Linol_lwt.Jsonrpc2.run server in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    let e = Printexc.to_string e in
    Printf.eprintf "error: %s\n%!" e;
    exit 1

  (* Finally, we actually run the server *)
let () = run ()
