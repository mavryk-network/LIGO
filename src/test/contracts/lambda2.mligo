type storage = unit

(* Not supported yet:
let main (a, s : unit * storage) = (fun x -> ()) ()
*)

let main (_ : unit) (_ : storage) =
  (fun (f : unit -> unit) -> f ()) (fun (_ : unit) -> unit)
