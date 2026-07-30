type t = Boreas [@@deriving eq, compare, sexp]

let current = Boreas
let in_use = Boreas (* Protocol we depend on *)

let variant_to_string : t -> string =
 fun s ->
  match s with
  | Boreas -> "boreas"


(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = List.map ~f:variant_to_string [ Boreas ]

let protocols_to_variant : string -> t option =
 fun p ->
  match p with
  | "current" -> Some current
  | s when String.equal s (variant_to_string Boreas) -> Some Boreas
  | i when not (List.exists ~f:(String.equal i) protocols_str) -> None
  | _ -> failwith "internal error: forgot to add the protocol string form to the list ?"
