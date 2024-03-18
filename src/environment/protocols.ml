type t = Atlas [@@deriving eq, compare]

let current = Atlas
let in_use = Atlas (* Protocol we depend on *)

let variant_to_string : t -> string =
 fun s ->
  match s with
  | Atlas -> "atlas"


(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = List.map ~f:variant_to_string [ Atlas ]

let protocols_to_variant : string -> t option =
 fun p ->
  match p with
  | "current" -> Some current
  | s when String.equal s (variant_to_string Atlas) -> Some Atlas
  | i when not (List.exists ~f:(String.equal i) protocols_str) -> None
  | _ -> failwith "internal error: forgot to add the protocol string form to the list ?"
