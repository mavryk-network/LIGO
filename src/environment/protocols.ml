type t = Kathmandu | Lima
let current = Kathmandu
let in_use = Lima (* Protocol we depend on *)

(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = [ "kathmandu" ; "lima" ]

let protocols_to_variant : string -> t option = fun p ->
	match p with
	| "current" -> Some current
  | "lima" -> Some Lima
  | "kathmandu" -> Some Kathmandu
	| i when not (List.exists ~f:(String.equal i) protocols_str) -> None
	| _ -> failwith "internal error: forgot to add the protocol string form to the list ?"

let variant_to_string : t -> string = fun s ->
	match s with
  | Lima -> "lima"
  | Kathmandu -> "kathmandu"

let compare : t -> t -> int = fun p q ->
  match p, q with
  | Lima, Lima -> 0
  | Kathmandu, Kathmandu -> 0
  | Kathmandu, Lima -> -1
  | Lima, Kathmandu -> 1

let equal : t -> t -> bool = fun a b -> (compare a b = 0)
