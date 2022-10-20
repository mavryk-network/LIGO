module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region
module Location  = Simple_utils.Location

module AST = Ast_unified

let r_split = Location.r_split
let r_fst x = fst (r_split x)
let w_split (x: 'a Lexing_shared.Wrap.t) : 'a * Location.t =
  (x#payload, Location.lift x#region)
let w_fst x = fst (w_split x)

(* TODO NP : Find a better way to remove [Directive _] in the declaration list *)
let filter_opt : 'a . 'a option list -> 'a list = fun decls ->
  List.fold
    ~init:[]
    ~f:(fun acc elt ->
      match elt with
      | None -> acc
      | Some d -> d :: acc )
    decls
  |> List.rev
