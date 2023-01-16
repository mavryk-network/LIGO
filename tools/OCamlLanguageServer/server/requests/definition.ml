open Lsp.Types
open Utils
let get_definition :
     Position.t
  -> Scopes.def list
  -> Scopes.def option =
fun pos -> List.find_opt (is_reference pos)
