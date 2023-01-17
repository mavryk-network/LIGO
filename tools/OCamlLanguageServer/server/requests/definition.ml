open Lsp.Types
open Utils

let get_definition :
     Position.t
  -> (Location.t -> bool)
  -> Scopes.def list
  -> Scopes.def option =
fun pos location_pred -> List.find_opt (is_reference pos location_pred)
