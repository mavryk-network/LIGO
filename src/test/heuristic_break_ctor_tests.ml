open Test_helpers
(* open Main_errors *)

module Core = Typesystem.Core
open Ast_typed.Types
open Ast_typed.Reasons
open Trace

let mk p_ctor_tag p_ctor_args =  (wrap (Todo "unit test") @@ P_constant { p_ctor_tag ; p_ctor_args ; })
(* A bunch of arbitrary types (they only need to be distrinct type constructors without arguments, feel free to replace the contents if/when some of these types move to the stdlib and aren't built-in anymore). *)
let (int, unit, nat, string, bytes, mutez) = (mk C_int [], mk C_unit [], mk C_nat [], mk C_string [], mk C_bytes [], mk C_mutez [])
(* An arbitrary two-argument type constructor (this only needs to be a type constructor with two arguments, feel free to replace). *)
let map (k,v) = mk C_map [k; v]
(* A bunch of type variables: *)
let (m,n,o,p,x,y,z) = let v name : type_variable = Var.fresh ~name () in v "m", v "n", v "o", v "p", v "x", v "y", v "z"

module Map = Database_plugins.All_plugins.GroupedByVariable
let selector_test : (type_constraint_simpl -> type_variable Map.t -> output_break_ctor list) -> unit -> (unit,Main_errors.all) result =
  fun _selector () ->
    (*create a state :) *)
  ok ()

let main =
  test_suite "Typer : ctor break heuristic" @@
    [
      test "selector" (selector_test Typer_new.Heuristic_break_ctor.selector_) ;
    ]
