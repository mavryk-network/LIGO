module H=Helpers
module Ligo_proto = Environment.Protocols
module Option = Simple_utils.Option
open Simple_utils.Trace
open Errors
open Ast_typed
open H

module CTMap = Simple_utils.Map.Make(struct type t = string let compare x y = String.compare x y end)
type t = typer CTMap.t

let typer_add : typer = fun args tv_opt ->
  match args, tv_opt with
  | [ { type_content = T_constant { injection = Int ; _ } ; _ } ;
      { type_content = T_constant { injection = Int ; _ } ; _ } ],
    (None | Some { type_content = T_constant { injection = Int ; _ } ; _ }) ->
     t_arrow (t_int ()) (t_arrow (t_int ()) (t_int ()) ()) ()
  | [ { type_content = T_constant { injection = Nat ; _ } ; _ } ;
      { type_content = T_constant { injection = Nat ; _ } ; _ } ],
    (None | Some { type_content = T_constant { injection = Nat ; _ } ; _ }) ->
     t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ()
  | _ -> failwith "typer not found"

let tbl = CTMap.of_list [("add", typer_add)]

let external_typers  ~raise c =
  match CTMap.find_opt c tbl with
  | Some typer -> typer
  | _ ->
     raise.raise (corner_case @@ Format.asprintf "Typer not implemented for external %s" c)
