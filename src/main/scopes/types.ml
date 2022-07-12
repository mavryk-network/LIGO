module Definitions = struct
  module Location = Simple_utils.Location
  module List     = Simple_utils.List
  module Def_map = Simple_utils.Map.Make( struct type t = string let compare = String.compare end)

  type type_case =
    | Core of Ast_core.type_expression
    | Resolved of Ast_typed.type_expression
    | Unresolved

  type vdef = {
    name  : Ast_core.expression_variable;
    range : Location.t ;
    body_range : Location.t ;
    t : type_case ;
    references : (Location.t list)
  }

  type tdef = {
    name : string ;
    range : Location.t ;
    body_range : Location.t ;
    content : Ast_core.type_expression ;
  }

  type adef = {
    name       : string ;
    range      : Location.t ;
    body_range : Location.t ;
    content    : Ast_core.module_variable List.Ne.t;
  }

  type mdef = {
    name : string ;
    range : Location.t ;
    body_range : Location.t ;
    content : def_map ;
  }

  and def = Variable of vdef | Type of tdef | Module of mdef
  and def_map = def Def_map.t

  let merge_refs : string -> def -> def -> def option = fun _ a b ->
    match a,b with
    | Variable a , Variable b ->
      let references = List.dedup_and_sort ~compare:Location.compare (a.references @ b.references) in
      Some (Variable { a with references })
    | (Variable _ |Type _ | Module _ ) , (Variable _ |Type _ | Module _ ) -> Some a

  let merge_defs a b =
    Def_map.union merge_refs a b

  let get_def_name = function
    | Variable    d -> Ast_core.Var.to_name_exn d.name
    | Type        d -> d.name
    | Module      d -> d.name

  let get_range = function
    | Type        t -> t.range
    | Variable    v -> v.range
    | Module      m -> m.range

  let make_v_def : Ast_core.expression_variable -> type_case -> Location.t -> Location.t -> def =
    fun name t range body_range ->
      Variable { name ; range ; body_range ; t ; references = [] }

  let make_t_def : string -> Location.t -> Ast_core.type_expression -> def =
    fun name loc te ->
      Type { name ; range = loc ; body_range = te.location ; content = te }

  let make_m_def : string -> Location.t -> _ Def_map.t -> def =
    fun name loc m ->
      Module { name ; range = loc ; body_range = Location.dummy ; content = m }

  let add_reference : Ast_core.expression_variable -> def_map -> def_map = fun x env ->
    let aux : string * def -> bool = fun (_,d) ->
      match d with
      | Variable v -> Ast_core.Var.equal v.name x
      | (Type _ | Module _ ) -> false
    in
    match List.find ~f:aux (Def_map.bindings env) with
    | Some (k,_) ->
      let aux : def option -> def option = fun d_opt ->
        match d_opt with
        | Some (Variable v) -> Some (Variable { v with references = (Ast_core.Var.get_location x :: v.references) })
        | Some x -> Some x
        | None -> None
      in
      Def_map.update k aux env
    | None -> env

end

include Definitions

type scope = { range : Location.t ; env : def_map }
type scopes = scope list

let add_scope (range,env) scopes = { range ; env } :: scopes


module Bindings_map = Simple_utils.Map.Make ( struct type t = Ast_typed.expression_variable let compare = Ast_typed.Compare.expression_variable end )
type bindings_map = Ast_typed.type_expression Bindings_map.t
