open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

module rec _ : DOC = struct
end
and Pass: PASS = struct
  
let compile ~syntax =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_proj { struct_; path = FieldName label } ->
      e_record_access ~loc { struct_; label }
    | E_proj { struct_; path = Component_num (label_str, _) } ->
      e_record_access ~loc { struct_; label = Label.of_string label_str }
    | E_proj { struct_; path = Component_expr key } ->
      let map_access =
        e_constant ~loc { cons_name = C_MAP_FIND_OPT; arguments = [ key; struct_ ] }
      in
      if Syntax_types.equal syntax JsLIGO
      then (
        (* this is weird, might be a TODO in the unification of CST representation ? *)
        match get_e key with
        | E_literal (Literal_int n) ->
          e_record_access ~loc { struct_; label = Label.of_z n }
        | _ -> map_access)
      else map_access
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_proj _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise ~syntax =
  morph
    ~name:__MODULE__
    ~compile:(compile ~syntax)
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)

end
