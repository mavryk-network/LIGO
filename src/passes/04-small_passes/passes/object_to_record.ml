open Ast_unified
open Pass_type
open Simple_utils.Trace

(* open Simple_utils *)
open Errors
module Location = Simple_utils.Location

let is_field = function
  | Object_.Property_rest _ -> false
  | _ -> true

let label_of_var x = Label.of_string @@ Variable.to_name_exn x

let field_of_property ~raise : expr Object_.property -> (Variable.t, expr) Field.t =
 fun p ->
  match p with
  | Property (l, r) ->
    (match get_e l with
    | E_variable v -> Complete (v, r)
    | _ -> raise.error @@ unsupported_object_field l)
  | Punned_property x ->
    (match get_e x with
    | E_variable v -> Punned v
    | _ -> raise.error @@ unsupported_object_field x)
  | Property_rest x -> raise.error @@ unsupported_rest_property x


let field_update_of_property ~raise : expr Object_.property -> expr Update.field =
 fun p ->
  match p with
  | Property (l, r) ->
    (match get_e l with
    | E_variable x ->
      Full_field
        { field_lhs = [ FieldName (Label.of_string @@ Variable.to_name_exn x) ]
        ; field_lens = Lens_Id
        ; field_rhs = r
        }
    | _ ->
      (* could match on E_Proj ? maybe ? *)
      raise.error @@ unsupported_update l)
  | Punned_property x ->
    (match get_e x with
    | E_variable x -> Pun (label_of_var x, [])
    | _ -> raise.error @@ unsupported_update x)
  | Property_rest x -> raise.error @@ unsupported_rest_property x


let compile ~raise =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Object (Property_rest structure, fields) ->
      let update = List.map ~f:(field_update_of_property ~raise) fields in
      e_update ~loc { structure; update }
    | E_Object fields ->
      let fields = List.Ne.map (field_of_property ~raise) fields in
      e_record_pun ~loc (List.Ne.to_list fields)
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_Object _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)