open Ligo_prim
open Types

(* This function transforms an application expression `l e1 ... en` into the pair `([ e1 ; ... ; en ] , l)` *)
let destruct_applications (e : expression) =
  let rec destruct_applications acc (lamb : expression) =
    match lamb.expression_content with
    | E_application { lamb; args } -> destruct_applications (args :: acc) lamb
    | _ -> lamb, acc
  in
  destruct_applications [] e


(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) =
    match t.type_content with
    | T_for_all { ty_binder; type_; _ } ->
      destruct_for_alls (ty_binder :: type_vars) type_
    | _ -> List.rev type_vars, t
  in
  destruct_for_alls [] t


module Free_type_variables = struct
  module Set = Type_var.Set

  let rec map_type_expression : bound:Set.t -> type_expression -> Set.t =
   fun ~bound type_ ->
    let self ?(bound = bound) type_ = map_type_expression ~bound type_ in
    match type_.type_content with
    | T_sum row | T_record row -> map_row ~bound row
    | T_tuple types -> types |> List.map ~f:self |> Set.union_list
    | T_arrow { type1; type2 } -> Set.union (self type1) (self type2)
    | T_app { arguments; _ } -> arguments |> List.map ~f:self |> Set.union_list
    | T_variable tvar when Set.mem bound tvar -> Set.empty
    | T_variable tvar -> Set.singleton tvar
    | T_module_accessor _ -> Set.empty
    | T_singleton _ -> Set.empty
    | T_abstraction { ty_binder; type_; _ } ->
      self ~bound:(Set.add bound ty_binder) type_
    | T_for_all { ty_binder; type_; _ } ->
      self ~bound:(Set.add bound ty_binder) type_


  and map_row : bound:Set.t -> ty_expr Rows.t -> Set.t =
   fun ~bound row ->
    row.fields
    |> Map.data
    |> List.map ~f:(fun { Rows.Elem.associated_type; _ } ->
           map_type_expression ~bound associated_type)
    |> Set.union_list


  let type_expression : Type_var.t list -> type_expression -> Type_var.t list =
   fun bound type_ ->
    map_type_expression ~bound:(Set.of_list bound) type_ |> Set.to_list
end
