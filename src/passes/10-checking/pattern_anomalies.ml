module AST = Ast_typed
module T = Stage_common.Types
module C = AST.Combinators

module LMap = AST.LMap

let unit_label = T.Label "#UNIT"
let cons_label = T.Label "#CONS"
let nil_label  = T.Label "#NIL"

type simple_pattern =
    SP_wildcard
  | SP_Tuple of simple_pattern list
  | SP_Constructor of T.label * simple_pattern * AST.type_expression

let rec pp_simple_pattern ppf sp =
  match sp with
    SP_wildcard -> Format.fprintf ppf "_"
  | SP_Tuple ps -> Format.fprintf ppf "(%s)" 
    (String.concat ~sep:", " 
      (List.map ps ~f:(fun p -> Format.asprintf "%a" pp_simple_pattern p)))
  | SP_Constructor (Label c, p, _) ->
    Format.fprintf ppf "%s (%a)" c pp_simple_pattern p

let get_variant_nested_type label (t_sum : AST.t_sum) =
  let label_map = t_sum.content in
  let c = LMap.find_opt label label_map in
  let c = Option.value_exn c in (* BAD *)
  c.associated_type

let rec to_simple_pattern ty_pattern =
  let pattern, ty = ty_pattern in
  let pattern = T.Location.unwrap pattern in
  match pattern with
    AST.P_unit -> SP_Constructor (unit_label, SP_wildcard, C.t_unit ())
  | P_var _    -> SP_wildcard
  | P_list (Cons (hd, tl)) ->
    let hd_ty = Option.value_exn (C.get_t_list ty) in (* BAD *)
    let tup = SP_Tuple [
      to_simple_pattern (hd, hd_ty);
      to_simple_pattern (tl, ty)] in
    SP_Constructor (cons_label, tup, ty)
  | P_list (List ps) ->
    let hd_ty = Option.value_exn (C.get_t_list ty) in (* BAD *)
    List.fold_right ps ~init:(SP_Constructor (nil_label, SP_wildcard, ty))
      ~f:(fun p acc ->
        let tup = SP_Tuple [to_simple_pattern (p, hd_ty); acc] in
        SP_Constructor (cons_label, tup, hd_ty))
  | P_variant (c, p) ->
    let p_ty = get_variant_nested_type c (Option.value_exn (C.get_t_sum ty)) in
    SP_Constructor (c, to_simple_pattern (p, p_ty), ty)
  | P_tuple ps
  | P_record (_, ps) ->
    let ps_tys = Option.value_exn (C.get_t_tuple ty) in (* BAD *)
    let ps = List.zip_exn ps ps_tys in
    SP_Tuple (List.map ps ~f:to_simple_pattern)

  let find_anomaly eqs =
    List.iter eqs
      ~f:(fun (p, t, _) -> 
        let sp = to_simple_pattern (p, t) in
        Format.printf "%a\n" pp_simple_pattern sp)
