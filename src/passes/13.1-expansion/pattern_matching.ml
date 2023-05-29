(**

This implements the pattern_matching compiler of `Peyton-Jones, S.L., The Implementation of Functional Programming Languages`, chapter 5.
By reduction, this algorithm transforms pattern matching expression into (nested) cases expressions.
`Sugar` match expression being 'pattern matching' expression and `Core`/`Typed` being 'case expressions'.

"Product patterns" (e.g. tuple & record) are considered variables, an extra rule (product_rule) handles them

List patterns are treated as the variant type `NIL | Cons of (hd , tl)` would be.
Option patterns are treated as the variant type `Some a | None` would be

**)

module Location = Simple_utils.Location
module Ligo_string = Simple_utils.Ligo_string
module I = Ast_aggregated
module O = Ast_expanded
open Ligo_prim

type matchees = Value_var.t list
type pattern = O.type_expression I.Pattern.t
type typed_pattern = pattern * O.type_expression
type equations = (typed_pattern list * O.expression) list
type rest = O.ty_expr -> O.expression_content

module PP_DEBUG = struct
  let pp_typed_pattern ppf ((p, t) : typed_pattern) =
    Format.fprintf
      ppf
      "(%a : %a)"
      (I.Pattern.pp O.PP.type_expression)
      p
      O.PP.type_expression
      t


  let pp_pattern_list ppf (plist : typed_pattern list) =
    Format.fprintf
      ppf
      "[%a]"
      Simple_utils.PP_helpers.(list_sep pp_typed_pattern (tag "; "))
      plist


  let pp_eq ppf ((plist, expr) : typed_pattern list * O.expression) =
    Format.fprintf ppf "@[%a -> %a]" pp_pattern_list plist O.PP.expression expr


  let pp_eqs ppf (eqs : equations) =
    Format.fprintf
      ppf
      "@[<hv>%a@]"
      Simple_utils.PP_helpers.(list_sep pp_eq (tag "; "))
      eqs


  let pp_partition ppf (part : equations list) =
    Format.fprintf
      ppf
      "@[<hv><@.%a@.>@]"
      Simple_utils.PP_helpers.(list_sep pp_eqs (tag "@."))
      part
end

let is_var : _ I.Pattern.t -> bool =
 fun p ->
  match p.wrap_content with
  | P_var _ -> true
  | P_tuple _ -> true
  | P_record _ -> true
  | P_unit -> true
  | _ -> false


let is_product' : _ I.Pattern.t -> bool =
 fun p ->
  match p.wrap_content with
  | P_tuple _ -> true
  | P_record _ -> true
  | _ -> false


let is_product : equations -> typed_pattern option =
 fun eqs ->
  List.find_map
    ~f:(fun (pl, _) ->
      match pl with
      | (p, t) :: _ -> if is_product' p then Some (p, t) else None
      | [] -> None)
    eqs


let corner_case : type a. string -> a = fun loc -> failwith ("broken invariant at " ^ loc)

let get_pattern_type (eqs : equations) =
  match eqs with
  | ((_, t) :: _, _) :: _ -> t
  | ([], _) :: _ -> corner_case __LOC__ (* typed_pattern list empty *)
  | [] -> corner_case __LOC__ (* equations empty *)


let get_body_type (eqs : equations) =
  match eqs with
  | (_, e) :: _ -> e.type_expression
  | [] -> corner_case __LOC__ (* equations empty *)


let extract_variant_type : pattern -> Label.t -> O.type_expression -> O.type_expression =
 fun _ label t ->
  let loc = t.location in
  match t.type_content with
  | T_sum rows ->
    (match Map.find rows.fields label with
    | Some t -> t
    | None -> corner_case __LOC__)
  | O.T_constant { injection = Literal_types.List; parameters = [ proj_t ]; language = _ }
    ->
    (match label with
    | Label "Cons" -> O.make_t_ez_record ~loc [ "0", proj_t; "1", t ]
    | Label "Nil" -> O.t_unit ~loc ()
    | Label _ -> corner_case __LOC__)
  | _ -> corner_case __LOC__


let extract_record_type : pattern -> Label.t -> O.type_expression -> O.type_expression =
 fun _ label t ->
  match t.type_content with
  | T_record rows ->
    (match Map.find rows.fields label with
    | Some t -> t
    | None -> corner_case __LOC__)
  | _ -> corner_case __LOC__


(**
  `substitute_var_in_body to_subst new_var body` replaces variables equal to `to_subst` with variable `new_var` in expression `body`.
  note that `new_var` here is never a user variable (always previously generated by the compiler)
**)
let rec substitute_var_in_body
    : Value_var.t -> Value_var.t -> O.expression -> O.expression
  =
 fun to_subst new_var body ->
  let aux : unit -> O.expression -> bool * unit * O.expression =
   fun () exp ->
    let ret continue exp = continue, (), exp in
    match exp.expression_content with
    | O.E_variable var when Value_var.equal var to_subst ->
      ret true { exp with expression_content = E_variable new_var }
    | O.E_let_in letin when Binder.apply (Value_var.equal to_subst) letin.let_binder ->
      let rhs = substitute_var_in_body to_subst new_var letin.rhs in
      let letin = { letin with rhs } in
      ret false { exp with expression_content = E_let_in letin }
    | O.E_lambda lamb when Value_var.equal to_subst (Param.get_var lamb.binder) ->
      ret false exp
    | O.E_recursive r when Value_var.equal r.fun_name to_subst -> ret false exp
    | O.E_matching m ->
      let matchee = substitute_var_in_body to_subst new_var m.matchee in
      let cases =
        match m.cases with
        | Match_record { fields; body; tv } ->
          if Record.exists
               ~f:(fun (b : O.type_expression Binder.t) ->
                 Binder.apply (Value_var.equal to_subst) b)
               fields
          then m.cases
          else (
            let body = substitute_var_in_body to_subst new_var body in
            Match_record { fields; body; tv })
        | Match_variant { cases; tv } ->
          let cases =
            List.fold_right cases ~init:[] ~f:(fun case cases ->
                if Value_var.equal case.pattern to_subst
                then case :: cases
                else (
                  let body = substitute_var_in_body to_subst new_var case.body in
                  { case with body } :: cases))
          in
          Match_variant { cases; tv }
      in
      ret false { exp with expression_content = O.E_matching { matchee; cases } }
    | E_literal _
    | E_constant _
    | E_variable _
    | E_application _
    | E_lambda _
    | E_assign _
    | E_let_mut_in _
    | E_while _
    | E_for _
    | E_for_each _
    | E_deref _
    | E_type_abstraction _
    | E_recursive _
    | E_let_in _
    | E_raw_code _
    | E_constructor _
    | E_record _
    | E_accessor _
    | E_update _
    | E_type_inst _ -> ret true exp
  in
  let (), res = O.Helpers.fold_map_expression aux () body in
  res


let make_var_pattern : Value_var.t -> O.type_expression -> pattern =
 fun var t ->
  Location.wrap ~loc:(Value_var.get_location var) @@ I.Pattern.P_var (Binder.make var t)


let rec partition : ('a -> bool) -> 'a list -> 'a list list =
 fun f lst ->
  let add_inner x ll =
    match ll with
    | hdl :: tll -> (x :: hdl) :: tll
    | _ -> assert false
  in
  match lst with
  | [] -> []
  | [ x ] -> [ [ x ] ]
  | x :: x' :: tl ->
    if Bool.( = ) (f x) (f x')
    then add_inner x (partition f (x' :: tl))
    else [ x ] :: partition f (x' :: tl)


(**
  groups together equations that begin with the same constructor
**)
let group_equations : equations -> equations Record.t =
 fun eqs ->
  let aux : typed_pattern list * O.expression -> equations Record.t -> equations Record.t =
   fun (pl, body) m ->
    let phd, t = List.hd_exn pl in
    let ptl = List.tl_exn pl in
    let upd : O.type_expression -> pattern -> equations option -> equations option =
     fun proj_t pattern kopt ->
      match kopt with
      | Some eqs ->
        let p = pattern, proj_t in
        Some ((p :: ptl, body) :: eqs)
      | None ->
        let p = pattern, proj_t in
        Some [ p :: ptl, body ]
    in
    let loc = phd.location in
    match phd.wrap_content with
    | P_variant (label, p_opt) ->
      let proj_t = extract_variant_type phd label t in
      Record.update_opt m label ~f:(upd proj_t p_opt)
    | P_list (List []) ->
      let label = Label.of_string "Nil" in
      let proj_t = extract_variant_type phd label t in
      Record.update_opt m label ~f:(upd proj_t (Location.wrap ~loc I.Pattern.P_unit))
    | P_list (Cons (p_hd, p_tl)) ->
      let label = Label.of_string "Cons" in
      let pattern = Location.wrap ~loc:phd.location @@ I.Pattern.P_tuple [ p_hd; p_tl ] in
      let proj_t = extract_variant_type phd label t in
      Record.update_opt m label ~f:(upd proj_t pattern)
    | _ -> corner_case __LOC__
  in
  List.fold_right ~f:aux ~init:Record.empty eqs


let rec match_ : matchees -> equations -> rest -> O.expression =
 fun ms eqs def ->
  match ms, eqs with
  | [], [ ([], body) ] -> body
  | [], eqs when List.for_all ~f:(fun (ps, _) -> List.length ps = 0) eqs ->
    corner_case __LOC__
  | _ ->
    let leq = partition (fun (pl, _) -> is_var (fst @@ List.hd_exn pl)) eqs in
    let aux
        (part_eq : equations)
        ((def, _, _) : rest * O.type_expression option * Location.t option)
      =
      let r = consvar ms part_eq def in
      (fun _ -> r.expression_content), Some r.type_expression, Some r.location
    in
    let r, t, location = List.fold_right ~f:aux ~init:(def, None, None) leq in
    let t = Option.value_exn ~here:[%here] t in
    O.make_e ~loc:(Option.value_exn ~here:[%here] location) (r t) t


and consvar : matchees -> equations -> rest -> O.expression =
 fun ms eqs def ->
  let p1s = List.map ~f:(fun el -> fst @@ List.hd_exn @@ fst el) eqs in
  if List.for_all ~f:is_var p1s
  then (
    let product_opt = is_product eqs in
    var_rule product_opt ms eqs def)
  else ctor_rule ms eqs def


and var_rule : typed_pattern option -> matchees -> equations -> rest -> O.expression =
 fun product_opt ms eqs def ->
  match ms with
  | mhd :: mtl ->
    (match product_opt with
    | Some shape -> product_rule shape ms eqs def
    | None ->
      let aux : typed_pattern list * O.expression -> typed_pattern list * O.expression =
       fun (pl, body) ->
        match pl with
        | (phd, _) :: ptl ->
          (match phd.wrap_content with
          | P_var b ->
            let body' = substitute_var_in_body (Binder.get_var b) mhd body in
            ptl, body'
          | P_unit -> ptl, body
          | _ -> corner_case __LOC__)
        | [] -> corner_case __LOC__
      in
      let eqs' = List.map ~f:aux eqs in
      match_ mtl eqs' def)
  | [] -> corner_case __LOC__


and ctor_rule : matchees -> equations -> rest -> O.expression =
 fun ms eqs def ->
  match ms with
  | mhd :: mtl ->
    let loc = Value_var.get_location mhd in
    let matchee_t = get_pattern_type eqs in
    let body_t = get_body_type eqs in
    let matchee = O.e_a_variable ~loc mhd matchee_t in
    let eq_map = group_equations eqs in
    let aux_p : Label.t * equations -> _ O.matching_content_case =
     fun (constructor, eq) ->
      let proj =
        match eq with
        | [ (tp, _) ] ->
          let pattern, _ = List.hd_exn tp in
          let loc = pattern.location in
          (match pattern.wrap_content with
          | P_var x -> Binder.get_var x
          | P_unit -> Value_var.fresh ~loc ~name:"unit_proj" ()
          | _ -> Value_var.fresh ~loc ~name:"ctor_proj" ())
        | _ -> Value_var.fresh ~loc ~name:"ctor_proj" ()
      in
      let new_ms = proj :: mtl in
      let nested = match_ new_ms eq def in
      O.{ constructor; pattern = proj; body = nested }
    in
    let aux_m : Label.t * O.type_expression -> _ O.matching_content_case =
     fun (constructor, t) ->
      let proj = Value_var.fresh ~loc ~name:"ctor_proj" () in
      let body = O.make_e ~loc (def t) t in
      { constructor; pattern = proj; body }
    in
    let grouped_eqs =
      match O.get_t_sum matchee_t with
      | Some _ when Option.is_some (O.get_t_option matchee_t) ->
        List.map
          ~f:(fun label -> label, Record.find_opt eq_map label)
          [ Label.of_string "Some"; Label.of_string "None" ]
      | Some rows ->
        let eq_opt_map =
          Record.mapi ~f:(fun ~label ~value:_ -> Record.find_opt eq_map label) rows.fields
        in
        Record.to_list @@ eq_opt_map
      | None ->
        (* REMITODO: parametric types in env ? *)
        (match O.get_t_list matchee_t with
        | Some _ ->
          List.map
            ~f:(fun label -> label, Record.find_opt eq_map label)
            [ Label.of_string "Cons"; Label.of_string "Nil" ]
        | None -> corner_case __LOC__ (* should be caught when typing the matchee *))
    in
    let present =
      List.filter_map
        ~f:(fun (c, eq_opt) ->
          match eq_opt with
          | Some eq -> Some (c, eq)
          | None -> None)
        grouped_eqs
    in
    let present_cases = List.map present ~f:aux_p in
    let missing =
      List.filter_map
        ~f:(fun (c, eq_opt) ->
          match eq_opt with
          | Some _ -> None
          | None -> Some (c, body_t))
        grouped_eqs
    in
    let missing_cases = List.map ~f:aux_m missing in
    let cases =
      O.Match_variant { cases = missing_cases @ present_cases; tv = matchee_t }
    in
    O.make_e ~loc (O.E_matching { matchee; cases }) body_t
  | [] -> corner_case __LOC__


and product_rule : typed_pattern -> matchees -> equations -> rest -> O.expression =
 fun product_shape ms eqs def ->
  match ms with
  | mhd :: _ ->
    let lb : (_ * _ Binder.t) list =
      let p, t = product_shape in
      match p.wrap_content, t with
      | P_tuple ps, t ->
        let aux : int -> _ I.Pattern.t -> Label.t * O.type_expression Binder.t =
         fun i proj_pattern ->
          let l = Label.of_int i in
          let field_t = extract_record_type p l t in
          let b =
            match proj_pattern.wrap_content with
            | P_var x -> Binder.map (Fn.const field_t) x
            | _ ->
              Binder.make
                (Value_var.fresh ~loc:proj_pattern.location ~name:"tuple_proj" ())
                field_t
          in
          l, b
        in
        List.mapi ~f:aux ps
      | P_record lps, t ->
        let aux : Label.t * _ I.Pattern.t -> Label.t * O.type_expression Binder.t =
         fun (l, proj_pattern) ->
          let field_t = extract_record_type p l t in
          let b =
            match proj_pattern.wrap_content with
            | P_var x -> Binder.map (Fn.const field_t) x
            | _ ->
              Binder.make
                (Value_var.fresh ~loc:proj_pattern.location ~name:"tuple_proj" ())
                field_t
          in
          l, b
        in
        List.map ~f:aux (Record.to_list lps)
      | _ -> corner_case __LOC__
    in
    let aux : typed_pattern list * O.expression -> typed_pattern list * O.expression =
     fun (pl, body) ->
      match pl with
      | (prod, t) :: ptl ->
        let loc = prod.location in
        let var_filler = make_var_pattern (Value_var.fresh ~loc ~name:"_" ()) t, t in
        (match prod.wrap_content with
        | P_tuple ps ->
          let aux i p =
            let field_t = extract_record_type p (Label.of_int i) t in
            p, field_t
          in
          let tps = List.mapi ~f:aux ps in
          tps @ (var_filler :: ptl), body
        | P_record lps ->
          let aux (label, p) =
            let field_t = extract_record_type p label t in
            p, field_t
          in
          let tps = List.map ~f:aux (Record.to_list lps) in
          tps @ (var_filler :: ptl), body
        | P_var _ ->
          let filler =
            let p, t = product_shape in
            match p.wrap_content, t with
            | P_tuple ps, t ->
              let aux i p =
                let field_t = extract_record_type p (Label.of_int i) t in
                let v =
                  match p.wrap_content with
                  | P_var _ -> p
                  | _ ->
                    make_var_pattern
                      (Value_var.fresh ~loc:p.location ~name:"_" ())
                      field_t
                in
                v, field_t
              in
              List.mapi ~f:aux ps
            | P_record lps, t ->
              let aux (l, p) =
                let field_t = extract_record_type p l t in
                let v =
                  match p.wrap_content with
                  | P_var _ -> p
                  | _ ->
                    make_var_pattern
                      (Value_var.fresh ~loc:p.location ~name:"_" ())
                      field_t
                in
                v, field_t
              in
              List.map ~f:aux (Record.to_list lps)
            | _ -> corner_case __LOC__
          in
          filler @ pl, body
        | _ -> corner_case __LOC__)
      | [] -> corner_case __LOC__
    in
    let matchee_t = get_pattern_type eqs in
    let eqs' = List.map ~f:aux eqs in
    let fields = Record.of_list lb in
    let new_matchees = List.map ~f:(Fn.compose Binder.get_var snd) lb in
    let body = match_ (new_matchees @ ms) eqs' def in
    let cases = O.Match_record { fields; body; tv = snd product_shape } in
    let loc = body.location in
    let matchee = O.e_a_variable ~loc mhd matchee_t in
    O.make_e ~loc (O.E_matching { matchee; cases }) body.type_expression
  | [] -> corner_case __LOC__


let compile_matching
    matchee
    (eqs : (O.type_expression I.Pattern.t * O.type_expression * O.expression) list)
  =
  let eqs =
    List.map ~f:(fun (pattern, pattern_ty, body) -> [ pattern, pattern_ty ], body) eqs
  in
  let loc = Value_var.get_location matchee in
  let missing_case_default ty =
    let fs =
      O.make_e
        ~loc
        (O.E_literal (Literal_value.Literal_string Backend.Michelson.fw_partial_match))
        (O.t_string ~loc ())
    in
    let lamb =
      O.e_raw_code
        ~loc
        { language = "michelson"
        ; code =
            O.e_literal_string
              ~loc
              (Ligo_string.standard "{ FAILWITH }")
              (O.t_string ~loc ())
        }
        (O.t_arrow ~loc (O.t_string ~loc ()) ty ())
    in
    let args = fs in
    O.E_application { lamb; args }
  in
  match_ [ matchee ] eqs missing_case_default
