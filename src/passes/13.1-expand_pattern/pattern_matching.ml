(**

This implements the pattern_matching compiler of `Peyton-Jones, S.L., The Implementation of Functional Programming Languages`, chapter 5.
By reduction, this algorithm transforms pattern matching expression into (nested) cases expressions.
`Sugar` match expression being 'pattern matching' expression and `Core`/`Typed` being 'case expressions'.

"Product patterns" (e.g. tuple & record) are considered variables, an extra rule (product_rule) handles them

List patterns are treated as the variant type `NIL | Cons of (hd , tl)` would be.
Option patterns are treated as the variant type `Some a | None` would be

**)

module I = Ast_aggregated
module O = Ast_pattern_expanded
module Ligo_string = Simple_utils.Ligo_string

open Ligo_prim

type matchees = O.type_expression Binder.t list
type pattern = O.type_expression I.Pattern.t
type typed_pattern = pattern * O.type_expression
type equations = (typed_pattern list * O.expression) list
type rest = O.expression_content

(* module PP_DEBUG = struct
  let pp_typed_pattern ppf ((p,t) : typed_pattern) = Format.fprintf ppf "(%a : %a)" (I.Pattern.pp O.PP.type_expression) p O.PP.type_expression t
  let pp_pattern_list ppf (plist : typed_pattern list) = Format.fprintf ppf "[%a]" Simple_utils.PP_helpers.(list_sep pp_typed_pattern (tag "; ")) plist
  let pp_eq ppf ((plist,expr):(typed_pattern list * O.expression)) = Format.fprintf ppf "@[%a -> %a]" pp_pattern_list plist O.PP.expression expr
  let pp_eqs ppf (eqs:equations) = Format.fprintf ppf "@[<hv>%a@]" Simple_utils.PP_helpers.(list_sep pp_eq (tag "; ")) eqs
  let pp_partition ppf (part: equations list) = Format.fprintf ppf "@[<hv><@.%a@.>@]" Simple_utils.PP_helpers.(list_sep pp_eqs (tag "@.")) part
end *)

let is_var : _ I.Pattern.t -> bool = fun p ->
  match p.wrap_content with
  | P_var _ -> true
  | P_tuple _ -> true
  | P_record _ -> true
  | P_unit -> true
  | _ -> false
let is_product' : _ I.Pattern.t -> bool = fun p ->
  match p.wrap_content with
  | P_tuple _ -> true
  | P_record _ -> true
  | _ -> false

let is_product : equations -> typed_pattern option = fun eqs ->
  List.find_map
    ~f:(fun (pl,_) ->
      match pl with
      | (p,t)::_ -> if is_product' p then Some(p,t) else None
      | [] -> None
    )
    eqs

let corner_case loc = failwith ("broken invariant at "^loc)

let get_pattern_type (eqs : equations) =
  match eqs with
    (((_,t)::_),_)::_ -> t
  | (([],_)::_) -> corner_case __LOC__ (* typed_pattern list empty *)
  | [] -> corner_case __LOC__ (* equations empty *)

let get_body_type (eqs : equations) =
  match eqs with
    (_,e)::_ -> e.type_expression
  | [] -> corner_case __LOC__ (* equations empty *)

let extract_variant_type : pattern -> Label.t -> O.type_expression -> O.type_expression =
  fun _ label t ->
  match t.type_content with
  | T_sum rows -> (
    match Record.LMap.find_opt label rows.fields with
    | Some t -> t.associated_type
    | None -> corner_case __LOC__
  )
  | T_constant { injection = Literal_types.List ; parameters = [proj_t] ; language=_} -> (
    match label with
    | Label "Cons" -> O.make_t_ez_record [("0",proj_t);("1",t)]
    | Label "Nil" -> (O.t_unit ())
    | Label _ -> corner_case __LOC__
  )
  | _ -> corner_case __LOC__

let extract_record_type : pattern -> Label.t -> O.type_expression -> O.type_expression =
  fun _ label t ->
  match t.type_content with
  | T_record rows -> (
    match Record.LMap.find_opt label rows.fields with
    | Some t -> t.associated_type
    | None -> corner_case __LOC__
  )
  | _ -> corner_case __LOC__

(**
  `substitute_var_in_body to_subst new_var body` replaces variables equal to `to_subst` with variable `new_var` in expression `body`.
  note that `new_var` here is never a user variable (always previously generated by the compiler)
**)
let rec substitute_var_in_body : Value_var.t -> Value_var.t -> O.expression -> O.expression =
  fun to_subst new_var body ->
    let aux : unit -> O.expression -> bool * unit * O.expression =
      fun () exp ->
        let ret continue exp = (continue,(),exp) in
        match exp.expression_content with
        | O.E_variable var when Value_var.equal var to_subst ->
          ret true { exp with expression_content = E_variable new_var }
        | O.E_let_in letin
          when Binder.apply (Value_var.equal to_subst) letin.let_binder ->
          let rhs = substitute_var_in_body to_subst new_var letin.rhs in
          let letin = { letin with rhs } in
          ret false { exp with expression_content = E_let_in letin }
        | O.E_assign assign when Binder.apply (Value_var.equal to_subst) assign.binder ->
          let expression = substitute_var_in_body to_subst new_var assign.expression in
          let assign = { assign with expression } in
          ret false { exp with expression_content = E_assign assign}
        | O.E_lambda lamb when Value_var.equal to_subst (Param.get_var lamb.binder) -> ret false exp
        | O.E_recursive r when Value_var.equal r.fun_name to_subst -> ret false exp
        | O.E_matching m -> (
          let matchee = substitute_var_in_body to_subst new_var m.matchee in
          let cases = match m.cases with
          | Match_record {fields;body;tv} ->
            if Record.LMap.exists (fun _ ((b : O.type_expression Binder.t)) -> Binder.apply (Value_var.equal to_subst) b) fields
            then m.cases
            else
              let body = substitute_var_in_body to_subst new_var body in
              Match_record {fields;body;tv}
          | Match_variant {cases;tv} ->
            let cases = List.fold_right cases ~init:[] ~f:(fun case cases ->
              let bvar = case.pattern in
              if Value_var.equal bvar to_subst then case::cases
              else
                let body = substitute_var_in_body to_subst new_var case.body in
                {case with body}::cases
            )in
            Match_variant {cases;tv}
          in
          ret false { exp with expression_content = O.E_matching {matchee ; cases}}
        )
        | (E_literal _ | E_constant _ | E_variable _ | E_application _ | E_lambda _ | E_assign _ | E_let_mut_in _ | E_while _ | E_for _ | E_for_each _ | E_deref _
           | E_type_abstraction _ | E_recursive _ | E_let_in _ |
           E_raw_code _ | E_constructor _ | E_record _ | E_accessor _ |
           E_update _ | E_type_inst _ ) -> ret true exp
    in
    let ((), res) = O.Helpers.fold_map_expression (aux) () body in
    res

let make_var_pattern : Value_var.t -> O.type_expression -> pattern =
  fun var t -> Location.wrap @@ I.Pattern.P_var (Binder.make var t)

let rec partition : ('a -> bool) -> 'a list -> 'a list list =
  fun f lst ->
    let add_inner x ll =
      match ll with
      | hdl::tll -> (x::hdl)::tll
      | _ -> assert false
    in
    match lst with
    | [] -> []
    | [x] -> [[x]]
    | x::x'::tl ->
      if Bool.(=) (f x) (f x') then add_inner x (partition f (x'::tl))
      else [x] :: (partition f (x'::tl))

(**
  groups together equations that begin with the same constructor
**)
let group_equations : equations -> equations Record.t =
  fun eqs ->
    let aux : typed_pattern list * O.expression -> equations Record.t -> equations Record.t =
      fun (pl , body) m ->
        let (phd,t) = List.hd_exn pl in
        let ptl = List.tl_exn pl in
        let upd : O.type_expression -> pattern -> equations option -> equations option =
          fun proj_t pattern kopt ->
            match kopt with
            | Some eqs ->
              let p = (pattern,proj_t) in
              Some (( p::ptl , body)::eqs)
            | None ->
              let p = (pattern,proj_t) in
              Some [ (p::ptl          , body) ]
        in
        match phd.wrap_content with
        | P_variant (label,p_opt) ->
          let proj_t = extract_variant_type phd label t in
          Record.LMap.update label (upd proj_t p_opt) m
        | P_list (List []) ->
          let label = Label.of_string "Nil" in
          let proj_t = extract_variant_type phd label t in
          Record.LMap.update label (upd proj_t (Location.wrap I.Pattern.P_unit)) m
        | P_list (Cons (p_hd,p_tl)) ->
          let label = Label.of_string "Cons" in
          let pattern = Location.wrap ~loc:(phd.location) @@ I.Pattern.P_tuple [p_hd;p_tl] in
          let proj_t = extract_variant_type phd label t in
          Record.LMap.update label (upd proj_t pattern) m
        | _ -> corner_case __LOC__
    in
    List.fold_right ~f:aux ~init:Record.LMap.empty eqs

let rec match_ : err_loc:Location.t -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc ms eqs def ->
  match ms , eqs with
  | [] , [([],body)] -> body
  | [] , eqs when List.for_all ~f:(fun (ps,_) -> List.length ps = 0) eqs ->
    corner_case __LOC__
  | _ ->
    let leq = partition (fun (pl,_) -> is_var (fst @@ List.hd_exn pl)) eqs in
    let aux = fun (part_eq:equations) ((def,_,_):O.expression_content * O.type_expression option * Location.t) ->
      let r = consvar ~err_loc ms part_eq def in
      (r.expression_content , Some r.type_expression, r.location)
    in
    let (r,t,location) = List.fold_right ~f:aux ~init:(def,None,Location.generated) leq in
    O.make_e ~location r (Option.value_exn ~here:[%here] t)

and consvar : err_loc:Location.t -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc ms eqs def ->
  let p1s = List.map ~f:(fun el -> fst @@ List.hd_exn @@ fst el) eqs in
    if List.for_all ~f:is_var p1s then
      let product_opt = is_product eqs in
      var_rule ~err_loc product_opt ms eqs def
    else
      ctor_rule ~err_loc ms eqs def

and var_rule : err_loc:Location.t -> typed_pattern option -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc product_opt ms eqs def ->
  match ms with
  | mhd::mtl -> (
    match product_opt with
    | Some shape ->
      product_rule ~err_loc shape ms eqs def
    | None ->
       let aux : typed_pattern list * O.expression ->
                 (typed_pattern list * O.expression) =
        fun (pl, body) ->
        match pl with
        | (phd,_)::ptl -> (
          match phd.wrap_content with
          | P_var b ->
            let body' = substitute_var_in_body (Binder.get_var b) (Binder.get_var mhd) body in
            (ptl , body')
          | P_unit -> (ptl , body)
          |  _ -> corner_case __LOC__
        )
        | [] -> corner_case __LOC__
      in
      let eqs' = List.map ~f:aux eqs in
      match_ ~err_loc mtl eqs' def
  )
  | [] -> corner_case __LOC__

and ctor_rule : err_loc:Location.t -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc ms eqs def ->
  match ms with
  | mhd::mtl ->
    let matchee_t = get_pattern_type eqs in
    let body_t = get_body_type eqs in
    let matchee = O.e_a_variable (Binder.get_var mhd) matchee_t in
    let eq_map = group_equations eqs in
    let aux_p :  Label.t * equations -> _ O.matching_content_case  =
      fun (constructor,eq) ->
        let proj =
          match eq with
          | [(tp,_)] -> (
            let (pattern,t) = List.hd_exn tp in
            match pattern.wrap_content with
            | P_var x -> x
            | P_unit -> Binder.make (Value_var.fresh ~name:"unit_proj" ()) t
            | _ -> Binder.make (Value_var.fresh ~name:"ctor_proj" ()) t
          )
          | _ ->
            let (tp,_) = List.hd_exn eq in
            let (_,t) = List.hd_exn tp in
            Binder.make (Value_var.fresh ~name:"ctor_proj" ()) t
        in
        let new_ms = proj::mtl in
        let nested = match_ ~err_loc new_ms eq def in
        O.{ constructor ; pattern = Binder.get_var proj ; body = nested }
    in
    let aux_m : O.type_expression * Label.t * O.type_expression -> _ O.matching_content_case =
      fun (tb,constructor,t) ->
        ignore tb ;
        (* TODO : remove this tb thing *)
        (* TODO : remove this tb thing *)
        (* TODO : remove this tb thing *)
        (* TODO : remove this tb thing *)
        (* TODO : remove this tb thing *)
        (* TODO : remove this tb thing *)
        let proj = Value_var.fresh ~name:"ctor_proj" () in
        let body = O.make_e def t in
        { constructor ; pattern = proj ; body }
    in
    let grouped_eqs =
      match O.get_t_sum matchee_t with
      | Some _ when Option.is_some (O.get_t_option matchee_t) ->
        List.map ~f:(fun label -> (matchee_t, label, Record.LMap.find_opt label eq_map)) [Label.of_string "Some"; Label.of_string "None"]
      | Some rows ->
        let eq_opt_map = Record.LMap.mapi (fun label _ -> Record.LMap.find_opt label eq_map) rows.fields in
        List.map (Record.LMap.to_kv_list @@ eq_opt_map) ~f:(fun (a,b) -> matchee_t, a, b)
      | None -> (
        (* REMITODO: parametric types in env ? *)
        match O.get_t_list matchee_t with
        | Some _ -> List.map ~f:(fun label -> (matchee_t, label, Record.LMap.find_opt label eq_map)) [Label.of_string "Cons"; Label.of_string "Nil"]
        | None -> corner_case __LOC__ (* should be caught when typing the matchee *)
      )
    in
    let present = List.filter_map ~f:(fun (_,c,eq_opt) -> match eq_opt with Some eq -> Some (c,eq) | None -> None) grouped_eqs in
    let present_cases = List.map present ~f:aux_p in
    let missing = List.filter_map ~f:(fun (tb,c,eq_opt) -> match eq_opt with Some _ -> None | None -> Some (tb,c,body_t)) grouped_eqs in
    let missing_cases = List.map ~f:aux_m missing in
    let cases = O.Match_variant { cases = missing_cases @ present_cases ; tv = matchee_t } in
    O.make_e (O.E_matching { matchee ; cases }) body_t
  | [] -> corner_case __LOC__

and product_rule : err_loc:Location.t -> typed_pattern -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc product_shape ms eqs def ->
  match ms with
  | mhd::_ -> (
    let lb :(_ * _ Binder.t) list =
      let (p,t) = product_shape in
      match (p.wrap_content,t) with
      | P_tuple ps , t ->
        let aux : int -> _ I.Pattern.t -> (Label.t * (O.type_expression Binder.t)) =
          fun i proj_pattern ->
            let l = (Label.of_int i) in
            let field_t = extract_record_type p l t in
            let b = match proj_pattern.wrap_content with
              | P_var x -> Binder.map (Fn.const field_t) x
              | _ -> Binder.make (Value_var.fresh ~loc:proj_pattern.location ~name:"tuple_proj" ()) field_t
            in
            (l, b)
        in
        List.mapi ~f:aux ps
      | P_record lps , t ->
        let aux : (Label.t * _ I.Pattern.t)  -> (Label.t * (O.type_expression Binder.t)) =
          fun (l,proj_pattern) ->
            let field_t = extract_record_type p l t in
            let b = match proj_pattern.wrap_content with
              | P_var x -> Binder.map (Fn.const field_t) x
              | _ -> Binder.make (Value_var.fresh ~loc:proj_pattern.location ~name:"tuple_proj" ()) field_t
            in
            (l, b)
        in
        List.map ~f:aux (Record.LMap.to_kv_list lps)
      | _ -> corner_case __LOC__
    in
    let aux : typed_pattern list * O.expression ->
              (typed_pattern list * O.expression) =
      fun (pl, body) ->
      match pl with
      | (prod,t)::ptl -> (
        let var_filler = (make_var_pattern (Value_var.fresh ~name:"_" ()) t, t) in
        match prod.wrap_content with
        | P_tuple ps ->
          let aux i p =
            let field_t = extract_record_type p (Label.of_int i) t in
            (p,field_t)
          in
          let tps = List.mapi ~f:aux ps in
          (tps @ var_filler::ptl , body)
        | P_record lps ->
          let aux (label, p) =
            let field_t = extract_record_type p label t in
            (p,field_t)
          in
          let tps = List.map ~f:aux (Record.LMap.to_kv_list lps) in
          (tps @ var_filler::ptl , body)
        | P_var _ ->
          let filler =
            let (p,t) = product_shape in
            match (p.wrap_content,t) with
            | P_tuple ps , t ->
              let aux i p =
                let field_t = extract_record_type p (Label.of_int i) t in
                let v = match p.wrap_content with
                  | P_var _ -> p
                  | _ -> make_var_pattern (Value_var.fresh ~loc:p.location ~name:"_" ()) field_t
                in
                (v,field_t)
              in
              List.mapi ~f:aux ps
            | P_record lps , t ->
              let aux (l, p) =
                let field_t = extract_record_type p l t in
                let v = match p.wrap_content with
                  | P_var _ -> p
                  | _ -> make_var_pattern (Value_var.fresh ~loc:p.location ~name:"_" ()) field_t
                in
                (v,field_t)
              in
              List.map ~f:aux (Record.LMap.to_kv_list lps)
            | _ -> corner_case __LOC__
          in
          (filler @ pl , body)
        | _ -> corner_case __LOC__
      )
      | [] -> corner_case __LOC__
    in
    let matchee_t = get_pattern_type eqs in
    let eqs' = List.map ~f:aux eqs in
    let fields = Record.LMap.of_list lb in
    let new_matchees = List.map ~f:(snd) lb in
    let body = match_ ~err_loc (new_matchees @ ms) eqs' def in
    let cases = O.Match_record { fields; body ; tv = snd product_shape } in
    let matchee = O.e_a_variable (Binder.get_var mhd) matchee_t in
    O.make_e (O.E_matching { matchee ; cases }) body.type_expression
  )
  | [] -> corner_case __LOC__

let compile_matching ~err_loc matchee (eqs: (O.type_expression I.Pattern.t * O.type_expression * O.expression) list) =
  let eqs = List.map ~f:(fun (pattern,pattern_ty,body) -> ( [(pattern,pattern_ty)] , body )) eqs in
  let missing_case_default =
    let fs = O.make_e (O.E_literal (Literal_value.Literal_string Backend.Michelson.fw_partial_match)) (O.t_string ()) in
    let t_fail =
      let a = Type_var.of_input_var "a" in
      let b = Type_var.of_input_var "b" in
      let ty_binder = a in
      let kind = Ligo_prim.Kind.Type in
      let type_ = 
        let ty_binder = b in
        let kind = Ligo_prim.Kind.Type in
        let type_ = O.t_arrow { type1 = (O.t_variable a ()) ; type2 = (O.t_variable b ()) } () in
        O.t_for_all { ty_binder ; kind ; type_ } () in
      O.t_for_all { ty_binder ; kind ; type_ } ()
    in
    let lamb = (O.e_variable (Value_var.of_input_var "failwith") t_fail) in
    let args = fs in
    O.E_application {lamb ; args }
  in
  match_ ~err_loc [matchee] eqs missing_case_default
