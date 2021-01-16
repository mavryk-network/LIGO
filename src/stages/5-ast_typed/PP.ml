[@@@coverage exclude_file]
open Ast
open Format
open PP_helpers
include Stage_common.PP

let lmap_sep value sep ppf m =
  let lst = List.sort (fun (Label a,_) (Label b,_) -> String.compare a b) m in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, v) = fprintf ppf "%a" value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep_t value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, {associated_type;_}) = fprintf ppf "%a" value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

(* Prints records which only contain the consecutive fields
   0..(cardinal-1) as tuples *)
let tuple_or_record_sep value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m
let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep_t value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep_t value (tag sep_record)) m
let list_sep_d_short x = list_sep x (tag " , ")
let list_sep_d x = list_sep x (tag " ,@ ")
let lmap_sep_d_short x = lmap_sep x (tag " , ")
let lmap_sep_d x = lmap_sep x (tag " ,@ ")
let tuple_or_record_sep_expr value = tuple_or_record_sep value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " ,@ "
let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " *@ "

let type_variable ppf (t : type_variable) : unit = fprintf ppf "%a" Var.pp t

open Format

let list_sep_d_par f ppf lst =
  match lst with
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_content -> unit =
  fun ppf tc ->
  match tc with
  | T_variable        tv -> type_variable                 ppf tv
  | T_constant        tc -> type_injection ppf tc
  | T_sum              m -> fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d type_expression) (LMap.to_kv_list_rev @@ LMap.map (fun {associated_type;_} -> associated_type) m.content)
  | T_record           m -> fprintf ppf "%a" record m
  | T_arrow            a -> arrow         type_expression ppf a
  | T_module_accessor ma -> module_access type_expression ppf ma

and type_injection ppf {language;injection;parameters} =
  ignore language;
  fprintf ppf "%s%a" (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters

and record ppf {content; layout=_} =
  fprintf ppf "%a"
    (tuple_or_record_sep_type type_expression) content

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te.type_content

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev.wrap_content


let rec expression ppf (e : expression) =
  fprintf ppf "%a"
    expression_content e.expression_content

and expression_content ppf (ec: expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "(%a)@(%a)" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "%a(%a)" label c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "%a(%a)" constant' c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "%a" (tuple_or_record_sep_expr expression) m
  | E_record_accessor ra ->
      fprintf ppf "%a.%a" expression ra.record label ra.path
  | E_record_update {record; path; update} ->
      fprintf ppf "{ %a with { %a = %a } }" expression record label path expression update
  | E_lambda {binder; result} ->
      fprintf ppf "lambda (%a) return %a" expression_variable binder
        expression result
  | E_matching {matchee; cases;} ->
      fprintf ppf "match %a with %a" expression matchee (matching expression) cases
  | E_let_in {let_binder; rhs; let_result; inline} ->
      fprintf ppf "let %a = %a%a in %a" expression_variable let_binder expression
        rhs option_inline inline expression let_result
  | E_type_in   ti -> type_in expression type_expression ppf ti
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_recursive { fun_name;fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )"
        expression_variable fun_name
        type_expression fun_type
        expression_content (E_lambda lambda)
  | E_module_accessor ma -> module_access expression ppf ma


and option_inline ppf inline =
  if inline then
    fprintf ppf "[@inline]"
  else
    fprintf ppf ""

and matching_variant_case : (_ -> expression -> unit) -> _ -> matching_content_case -> unit =
  fun f ppf {constructor=c; pattern; body} ->
  fprintf ppf "| %a %a -> %a" label c expression_variable pattern f body

and matching : (formatter -> expression -> unit) -> _ -> matching_expr -> unit = fun f ppf m -> match m with
  | Match_variant {cases ; tv=_} ->
      fprintf ppf "%a" (list_sep (matching_variant_case f) (tag "@.")) cases
  | Match_list {match_nil ; match_cons = {hd; tl; body; tv=_}} ->
      fprintf ppf "| Nil -> %a @.| %a :: %a -> %a" f match_nil expression_variable hd expression_variable tl f body
  | Match_option {match_none ; match_some = {opt; body; tv=_}} ->
      fprintf ppf "| None -> %a @.| Some %a -> %a" f match_none expression_variable opt f body

let declaration ppf (d : declaration) =
  match d with
  | Declaration_constant {binder; expr; inline} ->
      fprintf ppf "const %a = %a%a" expression_variable binder expression expr option_inline inline
  | Declaration_type {type_binder; type_expr} ->
      fprintf ppf "type %a = %a" type_variable type_binder type_expression type_expr

let program_fully_typed ppf (Program_Fully_Typed p : program_fully_typed) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)

let program_with_unification_vars ppf (Program_With_Unification_Vars p : program_with_unification_vars) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)

let typeVariableMap = fun f ppf tvmap   ->
      let lst = List.sort (fun (a, _) (b, _) -> Var.compare a b) (RedBlackTrees.PolyMap.bindings tvmap) in
      let aux ppf (k, v) =
        fprintf ppf "(Var %a, %a)" Var.pp k f v in
      fprintf ppf "typeVariableMap [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let typeVariableSet = fun ppf s   ->
      let lst = List.sort (fun (a) (b) -> Var.compare a b) (RedBlackTrees.PolySet.elements s) in
      let aux ppf (k) =
        fprintf ppf "(Var %a)" Var.pp k in
      fprintf ppf "typeVariableSet [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst
let constraint_identifier_set = fun ppf s   ->
      let lst = List.sort (fun (ConstraintIdentifier a) (ConstraintIdentifier b) -> Int64.compare a b) (RedBlackTrees.PolySet.elements s) in
      let aux ppf (ConstraintIdentifier k) =
        fprintf ppf "(ConstraintIdentifier %Li)" k in
      fprintf ppf "constraint_identifier_set [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let identifierMap = fun f ppf idmap ->
      let lst = List.sort (fun (ConstraintIdentifier a, _) (ConstraintIdentifier b, _) -> Int64.compare a b) (RedBlackTrees.PolyMap.bindings idmap) in
      let aux ppf (ConstraintIdentifier k, v) =
        fprintf ppf "(ConstraintIdentifier %Li, %a)" k f v in
      fprintf ppf "typeVariableMap [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let biMap = fun fk fv ppf idmap ->
      let lst = RedBlackTrees.PolyBiMap.bindings idmap in
      let aux ppf (k, v) =
        fprintf ppf "(%a, %a)" fk k fv v in
      fprintf ppf "typeVariableMap [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst
let poly_unionfind = (fun f ppf p   ->
  let lst = (UnionFind.Poly2.partitions p) in
  let aux1 ppf l = fprintf ppf "[@[<hv 2> (*%a*) %a @]@ ]"
                  f (UnionFind.Poly2.repr (List.hd l) p)
                  (list_sep (f) (fun ppf () -> fprintf ppf " ;@ ")) l in
  let aux2 = list_sep aux1 (fun ppf () -> fprintf ppf " ;@ ") in
  fprintf ppf "UnionFind [@[<hv 2>@ %a @]@ ]" aux2 lst)

let constant_tag ppf c_tag = match c_tag with
  | C_arrow     -> fprintf ppf "C_arrow"
  | C_option    -> fprintf ppf "C_option"
  | C_map       -> fprintf ppf "C_map"
  | C_big_map   -> fprintf ppf "C_big_map"
  | C_list      -> fprintf ppf "C_list"
  | C_set       -> fprintf ppf "C_set"
  | C_unit      -> fprintf ppf "C_unit"
  | C_string    -> fprintf ppf "C_string"
  | C_nat       -> fprintf ppf "C_nat"
  | C_mutez     -> fprintf ppf "C_mutez"
  | C_timestamp -> fprintf ppf "C_timestamp"
  | C_int       -> fprintf ppf "C_int"
  | C_address   -> fprintf ppf "C_address"
  | C_bytes     -> fprintf ppf "C_bytes"
  | C_key_hash  -> fprintf ppf "C_key_hash"
  | C_key       -> fprintf ppf "C_key"
  | C_signature -> fprintf ppf "C_signature"
  | C_operation -> fprintf ppf "C_operation"
  | C_contract  -> fprintf ppf "C_contract"
  | C_chain_id  -> fprintf ppf "C_chain_id"

let row_tag ppf = function
    C_record -> fprintf ppf "C_record"
  | C_variant -> fprintf ppf "C_variant"

let rec c_equation ppf {aval; bval} =
  fprintf ppf "{@[<hv 2>@ aval : %a;@ bval : %a;@]@ }"
    type_value aval
    type_value bval

and c_equation_short ppf {aval; bval} =
  fprintf ppf "%a = %a"
    type_value_short aval
    type_value_short bval

and c_typeclass ppf {tc_args; typeclass=tc;original_id} =
  fprintf ppf "{@[<hv 2>@ tc_args : %a;@ typeclass : %a;@ original_id : %s;@]@ }"
    (list_sep_d type_value) tc_args
    typeclass tc
    (match     original_id with Some (ConstraintIdentifier
        x) -> Int64.to_string x | None ->"null")

and c_typeclass_short ppf {tc_args; typeclass=tc;original_id=_} =
  fprintf ppf "[%a] in [%a]"
    (list_sep_d type_value) tc_args
    typeclass tc

and c_access_label ppf {c_access_label_tval; accessor; c_access_label_tvar} =
  fprintf ppf "{@[<hv 2>@
              c_access_label_tval : %a;@
              accessor : %a;@
              c_access_label_tvar : %a;@
              @]}"
    type_value c_access_label_tval
    label accessor
    type_variable c_access_label_tvar

and c_access_label_short ppf {c_access_label_tval; accessor; c_access_label_tvar} =
  fprintf ppf "%a.%a = %a"
    type_value c_access_label_tval
    label accessor
    type_variable c_access_label_tvar


and type_constraint_ ppf = function
  C_equation     eq -> fprintf ppf "C_equation (%a)" c_equation eq
| C_typeclass    tc -> fprintf ppf "C_typeclass (%a)" c_typeclass tc
| C_access_label al -> fprintf ppf "C_access_label (%a)" c_access_label al

and type_constraint_short_ ppf = function
  C_equation     eq -> fprintf ppf "C_eq (%a)" c_equation_short eq
| C_typeclass    tc -> fprintf ppf "C_tc (%a)" c_typeclass_short tc
| C_access_label al -> fprintf ppf "C_al (%a)" c_access_label_short al

and type_constraint_short ppf {reason=_; c} = fprintf ppf "%a" type_constraint_short_ c
and type_constraint ppf {reason; c} = fprintf ppf "{@[<hv 2>@ reason : %s;@ c : %a;@ @]}" reason type_constraint_ c
and p_constraints ppf const = fprintf ppf "%a" (list_sep_d_short type_constraint) const

and p_constraints_short ppf const = fprintf ppf "%a" (list_sep_d_short type_constraint_short) const

and p_forall ppf {binder;constraints;body} =
  fprintf ppf "{@[<hv 2>@ binder : %a;@ constraints : %a;@ body : %a;@]@ }"
    type_variable binder
    p_constraints constraints
    type_value body

and p_forall_short ppf {binder;constraints;body} =
  fprintf ppf "∀ %a, %a => %a"
    type_variable binder
    p_constraints_short constraints
    type_value_short body


and p_constant ppf {p_ctor_tag; p_ctor_args} =
  fprintf ppf "{@[<hv 2>@ p_ctor_tag : %a;@ p_ctor_args : (%a);@]@ }"
    constant_tag p_ctor_tag
    (list_sep_d type_value) p_ctor_args

and p_constant_short ppf {p_ctor_tag; p_ctor_args} =
  fprintf ppf "%a (%a)"
      constant_tag p_ctor_tag
      (list_sep_d_short type_value_short) p_ctor_args

and p_apply ppf {tf; targ} =
  fprintf ppf "{@[<hv 2>@ tf : %a;@ targ : %a;@]@ }"
    type_value tf
    type_value targ

and p_row ppf {p_row_tag; p_row_args} =
  fprintf ppf "{@[<hv 2>@ p_row_tag : %a;@ p_row_args : %a;@]@ }"
    row_tag p_row_tag
    (lmap_sep_d type_value) @@ LMap.to_kv_list p_row_args
and p_row_short ppf {p_row_tag; p_row_args} =
    fprintf ppf "%a { %a }"
    row_tag p_row_tag
    (lmap_sep_d_short type_value_short) @@ LMap.to_kv_list p_row_args

and type_value_ ppf = function
  P_forall   fa -> fprintf ppf "%a" p_forall fa
| P_variable tv -> fprintf ppf "%a" type_variable tv
| P_constant c  -> fprintf ppf "%a" p_constant c
| P_apply   app -> fprintf ppf "%a" p_apply app
| P_row       r -> fprintf ppf "%a" p_row r

and type_value_short_ ppf = function
| P_constant c  -> fprintf ppf "%a" p_constant_short c
| P_variable tv -> fprintf ppf "%a" type_variable tv
| P_forall   fa -> fprintf ppf "%a" p_forall_short fa
| P_apply   _app -> fprintf ppf "apply"
| P_row       r -> fprintf ppf "%a" p_row_short r

and type_value ppf t =
  fprintf ppf "{@[<hv 2> @ t : %a;@ loc : %a;@]@ }"
    type_value_ t.wrap_content
    Location.pp t.location

and type_value_short ppf t =
  fprintf ppf "%a" type_value_short_ t.wrap_content

and typeclass ppf tc = fprintf ppf "%a" (list_sep_d (list_sep_d type_value)) tc
let c_constructor_simpl ppf ({reason_constr_simpl;tv;c_tag;tv_list} : c_constructor_simpl) =
  fprintf ppf "{@[<hv 2> @ reason_constr_simpl : %s;@ tv : %a;@ c_tag : %a;@ tv_list : %a;@]@ }"
    reason_constr_simpl
    type_variable tv
    constant_tag c_tag
    (list_sep_d_short type_variable) tv_list

let c_constructor_simpl_short ppf ({reason_constr_simpl=_;tv;c_tag;tv_list} : c_constructor_simpl) =
  fprintf ppf "%a = %a (%a)"
    type_variable tv
    constant_tag c_tag
    (list_sep_d_short type_variable) tv_list

let c_alias ppf ({reason_alias_simpl;a;b}: c_alias) =
  fprintf ppf "{@[<hv 2> @ reason_alias_simpl : %s;@ a : %a;@ b : %a;@]@ }"
    reason_alias_simpl
    type_variable a
    type_variable b

let c_poly_simpl ppf ({reason_poly_simpl; tv; forall}) =
  fprintf ppf "{@[<hv 2> @ reason_poly_simpl : %s;@ tv : %a;@ forall : %a;@]@ }"
    reason_poly_simpl
    type_variable tv
    p_forall forall

let c_typeclass_simpl ppf ({id_typeclass_simpl = ConstraintIdentifier ci; reason_typeclass_simpl; original_id; tc; args}) =
  fprintf ppf "{@[<hv 2 >@ id_typeclass_simpl : %Li;@ original_id : %s;@ reason_typeclass_simpl : %s;@ tc : %a;@ args : %a;@]@ }"
    ci
    (match original_id with Some (ConstraintIdentifier x) -> Format.asprintf "%Li" x | None -> "null" )
    reason_typeclass_simpl
    typeclass tc
    (list_sep_d type_variable) args
let constraint_identifier ppf (ConstraintIdentifier ci) =
  fprintf ppf "ConstraintIdentifier %Li" ci

let constraint_identifierMap = fun f ppf tvmap   ->
      let lst = RedBlackTrees.PolyMap.bindings tvmap in
      let aux ppf (k, v) =
        fprintf ppf "(%a, %a)" constraint_identifier k f v in
      fprintf ppf "constraint_identifierMap [@ @[<hv 2> %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let c_row_simpl ppf ({reason_row_simpl; tv; r_tag; tv_map}) =
  fprintf ppf "{@[<hv 2> @ reason_row_simpl : %s;@ tv : %a;@ r_tag : %a;@ tv_map : %a;@]@ }"
    reason_row_simpl
    type_variable tv
    row_tag r_tag
    (lmap_sep_d type_variable) @@ LMap.to_kv_list tv_map

let type_constraint_simpl ppf (tc: type_constraint_simpl) = match tc with
  | SC_Constructor c -> fprintf ppf "SC_Constructor (%a)" c_constructor_simpl c
  | SC_Alias       a -> fprintf ppf "SC_Alias (%a)" c_alias a
  | SC_Poly        p -> fprintf ppf "SC_Poly (%a)" c_poly_simpl p
  | SC_Typeclass   t -> fprintf ppf "SC_Typeclass (%a)" c_typeclass_simpl t
  | SC_Row         r -> fprintf ppf "SC_Row (%a)" c_row_simpl r

let constraint_identifier ppf (ConstraintIdentifier ci) =
  fprintf ppf "ConstraintIdentifier %Li" ci


(* let structured_dbs ppf ({refined_typeclasses;refined_typeclasses_back;typeclasses_constrained_by;by_constraint_identifier;all_constraints;aliases;assignments;grouped_by_variable;cycle_detection_toposort} : structured_dbs) =
 *   fprintf ppf "{@,@[<hv 2> refined_typeclasses : %a ;@ refined_typeclasses_back : %a ;@ typeclasses_constrained_by : %a ;@ by_constraint_identifier : %a ;@ all_constraints : %a ;@ aliases : %a ;@ assignments : %a;@ gouped_by_variable : %a;@ cycle_detection_toposort : %a @]@,}"
 *     (identifierMap refined_typeclass) refined_typeclasses
 *     (constraint_identifierMap constraint_identifier) refined_typeclasses_back
 *     (typeVariableMap constraint_identifier_set) typeclasses_constrained_by
 *     (identifierMap c_typeclass_simpl) by_constraint_identifier
 *     (list_sep_d type_constraint_simpl) all_constraints
 *     (poly_unionfind type_variable) aliases
 *     (typeVariableMap c_constructor_simpl) assignments
 *     (typeVariableMap constraints) grouped_by_variable
 *     (fun _ppf _ -> ()) cycle_detection_toposort *)

let constructor_or_row ppf (t : constructor_or_row ) =
  match t with
  | `Row r -> c_row_simpl ppf r
  | `Constructor c -> c_constructor_simpl_short ppf c

let output_break_ctor ppf ({a_k_var;a_k'_var'}) =
  fprintf ppf "{@[<hv 2> @ a_k_var : %a;@ a_k'_var' : %a;@]@ }"
    constructor_or_row a_k_var
    constructor_or_row a_k'_var'

let output_specialize1 ppf ({poly;a_k_var}) =
  fprintf ppf "{@[<hv 2> @ poly : %a ;@ a_k_var : %a;@]@ }"
    c_poly_simpl poly
    c_constructor_simpl a_k_var

let output_tc_fundep ppd (t : output_tc_fundep) =
  let lst = t.tc in
  let a = t.c in fprintf ppd "{tc:%a;a:%a}" c_typeclass_simpl lst constructor_or_row a

let deduce_and_clean_result ppf {deduced;cleaned} =
  fprintf ppf "{@[<hv 2>@
              deduced : %a;@
              cleaned : %a;@
              @]}"
    (list c_constructor_simpl) deduced
    c_typeclass_simpl cleaned

let axiom ppf = function |HandWaved s -> fprintf ppf "HandWaved %s" s

let proof_trace ppf = function
  Axiom a -> fprintf ppf "Axiom %a" axiom a

let update ppf {remove_constraints;add_constraints;proof_trace=x} =
  fprintf ppf "{@[<hv 2> @ remove_constraints : %a;@ add_constraints : %a;@ proof_trace : %a;@]@ }"
    (list type_constraint_simpl) remove_constraints
    (list type_constraint) add_constraints
    proof_trace x

let updates_list ppf = fprintf ppf "%a" (list (list update))

let environment_element_definition ppf = function
  | ED_binder -> fprintf ppf "Binder"
  | ED_declaration {expression=e;free_variables=fv} ->
    fprintf ppf "Declaration : {expression : %a ;@ free_variables : %a}" expression e (list expression_variable) fv
let rec environment_element ppf ({type_value;source_environment;definition} : environment_element) =
  fprintf ppf "{@[<hv 2> @ type_value : %a;@ source_environment : %a;@ definition : %a;@]@ }"
    type_expression type_value
    environment source_environment
    environment_element_definition definition


and environment_binding ppf ({expr_var;env_elt} : environment_binding) =
  fprintf ppf "{@[<hv 2> @ expr_var : %a;@ env_elt : %a;@]@ }"
    expression_variable expr_var
    environment_element env_elt

and type_environment_binding ppf ({type_variable=tv;type_} : type_environment_binding) =
  fprintf ppf "{@[<hv 2> @ type_variable : %a;@ type_ : %a;@]@ }"
    type_variable tv
    type_expression type_

and module_environment_binding ppf ({module_name;module_} : module_environment_binding) =
  fprintf ppf "{@[<hv 2> @ module_name : %s;@ module_ : %a;@]@ }"
    module_name
    environment module_

and environment ppf ({expression_environment;type_environment=_;module_environment=_} : environment) =
  fprintf ppf "{@[<hv 2> @ expression_environment : (%a);@]@ }"
    (list_sep_d environment_binding) expression_environment
