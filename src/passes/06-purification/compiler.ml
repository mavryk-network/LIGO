module Location = Simple_utils.Location
module Pair     = Simple_utils.Pair
module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Simple_utils.Trace
module VMap = Simple_utils.Map.Make(Ligo_prim.Value_var)
open Ligo_prim

(* TODO this pass has been gutted, which parts are still useful and
   where do they belong? *)

let rec compile_type_expression ~raise : I.type_expression -> O.type_expression =
  fun te ->
  (*
    At this point sum and record types becomes linear by their type (see previous pass in self_imperative)
    TODO: nano pass from non-linear to linear types (?)
  *)
  let self = compile_type_expression ~raise in
  let return tc = O.make_t ~loc:te.location tc in
  match te.type_content with
    | I.T_sum { attributes ; fields } ->
      let fields = Record.(map (Rows.map_row_element self) (of_list fields)) in
      return @@ O.T_sum { attributes ; fields }
    | I.T_record { attributes ; fields } ->
      let fields = Record.(map (Rows.map_row_element self) (of_list fields)) in
      return @@ O.T_record { attributes ; fields }
    | I.T_tuple tuple ->
      let tuple = List.map ~f:self tuple in
      return @@ O.T_tuple tuple
    | I.T_arrow arr ->
      let arr = Arrow.map self arr in
      return @@ T_arrow arr
    | I.T_variable type_variable -> return @@ T_variable type_variable
    | I.T_app {type_operator;arguments=[l;r]} when Type_var.equal Literal_types.v_michelson_or type_operator ->
      let (l, l_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let (r, r_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let (l,r) = Pair.map ~f:(compile_type_expression ~raise) (l,r) in
      let sum : (Label.t * _ Rows.row_element) list = [
        (Label "M_left" , {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (Label "M_right", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 1}); ]
      in
      return @@ O.T_sum { fields = O.LMap.of_list sum ; attributes = [] }
    | I.T_app {type_operator;arguments=[l;r]} when Type_var.equal Literal_types.v_michelson_pair type_operator ->
      let (l, l_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let (r, r_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let (l,r) = Pair.map ~f:(compile_type_expression ~raise) (l,r) in
      let sum : (Label.t * _ Rows.row_element) list = [
        (Label "0", {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (Label "1", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 1}); ]
      in
      return @@ O.T_record { fields = (O.LMap.of_list sum) ; attributes = [] }
    | I.T_app c ->
      let c = Type_app.map self c in
      return @@ T_app c
    | I.T_module_accessor ma -> return @@ T_module_accessor ma
    | I.T_annoted (ty, _) -> self ty
    | I.T_singleton t -> return @@ O.T_singleton t
    | I.T_abstraction x ->
      let type_ = self x.type_ in
      return @@ O.T_abstraction { x with type_ }
    | I.T_for_all x ->
      let type_ = self x.type_ in
      return @@ O.T_for_all { x with type_ }

let compile_type_expression_option ~raise te_opt =
  Option.map ~f:(compile_type_expression ~raise) te_opt

let rec compile_expression ~raise : I.expression -> O.expression =
  fun e ->
  let self = compile_expression ~raise in
  let self_type = compile_type_expression ~raise in
  let self_type_option = compile_type_expression_option ~raise in
  let return' expr = expr in
  let return expr = return' @@ O.make_e ~loc:e.location expr in
  match e.expression_content with
    | I.E_literal literal   -> return @@ O.E_literal literal
    | I.E_constant {cons_name;arguments} ->
      let arguments = List.map ~f:(compile_expression ~raise) arguments in
      return' @@ O.e_constant ~loc:e.location (Constant.const_name cons_name) arguments
    | I.E_variable name     -> return @@ O.E_variable name
    | I.E_application app ->
      let app = Application.map self app in
      return @@ O.E_application app
    | I.E_lambda lamb ->
      let lamb = Lambda.map self self_type_option lamb in
      return @@ O.E_lambda lamb
    | I.E_type_abstraction ta ->
      let ta = Type_abs.map self ta in
      return @@ O.E_type_abstraction ta
    | I.E_recursive recs ->
      let recs = Recursive.map self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;attributes;rhs;let_result} ->
      let let_binder = Binder.map self_type_option let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      return @@ O.E_let_in {let_binder;attributes; rhs; let_result}
    | I.E_type_in ti ->
      let ti = Type_in.map self self_type ti in
      return @@ O.E_type_in ti
    | I.E_mod_in {module_binder;rhs;let_result} ->
      let rhs = compile_module_expr ~raise rhs in
      let let_result = self let_result in
      return @@ O.E_mod_in {module_binder;rhs;let_result}
    | I.E_raw_code rc ->
      let rc = Raw_code.map self rc in
      return @@ O.E_raw_code rc
    | I.E_constructor const ->
      let const = Constructor.map self const in
      return @@ O.E_constructor const
    | I.E_matching {matchee;cases} ->
      let matchee = self matchee in
      let aux Match_expr.{pattern;body} =
        let pattern = Pattern.map (Option.map ~f:(compile_type_expression ~raise)) pattern in
        Match_expr.{pattern;body = self body} in
      let cases   = List.map ~f:aux cases in
      return @@ O.E_matching {matchee;cases}
    | I.E_record recd ->
      (* at this point record expression become linear wrt labels *)
      let recd = List.map ~f:(fun (l,e) -> l, self e) recd in
      return @@ O.E_record (O.LMap.of_list recd)
    | I.E_accessor acc ->
      let acc = I.Accessor.map self acc in
      return @@ O.E_accessor acc
    | I.E_update up ->
      let up = I.Update.map self up in
      return @@ O.E_update up
    | I.E_map map ->
      let map = List.map ~f:(
        Pair.map ~f:self
      ) map
      in
      return @@ O.E_map map
    | I.E_big_map big_map ->
      let big_map = List.map ~f:(
        Pair.map ~f:self
      ) big_map
      in
      return @@ O.E_big_map big_map
    | I.E_list lst ->
      let lst = List.map ~f:self lst in
      return @@ O.E_list lst
    | I.E_set set ->
      let set = List.map ~f:self set in
      return @@ O.E_set set
    | I.E_ascription ascr ->
      let ascr = Ascription.map self self_type ascr in
      return @@ O.E_ascription ascr
    | I.E_module_accessor ma -> return @@ O.E_module_accessor ma
    | I.E_cond {condition;then_clause;else_clause} ->
      let condition    = self condition in
      let then_clause = self then_clause in
      let else_clause = self else_clause in
      return @@ O.E_cond {condition;then_clause;else_clause}
    | I.E_sequence {expr1; expr2} ->
      let expr1 = compile_expression ~raise expr1 in
      let expr2 = compile_expression ~raise expr2 in
      return @@ O.E_sequence {expr1; expr2}
    | I.E_skip () -> return @@ O.E_skip ()
    | I.E_tuple tuple ->
      let tuple = List.map ~f:self tuple in
      return @@ O.E_tuple tuple
    | I.E_assign {binder=b; expression} ->
      let binder = Binder.map self_type_option b in
      let expression = self expression in
      return @@ O.E_assign {binder; expression}
    | I.E_for {binder;start;final;incr;f_body} ->
      let final = compile_expression ~raise final in
      let start = compile_expression ~raise start in
      let incr = compile_expression ~raise incr in
      let f_body = compile_expression ~raise f_body in
      return @@ O.E_for {binder;start;final;incr;f_body}
    | I.E_for_each {fe_binder;collection;collection_type; fe_body} ->
      (* TODO previously this was translated to E_constant with C_*_ITER, hmm *)
      let fe_body = compile_expression ~raise fe_body in
      let collection = compile_expression ~raise collection in
      return @@ O.E_for_each {fe_binder; collection; collection_type; fe_body}
    | I.E_while {cond;body} ->
      let cond = compile_expression ~raise cond in
      let body = compile_expression ~raise body in
      return @@ O.E_while {cond; body}

and compile_declaration ~raise : I.declaration -> O.declaration = fun d ->
  let return wrap_content : O.declaration = {d with wrap_content} in
  match Location.unwrap d with
  | D_value {binder;expr;attr} ->
    let binder = Binder.map (compile_type_expression_option ~raise) binder in
    let expr   = compile_expression ~raise expr in
    return @@ D_value {binder;expr;attr}
  | D_type {type_binder;type_expr;type_attr} ->
    let type_expr = compile_type_expression ~raise type_expr in
    return @@ D_type {type_binder;type_expr;type_attr}
  | D_module {module_binder;module_;module_attr} ->
    let module_ = compile_module_expr ~raise module_ in
    return @@ D_module {module_binder;module_;module_attr}

and compile_module_expr ~raise : I.module_expr -> O.module_expr = fun me ->
  let return wrap_content : O.module_expr = {me with wrap_content} in
  match me.wrap_content with
    M_struct lst ->
      let lst = compile_module ~raise lst in
      return @@ M_struct lst
  | M_variable mv ->
      return @@ M_variable mv
  | M_module_path mp ->
      return @@ M_module_path mp

and compile_decl ~raise : I.decl -> O.decl = fun d -> compile_declaration ~raise d
and compile_module ~raise : I.module_ -> O.module_ = fun m ->
  List.map ~f:(compile_decl ~raise) m

let compile_program ~raise : I.program -> O.program = fun p ->
  Simple_utils.Trace.collect ~raise @@
  List.map ~f:(fun a ~raise -> compile_declaration ~raise a) p
