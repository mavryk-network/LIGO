module Location = Simple_utils.Location
open Simple_utils.Trace
open Errors
open Ligo_prim
module I = Ast_imperative
module O = Ast_core

let is_layout = String.chop_prefix ~prefix:"layout:"

let get_layout : string list -> Layout.t option =
 fun attributes ->
  let rec aux lst =
    match lst with
    | hd :: tl ->
      (match is_layout hd with
      | Some "tree" -> Some Layout.L_tree
      | Some "comb" -> Some Layout.L_comb
      (*deal with wrong layout*)
      | None | Some _ -> aux tl)
    | [] -> None
  in
  aux attributes


let compile_value_attributes : I.Attr.t -> O.Value_attr.t =
 fun attributes ->
  let is_inline attr = String.equal "inline" attr in
  let is_no_mutation attr = String.equal "no_mutation" attr in
  let is_view attr = String.equal "view" attr in
  let is_hidden attr = String.equal "hidden" attr in
  let is_thunk attr = String.equal "thunk" attr in
  let get_inline : string list -> bool = List.exists ~f:is_inline in
  let get_no_mutation : string list -> bool = List.exists ~f:is_no_mutation in
  let get_view : string list -> bool = List.exists ~f:is_view in
  let get_hidden : string list -> bool = List.exists ~f:is_hidden in
  let get_public : string list -> bool =
   fun attr -> not (List.mem attr "private" ~equal:String.equal)
  in
  let get_thunk : string list -> bool = List.exists ~f:is_thunk in
  let inline = get_inline attributes in
  let no_mutation = get_no_mutation attributes in
  let public = get_public attributes in
  let view = get_view attributes in
  let hidden = get_hidden attributes in
  let thunk = get_thunk attributes in
  { inline; no_mutation; view; public; hidden; thunk }


let compile_type_attributes : I.Attr.t -> O.Type_or_module_attr.t =
 fun attributes ->
  let get_public : string list -> bool =
   fun attr -> not (List.mem attr "private" ~equal:String.equal)
  in
  let get_hidden : string list -> bool =
   fun attr -> List.mem attr "hidden" ~equal:String.equal
  in
  let public = get_public attributes in
  let hidden = get_hidden attributes in
  { public; hidden }


let compile_module_attributes = compile_type_attributes

let compile_row_attributes : string list -> O.Layout_attr.t =
 fun attributes ->
  let layout =
    List.find_map attributes ~f:(fun attr ->
        match is_layout attr with
        | Some "tree" -> Some Layout.L_tree
        | Some "comb" -> Some Layout.L_comb
        | _ -> None)
  in
  { layout }


let rec compile_type_expression ~raise (type_ : I.type_expression)
    : O.type_expression
  =
  let loc = type_.location in
  let compile_type_expression = compile_type_expression ~raise in
  let compile_row = compile_row ~raise in
  let return content = O.make_t ~loc content in
  match type_.type_content with
  | I.T_sum row ->
    let row = compile_row row in
    return @@ O.T_sum row
  | I.T_record row ->
    let row = compile_row row in
    return @@ O.T_record row
  | I.T_tuple tuple ->
    let tuple = Tuple.map compile_type_expression tuple in
    return @@ O.T_tuple tuple
  | I.T_arrow arr ->
    let arr = Arrow.map compile_type_expression arr in
    return @@ T_arrow arr
  | I.T_variable type_variable -> return @@ T_variable type_variable
  | I.T_app { type_operator; arguments = [ l; r ] }
    when Type_var.equal Literal_types.v_michelson_or type_operator ->
    let l, l_ann =
      trace_option ~raise (corner_case "not an annotated type")
      @@ I.get_t_annoted l
    in
    let r, r_ann =
      trace_option ~raise (corner_case "not an annotated type")
      @@ I.get_t_annoted r
    in
    let l = compile_type_expression l in
    let r = compile_type_expression r in
    O.t_michelson_sum ~loc l l_ann r r_ann
  | I.T_app { type_operator; arguments = [ l; r ] }
    when Type_var.equal Literal_types.v_michelson_pair type_operator ->
    let l, l_ann =
      trace_option ~raise (corner_case "not an annotated type")
      @@ I.get_t_annoted l
    in
    let r, r_ann =
      trace_option ~raise (corner_case "not an annotated type")
      @@ I.get_t_annoted r
    in
    let l = compile_type_expression l in
    let r = compile_type_expression r in
    O.t_michelson_pair ~loc l l_ann r r_ann
  | I.T_app type_app ->
    let type_app = Type_app.map compile_type_expression type_app in
    return @@ T_app type_app
  | I.T_module_accessor ma -> return @@ T_module_accessor ma
  | I.T_annoted (type_, _) -> compile_type_expression type_
  | I.T_singleton t -> return @@ O.T_singleton t
  | I.T_abstraction abs ->
    let abs = Abstraction.map compile_type_expression abs in
    return @@ O.T_abstraction abs
  | I.T_for_all for_all ->
    let for_all = Abstraction.map compile_type_expression for_all in
    return @@ O.T_for_all for_all


and compile_row ~raise ({ fields; attributes } : _ I.Rows.t) : _ O.Rows.t =
  let fields =
    List.Assoc.map
      fields
      ~f:(fun
           ({ content = { associated_type; decl_pos }; attributes = _ } :
             _ I.Rows.Elem.t)
           : _ O.Rows.Elem.t
         ->
        let associated_type = compile_type_expression ~raise associated_type in
        { content = { associated_type; decl_pos }; michelson_annotation = None })
    |> Label.Map.of_alist_exn
  in
  let attributes = compile_row_attributes attributes in
  { fields; attributes }


let compile_type_expression_option ~raise te_opt =
  Option.map ~f:(compile_type_expression ~raise) te_opt


let rec compile_expression ~raise : I.expression -> O.expression =
 fun expr ->
  let loc = expr.location in
  let self = compile_expression ~raise in
  let self_type = compile_type_expression ~raise in
  let self_type_option = compile_type_expression_option ~raise in
  let return expr = O.make_e ~loc expr in
  match expr.expression_content with
  | I.E_literal literal -> return @@ O.E_literal literal
  | I.E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:self arguments in
    let cons_name = Constant.const_name cons_name in
    return @@ O.E_constant { cons_name; arguments }
  | I.E_variable name -> return @@ O.E_variable name
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
  | I.E_let_in { let_binder; attributes; rhs; let_result } ->
    let let_binder = Binder.map self_type_option let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    let attributes = compile_value_attributes attributes in
    return @@ O.E_let_in { let_binder; attributes; rhs; let_result }
  | I.E_type_in ti ->
    let ti = Type_in.map self self_type ti in
    return @@ O.E_type_in ti
  | I.E_mod_in { module_binder; rhs; let_result } ->
    let rhs = compile_module_expr ~raise rhs in
    let let_result = self let_result in
    return @@ O.E_mod_in { module_binder; rhs; let_result }
  | I.E_raw_code rc ->
    let rc = Raw_code.map self rc in
    return @@ O.E_raw_code rc
  | I.E_constructor const ->
    let const = Constructor.map self const in
    return @@ O.E_constructor const
  | I.E_matching match_expr ->
    let m = Match_expr.map self self_type_option match_expr in
    return @@ O.E_matching m
  | I.E_record record ->
    let record =
      record |> List.map ~f:(fun (l, e) -> l, self e) |> Label.Map.of_alist_exn
    in
    return @@ O.E_record record
  | I.E_accessor { struct_; path } ->
    let struct_ = self struct_ in
    let accessor expr a =
      match (a : _ Access_path.access) with
      | Access_tuple i -> O.e_record_accessor ~loc expr (Label (Z.to_string i))
      | Access_record a -> O.e_record_accessor ~loc expr (Label a)
      | Access_map k ->
        let k = self k in
        O.e_constant ~loc C_MAP_FIND_OPT [ k; expr ]
    in
    List.fold ~f:accessor ~init:struct_ path
  | I.E_update { struct_; update; path } ->
    let struct_ = self struct_ in
    let update = self update in
    let accessor ~loc expr a =
      match (a : _ Access_path.access) with
      | Access_tuple i -> O.e_record_accessor ~loc expr (Label (Z.to_string i))
      | Access_record a -> O.e_record_accessor ~loc expr (Label a)
      | Access_map k ->
        let k = self k in
        O.e_constant ~loc C_MAP_FIND_OPT [ k; expr ]
    in
    let updator ~loc (s : O.expression) a expr =
      match (a : _ Access_path.access) with
      | Access_tuple i -> O.e_record_update ~loc s (Label (Z.to_string i)) expr
      | Access_record a -> O.e_record_update ~loc s (Label a) expr
      | Access_map k ->
        let k = self k in
        O.e_constant ~loc C_MAP_ADD [ k; expr; s ]
    in
    let aux ((s, e) : O.expression * _) lst =
      let s' = accessor ~loc:s.location s lst in
      let e' expr =
        let u = updator ~loc:s.location s lst expr in
        e u
      in
      s', e'
    in
    let _, rhs = List.fold ~f:aux ~init:(struct_, fun e -> e) path in
    rhs @@ update
  | I.E_map map ->
    let map = Map_expr.map self map in
    return @@ O.E_map map
  | I.E_big_map map ->
    let map = Map_expr.map self map in
    return @@ O.E_big_map map
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
  | I.E_cond cond ->
    let cond = Conditional.map self cond in
    return @@ O.E_cond cond
  | I.E_sequence seq ->
    let seq = Sequence.map self seq in
    return @@ O.E_sequence seq
  | I.E_skip () -> return @@ O.E_skip
  | I.E_tuple tuple ->
    let tuple = List.map ~f:self tuple in
    return @@ O.E_tuple tuple
  | I.E_assign { binder = b; expression } ->
    let binder = Binder.map self_type_option b in
    let expression = self expression in
    return @@ O.E_assign { binder; expression }
  | I.E_for for_loop ->
    let for_loop = For_loop.map self for_loop in
    return @@ O.E_for for_loop
  | I.E_for_each for_each_loop ->
    let for_each_loop = For_each_loop.map self for_each_loop in
    return @@ O.E_for_each for_each_loop
  | I.E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ O.E_while while_loop
  | I.E_let_mut_in { let_binder; attributes; rhs; let_result } ->
    let let_binder = Binder.map self_type_option let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    let attributes = compile_value_attributes attributes in
    return @@ O.E_let_mut_in { let_binder; attributes; rhs; let_result }


and compile_declaration ~raise : I.declaration -> O.declaration =
 fun d ->
  let return wrap_content : O.declaration = { d with wrap_content } in
  match Location.unwrap d with
  | D_value { binder; expr; attr } ->
    let binder = Binder.map (compile_type_expression_option ~raise) binder in
    let expr = compile_expression ~raise expr in
    let attr = compile_value_attributes attr in
    return @@ D_value { binder; expr; attr }
  | D_type { type_binder; type_expr; type_attr } ->
    let type_expr = compile_type_expression ~raise type_expr in
    let type_attr = compile_type_attributes type_attr in
    return @@ D_type { type_binder; type_expr; type_attr }
  | D_module { module_binder; module_; module_attr } ->
    let module_ = compile_module_expr ~raise module_ in
    let module_attr = compile_module_attributes module_attr in
    return @@ D_module { module_binder; module_; module_attr }


and compile_module_expr ~raise : I.module_expr -> O.module_expr =
 fun me ->
  let return wrap_content : O.module_expr = { me with wrap_content } in
  match me.wrap_content with
  | M_struct lst ->
    let lst = compile_module ~raise lst in
    return @@ M_struct lst
  | M_variable mv -> return @@ M_variable mv
  | M_module_path mp -> return @@ M_module_path mp


and compile_decl ~raise : I.decl -> O.decl =
 fun d -> compile_declaration ~raise d


and compile_module ~raise : I.module_ -> O.module_ =
 fun m -> List.map ~f:(compile_decl ~raise) m


let compile_program ~raise : I.program -> O.program =
 fun p ->
  Simple_utils.Trace.collect ~raise
  @@ List.map ~f:(fun a ~raise -> compile_declaration ~raise a) p
