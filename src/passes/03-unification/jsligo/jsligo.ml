open Simple_utils.Utils
open Simple_utils.Trace
open Unification_shared.Helpers

module CST = Cst.Jsligo
module AST = Ast_unified

module Option = Simple_utils.Option

open AST  (* Brings types and combinators functions *)

module TODO_do_in_parsing = struct
  let unused_node () = failwith "unused node, can we clean ?"
  let labelize x = Label.of_string x
  let labelize_pattern p = match p with
    (* would be better to emit a label/string directly ? *)
    | CST.PVar var -> Label.of_string var.value.variable.value 
    | _ -> failwith "impossible??"
  let r_split = r_split (* could compute Location directly in Parser *)
  let var ~loc var = Ligo_prim.Value_var.of_input_var ~loc var
  let tvar ~loc var = Ligo_prim.Type_var.of_input_var ~loc var
  let mvar ~loc var = Ligo_prim.Module_var.of_input_var ~loc var
  let t_disc_locs (objs: (CST.obj_type, CST.vbar) nsepseq) =
    (* The region of the discriminated union TDisc
    is the union of all its objects' regions *)
    let locations = List.Ne.map (fun obj -> snd @@ r_split obj) (nsepseq_to_nseq objs) in
    List.Ne.fold_left locations ~init:Location.dummy ~f:Location.cover
end
module TODO_unify_in_cst = struct
  let conv_attr (attr:CST.attributes) =
    List.map attr ~f:(fun attr_reg ->
      let key,loc = r_split attr_reg in
      Temp_prim.Attribute.{ key ; value = None },loc
    )
  let s_attach_attr (attr:CST.attributes) (e:AST.statement_jsligo) : AST.statement_jsligo =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr,loc) -> s_attrjs ~loc attr e ())
  let p_attach_attr (attr:CST.attributes) (e:AST.pattern) : AST.pattern =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr,loc) ->
      Location.wrap ~loc (P_attr (attr, e)))
  let t_attach_attr (attr:CST.attributes) (e:AST.type_expr) : AST.type_expr =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr,loc) -> t_attr ~loc attr e ())
  let compile_rows = Non_linear_rows.make
  let compile_disc_rows = Non_linear_disc_rows.make
end

let rec compile_val_binding ~(raise: ('e, 'w) raise) : CST.val_binding -> AST.let_binding = fun b ->
  let is_rec = false in
  let binders = List.Ne.singleton @@ compile_pattern ~raise b.binders in
  let type_params = Option.map b.type_params ~f:(fun (tp : CST.type_generics) ->
    List.Ne.map r_fst @@ nsepseq_to_nseq (r_fst tp).inside)
  in
  let rhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) b.lhs_type in
  let let_rhs = compile_expression ~raise b.expr in
  {is_rec; type_params; binders; rhs_type; let_rhs}

(* ========================== TYPES ======================================== *)

and compile_type_expression ~(raise: ('e, 'w) raise) : CST.type_expr -> AST.type_expr = fun te ->
  let self = compile_type_expression ~raise in
  match te with
  | TProd {inside ; attributes} -> (
    let t, loc = r_split inside in
    let t = List.Ne.map self @@ nsepseq_to_nseq t.inside in
    TODO_unify_in_cst.t_attach_attr attributes (
      t_prod t ~loc ()
    )
  )
  | TSum t -> (
    let CST.{variants ; attributes ; _}, loc = r_split t in
    let variants =
      let destruct : CST.variant -> _ = fun {tuple ; attributes} ->
        let CST.{constr ; params} = (r_fst tuple).inside in
        ( TODO_do_in_parsing.labelize (r_fst constr),
        Option.map ~f:(List.map ~f:self <@ nsepseq_to_list <@ snd) params,
        List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let lst = List.map (nsepseq_to_list (r_fst variants)) ~f:(destruct <@ r_fst) in
      TODO_unify_in_cst.compile_rows lst
    in
    TODO_unify_in_cst.t_attach_attr attributes (
      t_arg_sum_raw variants ~loc ()
    )
  )
  | TObject t -> (
    let CST.{ne_elements ; attributes ; _}, loc = r_split t in
    let fields =
      let destruct = fun CST.{ field_name; field_type ; attributes ; _ } ->
        ( TODO_do_in_parsing.labelize (r_fst field_name),
        Some (self field_type),
        List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let lst = List.map ~f:(destruct <@ r_fst) @@ nsepseq_to_list ne_elements in
      TODO_unify_in_cst.compile_rows lst
    in
    TODO_unify_in_cst.t_attach_attr attributes (
      t_record_raw ~loc fields ()
    )
  )
  | TApp t -> (
    let t, loc = r_split t in
    let constr, args = t in
    let constr : string = r_fst constr in
    let type_args = List.Ne.map self @@ nsepseq_to_nseq (r_fst args).inside in
    t_app {constr; type_args} ~loc ()
  )
  | TFun t -> (
    let (fta, _, te2), loc = r_split t in
    let fun_type_args =
      let compile_fun_type_arg : CST.fun_type_arg -> _ AST.Named_fun.fun_type_arg = fun fta ->
        let name = r_fst fta.name in
        let type_expr = self fta.type_expr in
        {name; type_expr}
      in
      List.map ~f:compile_fun_type_arg (nsepseq_to_list fta.inside) in
    let type_expr = self te2 in
    t_named_fun (fun_type_args, type_expr) ~loc ()
  )
  | TPar t -> (
    self (r_fst t).inside
  )
  | TVar t -> (
    let t, loc = r_split t in
    t_var (TODO_do_in_parsing.tvar ~loc t) ~loc ()
  )
  | TString t -> (
    let t, loc = r_split t in
    t_string t ~loc ()
  )
  | TInt t -> (
    let (s, z), loc = r_split t in
    t_int s z ~loc ()
  )
  | TModA t -> (
    let t, loc = r_split t in
    let module_path =
      let x,loc = r_split t.module_name in
      TODO_do_in_parsing.mvar ~loc x
    in
    let field = self t.field in
    t_moda {module_path; field} ~loc ()
  )
  | TDisc t -> (
    let loc = TODO_do_in_parsing.t_disc_locs t in
    let fields =
      let destruct_field CST.{field_name ; field_type; attributes ; _} =
        ( Label.of_string (r_fst field_name),
        Some (self field_type),
        List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let destruct_obj = fun (x:CST.obj_type) ->
        let CST.{ attributes ; ne_elements ; _ },loc = r_split x in
        let lst = List.map ~f:(destruct_field <@ r_fst) (nsepseq_to_list ne_elements) in
        ( ()
        , t_record_raw ~loc (TODO_unify_in_cst.compile_rows lst) ()
        , List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let lst = List.map ~f:destruct_obj (nsepseq_to_list t) in
      TODO_unify_in_cst.compile_disc_rows lst
    in
    t_disc_union fields ~loc ()
  )



(* ========================== PATTERNS ===================================== *)

and compile_pattern ~(raise: ('e, 'w) raise) : CST.pattern -> AST.pattern = fun p ->
  let self = compile_pattern ~raise in
  let pat ~loc p = Location.wrap ~loc p in
  match p with
  | PConstr _p -> TODO_do_in_parsing.unused_node ()
  | PAssign   _p -> TODO_do_in_parsing.unused_node ()
  | PDestruct _p -> TODO_do_in_parsing.unused_node ()
  | PRest p -> (
    let (p : CST.rest_pattern), loc = r_split p in
    let s = r_fst p.rest in
    pat ~loc (P_rest (TODO_do_in_parsing.labelize s))
  )
  | PVar      p -> (
    let CST.{variable ; attributes}, loc = r_split p in
    let s = r_fst variable in
    TODO_unify_in_cst.p_attach_attr attributes (
      pat ~loc (P_var (TODO_do_in_parsing.var ~loc s))
    )
  )
  | PObject p -> (
    let (record, loc) = r_split p in
    let lps = List.map
      ~f:(fun p ->  
        let l = TODO_do_in_parsing.labelize_pattern p in
        Punned l)
      (Utils.nsepseq_to_list record.inside) in
    Location.wrap ~loc (P_pun_record lps)
  )
  | PArray p -> (
    let p, loc = r_split p in
    let p = List.map ~f:self (nsepseq_to_list p.inside) in
    pat ~loc (P_tuple p)
  )

(* ========================== STATEMENTS ================================= *)

and compile_statement ~(raise: ('e, 'w) raise) : CST.statement -> AST.statement_jsligo = fun s ->
  let self = compile_statement ~raise in
  let () = ignore (self, raise) in
  let extract_type_vars : CST.type_vars -> string nseq = fun tv ->
    List.Ne.map r_fst @@ nsepseq_to_nseq (r_fst tv).inside
  in
  match s with
  | SBlock s -> (
    let s, loc = r_split s in
    let statements = List.Ne.map self @@ nsepseq_to_nseq s.inside in
    s_block statements ~loc ()
  )
  | SExpr s -> (
    let expr = compile_expression ~raise s in
    let loc = expr.location in
    s_expr expr ~loc ()
  )
  | SCond s -> (
    let s, loc = r_split s in
    let test = compile_expression ~raise s.test.inside in
    let ifso = self s.ifso in
    let ifnot = Option.map ~f:(self <@ snd) s.ifnot in
    s_cond {test; ifso; ifnot} ~loc ()
  )
  | SReturn s -> (
    let s, loc = r_split s in
    let expr_opt = Option.map ~f:(compile_expression ~raise) s.expr in
    s_return expr_opt ~loc ()
  )
  | SLet s -> (
    let CST.{bindings ; attributes ; kwd_let = _}, loc = r_split s in
    let bindings = List.Ne.map (compile_val_binding ~raise <@ r_fst) @@ nsepseq_to_nseq bindings in
    TODO_unify_in_cst.s_attach_attr attributes (
      s_let bindings ~loc ()
    )
  )
  | SConst s -> (
    let CST.{bindings ; attributes ; _}, loc = r_split s in
    let bindings = List.Ne.map (compile_val_binding ~raise <@ r_fst) @@ nsepseq_to_nseq bindings in
    TODO_unify_in_cst.s_attach_attr attributes (
      s_const bindings ~loc ()
    )
  )
  | SType s -> (
    let CST.{attributes ; name ; params ; type_expr ; _ }, loc = r_split s in
    let name      = r_fst name in
    let params    = Option.map ~f:extract_type_vars params in
    let type_expr = compile_type_expression ~raise type_expr in
    TODO_unify_in_cst.s_attach_attr attributes (
      s_type {name; params; type_expr} ~loc ()
    )
  )
  | SSwitch s -> (
    let s, loc = r_split s in
    let switch_expr = compile_expression ~raise s.expr in
    let switch_cases =
      let translate_statements_opt = Option.map ~f:(List.Ne.map self <@ nsepseq_to_nseq) in
      let translate_switch_case : CST.switch_case -> AST.switch_case = function
      | CST.Switch_case c -> (
        let e     = compile_expression ~raise c.expr in
        let s_opt = translate_statements_opt c.statements in
        AST.Switch_case (e, s_opt)
      )
      | CST.Switch_default_case c -> (
        let s_opt = translate_statements_opt c.statements in
        AST.Switch_default_case s_opt
      )
      in
      List.Ne.map translate_switch_case s.cases
    in
    s_switch {switch_expr; switch_cases} ~loc ()
  )
  | SBreak s -> (
    let _, loc = w_split s  in
    s_break ~loc ()
  )
  | SNamespace s -> (
    let s, loc = r_split s in
    let (_, module_name, statements, attributes) = s in
    let module_name       = r_fst module_name in
    let namespace_content =
      List.Ne.map self @@ nsepseq_to_nseq (r_fst statements).inside
    in
    TODO_unify_in_cst.s_attach_attr attributes (
      s_namespace {module_name; namespace_content} ~loc ()
    )
  )
  | SExport s -> (
    let (_, s), loc = r_split s in
    let s = self s in
    s_export s ~loc ()
  )
  | SImport s -> (
    let s, loc = r_split s in
    let s : AST.import = match s with
    | CST.Import_rename s -> (
      let alias       = r_fst s.alias in
      let module_path = List.Ne.map r_fst @@ nsepseq_to_nseq s.module_path in
      AST.Import_rename {alias; module_path}
    )
    | CST.Import_all_as s -> (
      let alias      = r_fst s.alias in
      let module_str = r_fst s.module_path in
      AST.Import_all_as {alias; module_str}
    )  
    | CST.Import_selected s -> (
      let imported   = List.Ne.map r_fst @@ nsepseq_to_nseq (r_fst s.imported).inside in
      let module_str = r_fst s.module_path in
      AST.Import_selected {imported; module_str}
    )
    in
    s_import s ~loc ()
  )
  | SWhile s -> (
    let s, loc = r_split s in
    let expr       = compile_expression ~raise s.expr in
    let while_body = self s.statement in
    s_while {expr; while_body } ~loc ()
  )
  | SForOf s -> (
    let s, loc = r_split s in
    let index_kind = match s.index_kind with
      | `Let   _ -> AST.Let
      | `Const _ -> AST.Const
    in
    let index = r_fst s.index in
    let expr  = compile_expression ~raise s.expr in
    let for_stmt = self s.statement in
    s_forof {index_kind; index; expr; for_stmt} ~loc ()
  )

(* ========================== EXPRESSIONS ================================== *)

and compile_expression ~(raise: ('e, 'w) raise) : CST.expr -> AST.expr = fun e ->
  let self = compile_expression ~raise in
  let return e = e in
  let e_constant_of_bin_op_reg (op_type : Ligo_prim.Constant.constant') (op : _ CST.bin_op CST.reg) =
    let op, loc = r_split op in
    let a = self op.arg1 in
    let b = self op.arg2 in
    e_constant ~loc (Const op_type) [a; b]
  in
  let e_constant_of_un_op_reg (op_type : Ligo_prim.Constant.constant') (op : _ CST.un_op CST.reg) =
    let op, loc = r_split op in
    let arg = self op.arg in
    e_constant ~loc (Const op_type) [arg]
  in
  let translate_selection_jsligo : CST.selection -> AST.expr AST.selection = function
  | FieldName name -> let name = r_fst (r_fst name).value in AST.FieldName name
  | Component comp -> let comp = self (r_fst comp).inside in AST.Component comp
  in
  return @@ match e with
  | EVar var -> (
    let var, loc = r_split var in
    e_variable (TODO_do_in_parsing.var ~loc var) ~loc ()
  )
  | EPar par -> (
    let par, loc = r_split par in
    let par = self par.inside in
    e_par par ~loc ()
  )
  | EUnit the_unit -> (
    let _, loc = r_split the_unit in
    return @@ e_unit ~loc ()
  )
  | EBytes bytes_ -> (
    let bytes_, loc = r_split bytes_ in
    let _s, b = bytes_ in
    e_bytes_hex b ~loc
  )
  | EString str -> (
    match str with
    | String str ->
      let (str, loc) = r_split str in
      e_string str ~loc ()
    | Verbatim str ->
      let (str, loc) = r_split str in
      e_verbatim str ~loc ()
  )
  | EArith arth -> (
    match arth with
    | Add plus   -> e_constant_of_bin_op_reg C_POLYMORPHIC_ADD plus
    | Sub minus  -> e_constant_of_bin_op_reg C_POLYMORPHIC_SUB minus
    | Mult times -> e_constant_of_bin_op_reg C_MUL times
    | Div slash  -> e_constant_of_bin_op_reg C_DIV slash
    | Mod mod_   -> e_constant_of_bin_op_reg C_MOD mod_
    | Neg minus  -> e_constant_of_un_op_reg C_NEG minus
    | Int i ->
      let ((_,i), loc) = r_split i in
      return @@ e_int_z ~loc i
    )
  | ELogic logic -> (
    match logic with
      BoolExpr be -> (
      match be with
        Or or_   -> e_constant_of_bin_op_reg C_OR  or_
      | And and_ -> e_constant_of_bin_op_reg C_AND and_
      | Not not_ -> e_constant_of_un_op_reg  C_NOT not_
    )
    | CompExpr ce -> (
      match ce with
        Lt lt    -> e_constant_of_bin_op_reg C_LT  lt
      | Leq le   -> e_constant_of_bin_op_reg C_LE  le
      | Gt gt    -> e_constant_of_bin_op_reg C_GT  gt
      | Geq ge   -> e_constant_of_bin_op_reg C_GE  ge
      | Equal eq -> e_constant_of_bin_op_reg C_EQ  eq
      | Neq ne   -> e_constant_of_bin_op_reg C_NEQ ne
    )
  )
  | ECall call -> (
    let (expr, args), loc = r_split call in
    let expr = self expr in
    let args : AST.expr nseq =
      match args with
      | Unit the_unit -> List.Ne.singleton @@ self (CST.EUnit the_unit)
      | Multiple args -> List.Ne.map self @@ nsepseq_to_nseq (r_fst args).inside
      in
    e_call expr args ~loc ()
  )
  | EConstr constr -> (
    let (constr, arg_opt), loc = r_split constr in
    let name = constr.value in
    let arg_opt = Option.map ~f:self arg_opt in
    e_constr (name,arg_opt) ~loc ()
  )
  | EArray items -> (
    let items, loc = r_split items in
    let items : AST.array_jsligo =
      let translate_array_item : CST.array_item -> AST.array_item_jsligo = function
      | Expr_entry e -> Expr_entry (self e)
      | Rest_entry e -> Rest_entry (self (r_fst e).expr)
      in
      match items.inside with
      | Some items -> List.map ~f:translate_array_item @@ nsepseq_to_list items
      | None       -> []
    in
    e_arrayjsligo items ~loc ()
  )
  | EObject obj -> (
    let obj, loc = r_split obj in
    let props : AST.object_jsligo =
      let translate_property : CST.property -> AST.property_jsligo = function
      | Punned_property e -> let e = r_fst e in AST.Punned_property (self e)
      | Property p        -> let p = r_fst p in AST.Property (self p.name, self p.value)
      | Property_rest p   -> let p = r_fst p in AST.Property_rest (self p.expr)
      in
      nseq_map translate_property @@ nsepseq_to_nseq obj.inside
    in
    e_objectjsligo props ~loc ()
  )
  | EProj proj -> (
    let proj, loc = r_split proj in
    let expr      = self proj.expr in
    let selection = translate_selection_jsligo proj.selection in
    e_projjsligo {expr; selection} ~loc ()
  )
  | EModA ma -> (
    let ma, loc = r_split ma in
    let module_path = r_fst ma.module_name in
    let field = self ma.field in
    e_moda {module_path; field} ~loc ()
  )
  | EFun f -> (
    let f, loc = r_split f in
    let parameters = self f.parameters in
    let lhs_type   = Option.map ~f:(compile_type_expression ~raise <@ snd) f.lhs_type in
    let body =
      let compile_body : CST.body -> AST.body_jsligo = function
      | FunctionBody   b -> AST.FunctionBody   (nseq_map (compile_statement ~raise) @@ nsepseq_to_nseq (r_fst b).inside)
      | ExpressionBody e -> AST.ExpressionBody (self e)
      in
      compile_body f.body
    in
    e_funjsligo {parameters; lhs_type; body} ~loc ()
  )
  | EAnnot a -> (
    let (e, _, te), loc = r_split a in
    let e = self e in
    let te = compile_type_expression ~raise te in
    e_annot (e, te) ~loc ()
  )

  | ECodeInj ci -> (
    let ci, loc = r_split ci in
    let language = r_fst ci.language in
    let code = self ci.code in
    e_rawcode {language; code} ~loc ()
  )
  | ESeq seq -> (
    let seq, loc = r_split seq in
    let seq = List.map ~f:self @@ nsepseq_to_list seq in
    e_seq seq ~loc ()
  )
  | EAssign (expr1, op, expr2) -> (
    let op, loc = r_split op in
    let expr1 = self expr1 in
    let op =
      let translate_operator_jsligo : CST.operator -> AST.operator_jsligo = function
      | Eq -> AST.Eq
      | Assignment_operator aop -> AST.Assignment_operator (
        match aop with
        | Times_eq -> AST.Times_eq
        | Div_eq   -> AST.Div_eq
        | Min_eq   -> AST.Min_eq
        | Plus_eq  -> AST.Plus_eq
        | Mod_eq   -> AST.Mod_eq
        )
      in
      translate_operator_jsligo op
    in
    let expr2 = self expr2 in
    e_assignjsligo {expr1; op; expr2} ~loc ()
  )
  | ETernary e -> (
    let e, loc = r_split e in
    let test   = self e.condition in
    let truthy = self e.truthy in
    let falsy = self e.falsy in
    e_ternary {test; truthy; falsy} ~loc ()
  )

(* ========================== DECLARATIONS ================================= *)

let compile_toplevel_statement ~(raise: ('e, 'w) raise) : CST.toplevel_statement -> AST.declaration = fun s -> 
  match s with
  | Directive d -> (
    let loc = Simple_utils.Location.lift (Preprocessor.Directive.to_region d) in
    d_directive d ~loc ()
  )
  | TopLevel (statement, _semicolon_opt) -> (
    let statement = compile_statement ~raise statement in
    let loc = statement.location in
    d_topleveljsligo statement ~loc ()
  )
  
let compile_program ~raise : CST.t -> AST.program = fun t ->
  let declarations :                    CST.toplevel_statement  list = nseq_to_list t.statements in
  let declarations : (raise: ('e, 'w) raise -> AST.declaration) list = List.map ~f:(fun a ~raise -> compile_toplevel_statement ~raise a) declarations in
  let declarations :                           AST.declaration  list = Simple_utils.Trace.collect ~raise declarations in
  declarations
