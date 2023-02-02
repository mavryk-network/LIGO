open Simple_utils.Utils
open Simple_utils.Trace
open Unification_shared.Helpers
module CST = Cst.Jsligo
module AST = Ast_unified
module Option = Simple_utils.Option
open AST (* Brings types and combinators functions *)

module TODO_do_in_parsing = struct
  let unused_node () = failwith "unused node, can we clean ?"
  let labelize x = Label.of_string x

  let labelize_pattern p =
    match p with
    (* would be better to emit a label/string directly ? *)
    | CST.PVar var -> Label.of_string var.value.variable.value
    | _ -> failwith "impossible??"


  let r_split = r_split (* could compute Location directly in Parser *)
  let var ~loc var = Ligo_prim.Value_var.of_input_var ~loc var
  let tvar ~loc var = Ligo_prim.Type_var.of_input_var ~loc var
  let mvar ~loc var = Ligo_prim.Module_var.of_input_var ~loc var

  let t_disc_locs (objs : (CST.obj_type, CST.vbar) nsepseq) =
    (* The region of the discriminated union TDisc
    is the union of all its objects' regions *)
    let locations = List.Ne.map (fun obj -> snd @@ r_split obj) (nsepseq_to_nseq objs) in
    List.Ne.fold_left locations ~init:Location.dummy ~f:Location.cover


  let export_statement_to_decl s : declaration =
    let rec aux s =
      match get_s s with
      | S_Instr _ -> unused_node ()
      | S_Decl declaration -> declaration
      | S_Attr (attr, x) -> d_attr ~loc:(get_s_loc x) (attr, aux x)
    in
    aux s


  let rec pattern_of_expr compile_type : CST.expr -> AST.pattern list =
    (*
    this is the most troubling thing with jsligo: functions parameters is a single expression
    `<parameters:expr> : <lhs_type:type> => ..` (see EFun node bellow)
     I believe they should really be parsed as a pattern list

    note:
      as of today, this is a value expression `([x ,...[y, ...z]]) => x`
      while this is not a valid declaration `const [x ,...[y, ...z]] = toto`.
      in the first case, it's an "expression as a pattern". In the second case, it's a
      pattern
  *)
    let rec aux : CST.expr -> AST.pattern =
      let self = aux in
      function
      | EPar x -> self x.value.inside
      | EVar x ->
        let v, loc = r_split x in
        p_var ~loc (var ~loc v)
      | EUnit x -> p_unit ~loc:(Location.lift x.region)
      | EAnnot ea ->
        let (expr, _as, type_expr), loc = r_split ea in
        p_typed ~loc (compile_type type_expr) (self expr)
      | EArray items ->
        let items, loc = r_split items in
        (match sepseq_to_list items.inside with
        | [] -> p_list ~loc (List [])
        | [ Expr_entry hd; Rest_entry tl ] ->
          (* [x ,...[y, ...z]] *)
          (* see https://tezos-dev.slack.com/archives/GMHV0U3Q9/p1670929406569099 *)
          p_list ~loc (Cons (self hd, self tl.value.expr))
        | lst ->
          p_tuple
            ~loc
            (List.map lst ~f:(function
                | Expr_entry x -> self x
                | _ -> failwith "incorrect pattern")))
      | EObject obj ->
        let obj, loc = r_split obj in
        let lst = nsepseq_to_list obj.inside in
        let aux : CST.property -> (_, _) Field.t = function
          | Punned_property { value = EVar v; _ } -> Punned (Label.of_string v.value)
          | Property { value = { name = EVar v; value; _ }; _ } ->
            Complete (Label.of_string v.value, aux value)
          | _ -> failwith "unrecognized pattern"
        in
        p_pun_record ~loc (List.map ~f:aux lst)
      | _ -> failwith "unrecognized pattern"
    in
    function
    | EPar x -> pattern_of_expr compile_type x.value.inside
    | ESeq seq ->
      let lst, _ = r_split seq in
      (match lst with
      | hd, [] -> [ aux hd ]
      | _ -> List.map ~f:aux (nsepseq_to_list lst))
    (* | EUnit _ -> [] *)
    | else_ -> [ aux else_ ]
end

module TODO_unify_in_cst = struct
  let conv_attr (attr : CST.attributes) =
    List.map attr ~f:(fun attr_reg ->
        let key, loc = r_split attr_reg in
        Temp_prim.Attribute.{ key; value = None }, loc)


  let s_attach_attr (attr : CST.attributes) (e : AST.statement) : AST.statement =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr, loc) -> s_attr ~loc (attr, e))


  let p_attach_attr (attr : CST.attributes) (e : AST.pattern) : AST.pattern =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr, loc) -> p_attr ~loc attr e)


  let t_attach_attr (attr : CST.attributes) (e : AST.ty_expr) : AST.ty_expr =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr, loc) -> t_attr ~loc attr e)


  let compile_rows = Non_linear_rows.make
  let compile_disc_rows = Non_linear_disc_rows.make
  let instr_as_stmt ~loc (x : AST.instruction) = AST.s_instr ~loc x

  let test_clause_branch x =
    AST.Test_clause.ClauseBlock (make_b ~loc:(get_s_loc x) (List.Ne.singleton x))


  let let_as_decl ~loc x = s_decl ~loc (d_multi_var ~loc x)
  let const_as_decl ~loc x = s_decl ~loc (d_multi_const ~loc x)
  let ty_as_decl ~loc x = s_decl ~loc (d_type_abstraction ~loc x)
  let import_as_decl ~loc x = s_decl ~loc (d_import ~loc x)

  let rec statement_to_program_entry s =
    match get_s s with
    | S_Attr (attr, x) -> program_entry (PE_Attr (attr, statement_to_program_entry x))
    | S_Instr instr -> program_entry (PE_Top_level_instruction instr)
    | S_Decl declaration -> program_entry (PE_Declaration declaration)


  let namespace_decl ~loc name (statements : statement nseq) =
    let pes = nseq_map statement_to_program_entry statements in
    s_decl ~loc (d_module ~loc { name; mod_expr = m_body ~loc pes })


  let i_expr ~loc expr =
    (* IIUC, this can be a return or a call ? could we parse it as such ?*)
    i_expr ~loc expr


  let type_operator ~loc v =
    (* could be a type expr ? or we could emit a type variable expression ? *)
    t_var ~loc (TODO_do_in_parsing.tvar ~loc v)


  let e_string ~loc s =
    e_literal ~loc (Literal_string (Simple_utils.Ligo_string.Standard s))


  let e_verbatim ~loc s =
    e_literal ~loc (Literal_string (Simple_utils.Ligo_string.Verbatim s))


  let nested_ctor_application ~loc (constr : CST.constr) arg_opt =
    let constructor = Label.of_string constr.value in
    let element = Option.map ~f:List.Ne.singleton arg_opt in
    e_ctor_app ~loc (e_constr ~loc:(Location.lift constr.region) constructor, element)


  let sum_rhs_as_a_type ~loc self params =
    Option.map ~f:(t_prod ~loc <@ List.Ne.map self <@ nsepseq_to_nseq <@ snd) params
end

let rec compile_val_binding : CST.val_binding -> (pattern, expr, ty_expr) Simple_decl.t =
 fun { binders; type_params; lhs_type; eq = _; expr } ->
  let pattern = compile_pattern binders in
  let type_params =
    Option.map type_params ~f:(fun (tp : CST.type_generics) ->
        List.Ne.map
          (fun x -> TODO_do_in_parsing.tvar ~loc:(r_snd x) (r_fst x))
          (nsepseq_to_nseq (r_fst tp).inside))
  in
  let rhs_type = Option.map ~f:(compile_type_expression <@ snd) lhs_type in
  let let_rhs = compile_expression expr in
  { type_params; pattern; rhs_type; let_rhs }


(* ========================== TYPES ======================================== *)

and compile_type_expression : CST.type_expr -> AST.ty_expr =
 fun te ->
  let self = compile_type_expression in
  match te with
  | TProd { inside; attributes } ->
    let t, loc = r_split inside in
    let t = List.Ne.map self @@ nsepseq_to_nseq t.inside in
    TODO_unify_in_cst.t_attach_attr attributes (t_prod t ~loc)
  | TSum t ->
    let CST.{ variants; attributes; _ }, loc = r_split t in
    let variants =
      let destruct : CST.variant -> _ =
       fun { tuple; attributes } ->
        let CST.{ constr; params } = (r_fst tuple).inside in
        ( TODO_do_in_parsing.labelize (r_fst constr)
        , TODO_unify_in_cst.sum_rhs_as_a_type ~loc self params
        , List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let lst = List.map (nsepseq_to_list (r_fst variants)) ~f:(destruct <@ r_fst) in
      TODO_unify_in_cst.compile_rows lst
    in
    TODO_unify_in_cst.t_attach_attr attributes (t_sum_raw variants ~loc)
  | TObject t ->
    let CST.{ ne_elements; attributes; _ }, loc = r_split t in
    let fields =
      let destruct CST.{ field_name; field_type; attributes; _ } =
        ( TODO_do_in_parsing.labelize (r_fst field_name)
        , Some (self field_type)
        , List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let lst = List.map ~f:(destruct <@ r_fst) @@ nsepseq_to_list ne_elements in
      TODO_unify_in_cst.compile_rows lst
    in
    TODO_unify_in_cst.t_attach_attr attributes (t_record_raw ~loc fields)
  | TApp t ->
    let t, loc = r_split t in
    let constr, args = t in
    let constr = TODO_unify_in_cst.type_operator ~loc:(r_snd constr) (r_fst constr) in
    let type_args = List.Ne.map self @@ nsepseq_to_nseq (r_fst args).inside in
    t_app { constr; type_args } ~loc
  | TFun t ->
    let (fta, _, te2), loc = r_split t in
    let fun_type_args =
      let compile_fun_type_arg : CST.fun_type_arg -> _ AST.Named_fun.fun_type_arg =
       fun fta ->
        let name = TODO_do_in_parsing.tvar ~loc:(r_snd fta.name) (r_fst fta.name) in
        let type_expr = self fta.type_expr in
        { name = Some name; type_expr }
      in
      List.map ~f:compile_fun_type_arg (nsepseq_to_list fta.inside)
    in
    let type_expr = self te2 in
    t_named_fun (fun_type_args, type_expr) ~loc
  | TPar t -> self (r_fst t).inside
  | TVar t ->
    let t, loc = r_split t in
    t_var (TODO_do_in_parsing.tvar ~loc t) ~loc
  | TString t ->
    let t, loc = r_split t in
    t_string t ~loc
  | TInt t ->
    let (s, z), loc = r_split t in
    t_int s z ~loc
  | TModA t ->
    let t, loc = r_split t in
    let module_path =
      let x, loc = r_split t.module_name in
      TODO_do_in_parsing.mvar ~loc x
    in
    let field = self t.field in
    t_module_open_in { module_path; field } ~loc
  | TDisc t ->
    let loc = TODO_do_in_parsing.t_disc_locs t in
    let fields =
      let destruct_field CST.{ field_name; field_type; attributes; _ } =
        ( Label.of_string (r_fst field_name)
        , Some (self field_type)
        , List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let destruct_obj (x : CST.obj_type) =
        let CST.{ attributes; ne_elements; _ }, loc = r_split x in
        let lst = List.map ~f:(destruct_field <@ r_fst) (nsepseq_to_list ne_elements) in
        ( ()
        , t_record_raw ~loc (TODO_unify_in_cst.compile_rows lst)
        , List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let lst = List.map ~f:destruct_obj (nsepseq_to_list t) in
      TODO_unify_in_cst.compile_disc_rows lst
    in
    t_disc_union fields ~loc


(* ========================== PATTERNS ===================================== *)

and compile_pattern : CST.pattern -> AST.pattern =
 fun p ->
  let self = compile_pattern in
  match p with
  | PConstr _p -> TODO_do_in_parsing.unused_node ()
  | PAssign _p -> TODO_do_in_parsing.unused_node ()
  | PDestruct _p -> TODO_do_in_parsing.unused_node ()
  | PRest p ->
    let (p : CST.rest_pattern), loc = r_split p in
    let s = r_fst p.rest in
    p_rest ~loc (TODO_do_in_parsing.labelize s)
  | PVar p ->
    let CST.{ variable; attributes }, loc = r_split p in
    let s = r_fst variable in
    TODO_unify_in_cst.p_attach_attr
      attributes
      (p_var ~loc (TODO_do_in_parsing.var ~loc s))
  | PObject p ->
    let record, loc = r_split p in
    let lps =
      List.map
        ~f:(fun p ->
          let l = TODO_do_in_parsing.labelize_pattern p in
          AST.Field.Punned l)
        (Utils.nsepseq_to_list record.inside)
    in
    p_pun_record ~loc lps
  | PArray p ->
    let p, loc = r_split p in
    let p = List.map ~f:self (nsepseq_to_list p.inside) in
    p_tuple ~loc p


(* ========================== STATEMENTS ================================= *)

and compile_statement : CST.statement -> AST.statement =
 fun s ->
  let self = compile_statement in
  let () = ignore (self, raise) in
  let extract_type_vars : CST.type_vars -> _ nseq =
   fun tv ->
    List.Ne.map
      (fun x -> TODO_do_in_parsing.tvar ~loc:(r_snd x) (r_fst x))
      (nsepseq_to_nseq (r_fst tv).inside)
  in
  match s with
  | SNamespace s ->
    let s, loc = r_split s in
    let _, module_name, statements, attributes = s in
    let name = TODO_do_in_parsing.mvar ~loc:(r_snd module_name) (r_fst module_name) in
    let stmts = List.Ne.map self (nsepseq_to_nseq (r_fst statements).inside) in
    TODO_unify_in_cst.s_attach_attr
      attributes
      (TODO_unify_in_cst.namespace_decl ~loc name stmts)
  | SImport s ->
    let s, loc = r_split s in
    let import =
      match s with
      | CST.Import_rename s ->
        let alias = TODO_do_in_parsing.mvar ~loc:(r_snd s.alias) (r_fst s.alias) in
        let module_path =
          List.Ne.map
            (fun x -> TODO_do_in_parsing.mvar ~loc:(r_snd x) (r_fst x))
            (nsepseq_to_nseq s.module_path)
        in
        Import.Import_rename { alias; module_path }
      | CST.Import_all_as s ->
        let alias = TODO_do_in_parsing.mvar ~loc:(r_snd s.alias) (r_fst s.alias) in
        let module_str = r_fst s.module_path in
        Import.Import_all_as { alias; module_str }
      | CST.Import_selected s ->
        let imported =
          List.Ne.map
            (fun x -> TODO_do_in_parsing.var ~loc:(r_snd x) (r_fst x))
            (nsepseq_to_nseq (r_fst s.imported).inside)
        in
        let module_str = r_fst s.module_path in
        Import.Import_selected { imported; module_str }
    in
    TODO_unify_in_cst.import_as_decl ~loc import
  | SExport e ->
    let (_, statement), loc = r_split e in
    let statement = self statement in
    s_decl ~loc (TODO_do_in_parsing.export_statement_to_decl statement)
  | SBlock s ->
    let s, loc = r_split s in
    let statements = List.Ne.map self @@ nsepseq_to_nseq s.inside in
    let block = make_b ~loc statements in
    TODO_unify_in_cst.instr_as_stmt ~loc (i_block ~loc block)
  | SExpr s ->
    let expr = compile_expression s in
    let loc = get_e_loc expr in
    TODO_unify_in_cst.instr_as_stmt ~loc (TODO_unify_in_cst.i_expr ~loc expr)
  | SCond s ->
    let s, loc = r_split s in
    let test = compile_expression s.test.inside in
    let ifso = TODO_unify_in_cst.test_clause_branch (self s.ifso) in
    let ifnot =
      Option.map ~f:(TODO_unify_in_cst.test_clause_branch <@ self <@ snd) s.ifnot
    in
    TODO_unify_in_cst.instr_as_stmt ~loc (i_cond { test; ifso; ifnot } ~loc)
  | SReturn s ->
    let s, loc = r_split s in
    let expr_opt = Option.map ~f:compile_expression s.expr in
    TODO_unify_in_cst.instr_as_stmt ~loc (i_return expr_opt ~loc)
  | SLet s ->
    let CST.{ bindings; attributes; kwd_let = _ }, loc = r_split s in
    let bindings =
      List.Ne.map (compile_val_binding <@ r_fst) (nsepseq_to_nseq bindings)
    in
    TODO_unify_in_cst.s_attach_attr
      attributes
      (TODO_unify_in_cst.let_as_decl ~loc bindings)
  | SConst s ->
    let CST.{ bindings; attributes; _ }, loc = r_split s in
    let bindings =
      List.Ne.map (compile_val_binding <@ r_fst) @@ nsepseq_to_nseq bindings
    in
    TODO_unify_in_cst.s_attach_attr
      attributes
      (TODO_unify_in_cst.const_as_decl ~loc bindings)
  | SType s ->
    let CST.{ attributes; name; params; type_expr; _ }, loc = r_split s in
    let name = TODO_do_in_parsing.tvar ~loc:(r_snd name) (r_fst name) in
    let params = Option.map ~f:extract_type_vars params in
    let type_expr = compile_type_expression type_expr in
    TODO_unify_in_cst.s_attach_attr
      attributes
      (TODO_unify_in_cst.ty_as_decl ~loc { name; params; type_expr })
  | SSwitch s ->
    let s, loc = r_split s in
    let switchee = compile_expression s.expr in
    let cases =
      let translate_statements_opt =
        Option.map ~f:(fun x ->
            let stmts = List.Ne.map self (nsepseq_to_nseq x) in
            block_of_statements stmts)
      in
      let translate_switch_case : CST.switch_case -> (_, _) AST.Switch.case = function
        | CST.Switch_case c ->
          let e = compile_expression c.expr in
          let s_opt = translate_statements_opt c.statements in
          Switch.Switch_case (e, s_opt)
        | CST.Switch_default_case c ->
          let s_opt = translate_statements_opt c.statements in
          Switch.Switch_default_case s_opt
      in
      List.Ne.map translate_switch_case s.cases
    in
    TODO_unify_in_cst.instr_as_stmt ~loc (i_switch { switchee; cases } ~loc)
  | SBreak s ->
    let _, loc = w_split s in
    TODO_unify_in_cst.instr_as_stmt ~loc (i_break ~loc)
  | SWhile s ->
    let s, loc = r_split s in
    let cond = compile_expression s.expr in
    let while_body =
      make_b
        ~loc:(Location.lift @@ CST.statement_to_region s.statement)
        (List.Ne.singleton (self s.statement))
    in
    TODO_unify_in_cst.instr_as_stmt ~loc (i_while { cond; block = while_body } ~loc)
  | SForOf s ->
    let CST.{ index_kind; index; expr; statement; _ }, loc = r_split s in
    let index_kind =
      match index_kind with
      | `Let _ -> `Let
      | `Const _ -> `Const
    in
    let index = TODO_do_in_parsing.var ~loc:(r_snd index) (r_fst index) in
    let expr = compile_expression expr in
    let for_stmt = self statement in
    TODO_unify_in_cst.instr_as_stmt
      ~loc
      (i_forof { index_kind; index; expr; for_stmt } ~loc)


(* ========================== EXPRESSIONS ================================== *)

and compile_expression : CST.expr -> AST.expr =
 fun e ->
  let self = compile_expression in
  let return e = e in
  let compile_bin_op (sign : AST.Operators.op)
      : string CST.wrap CST.bin_op CST.reg -> AST.expr
    =
   fun op ->
    let CST.{ op; arg1; arg2 }, _loc = r_split op in
    let _, loc = w_split op in
    e_binary_op
      ~loc
      AST.{ operator = Location.wrap ~loc sign; left = self arg1; right = self arg2 }
  in
  let compile_unary_op (sign : AST.Operators.op)
      : string CST.wrap CST.un_op CST.reg -> AST.expr
    =
   fun op ->
    let CST.{ op; arg }, _loc = r_split op in
    let _, loc = w_split op in
    e_unary_op ~loc AST.{ operator = Location.wrap ~loc sign; arg = self arg }
  in
  let translate_selection_jsligo : CST.selection -> _ AST.Selection.t =
   fun sel ->
    match sel with
    | FieldName name ->
      let name = r_fst (r_fst name).value in
      FieldName (Label.of_string name)
    | Component comp ->
      let comp = self (r_fst comp).inside in
      Component_expr comp
  in
  return
  @@
  match e with
  | EVar var ->
    let var, loc = r_split var in
    e_variable (TODO_do_in_parsing.var ~loc var) ~loc
  | EPar par ->
    let par, _loc = r_split par in
    self par.inside
  | EUnit the_unit ->
    let _, loc = r_split the_unit in
    return @@ e_unit ~loc
  | EBytes bytes_ ->
    let bytes_, loc = r_split bytes_ in
    let _s, b = bytes_ in
    e_bytes_hex b ~loc
  | EString str ->
    (match str with
    | String str ->
      let str, loc = r_split str in
      TODO_unify_in_cst.e_string str ~loc
    | Verbatim str ->
      let str, loc = r_split str in
      TODO_unify_in_cst.e_verbatim str ~loc)
  | EArith arth ->
    (match arth with
    | Add plus -> compile_bin_op PLUS plus
    | Sub minus -> compile_bin_op MINUS minus
    | Mult times -> compile_bin_op STAR times
    | Div slash -> compile_bin_op SLASH slash
    | Mod mod_ -> compile_bin_op PRCENT mod_
    | Neg minus -> compile_unary_op MINUS minus
    | Int i ->
      let (_, i), loc = r_split i in
      return @@ e_int_z ~loc i)
  | ELogic logic ->
    (match logic with
    | BoolExpr be ->
      (match be with
      | Or or_ -> compile_bin_op DPIPE or_
      | And and_ -> compile_bin_op DAMPERSAND and_
      | Not not_ -> compile_unary_op EX_MARK not_)
    | CompExpr ce ->
      (match ce with
      | Lt lt -> compile_bin_op LT lt
      | Leq le -> compile_bin_op LE le
      | Gt gt -> compile_bin_op GT gt
      | Geq ge -> compile_bin_op GE ge
      | Equal eq -> compile_bin_op DEQ eq
      | Neq ne -> compile_bin_op EQ_SLASH_EQ ne))
  | ECall call ->
    let (expr, args), loc = r_split call in
    let expr = self expr in
    let args : AST.expr list =
      match args with
      | Unit _ -> []
      | Multiple args -> List.map ~f:self (nsepseq_to_list (r_fst args).inside)
    in
    e_call expr args ~loc
  | EConstr constr ->
    let (constr, arg_opt), loc = r_split constr in
    let element = Option.map ~f:self arg_opt in
    TODO_unify_in_cst.nested_ctor_application ~loc constr element
  | EArray items ->
    let items, loc = r_split items in
    let items : expr AST.Array_repr.t =
      let translate_array_item : CST.array_item -> expr AST.Array_repr.item = function
        | Expr_entry e -> Expr_entry (self e)
        | Rest_entry e -> Rest_entry (self (r_fst e).expr)
      in
      Option.value_map items.inside ~default:[] ~f:(fun lst ->
          List.map ~f:translate_array_item (nsepseq_to_list lst))
    in
    e_array items ~loc
  | EObject obj ->
    let obj, loc = r_split obj in
    let props : _ AST.Object_.t =
      let translate_property : CST.property -> _ AST.Object_.property = function
        | Punned_property e ->
          let e = r_fst e in
          Punned_property (self e)
        | Property p ->
          let p = r_fst p in
          Property (self p.name, self p.value)
        | Property_rest p ->
          let p = r_fst p in
          Property_rest (self p.expr)
      in
      nseq_map translate_property @@ nsepseq_to_nseq obj.inside
    in
    e_object props ~loc
  | EProj proj ->
    let proj, loc = r_split proj in
    let expr = self proj.expr in
    let selection = translate_selection_jsligo proj.selection in
    e_proj { expr; selection } ~loc
  | EModA ma ->
    let ma, loc = r_split ma in
    let module_path =
      Simple_utils.List.Ne.singleton
      @@ TODO_do_in_parsing.mvar ~loc:(r_snd ma.module_name) (r_fst ma.module_name)
    in
    let field = self ma.field in
    e_module_open_in { module_path; field } ~loc
  | EFun f ->
    let f, loc = r_split f in
    let parameters =
      TODO_do_in_parsing.pattern_of_expr compile_type_expression f.parameters
    in
    (* this can be E_seq E_par *)
    let lhs_type = Option.map ~f:(compile_type_expression <@ snd) f.lhs_type in
    let body =
      let compile_body : CST.body -> (_, _) AST.Block_fun.fun_block = function
        | FunctionBody b ->
          let stmts, loc = r_split b in
          let stmts = nseq_map compile_statement @@ nsepseq_to_nseq stmts.inside in
          FunctionBody (make_b ~loc stmts)
        | ExpressionBody e -> ExpressionBody (self e)
      in
      compile_body f.body
    in
    e_block_fun { parameters; lhs_type; body } ~loc
  | EAnnot a ->
    let (e, _, te), loc = r_split a in
    let e = self e in
    let te = compile_type_expression te in
    e_annot (e, te) ~loc
  | ECodeInj ci ->
    let ci, loc = r_split ci in
    let language = r_fst ci.language in
    let code = self ci.code in
    e_rawcode { language; code } ~loc
  | ESeq seq ->
    let seq, loc = r_split seq in
    let seq = nseq_map self (nsepseq_to_nseq seq) in
    e_sequence ~loc (List.Ne.to_list seq)
  | EAssign (expr1, op, expr2) ->
    let op, loc = r_split op in
    let expr1 = self expr1 in
    let op =
      let translate_operator_jsligo : CST.operator -> AST.Assign_chainable.operator
        = function
        | Eq -> Eq
        | Assignment_operator aop ->
          Assignment_operator
            (match aop with
            | Times_eq -> Times_eq
            | Div_eq -> Div_eq
            | Min_eq -> Min_eq
            | Plus_eq -> Plus_eq
            | Mod_eq -> Mod_eq)
      in
      translate_operator_jsligo op
    in
    let expr2 = self expr2 in
    e_assign_chainable { expr1; op; expr2 } ~loc
  | ETernary e ->
    let e, loc = r_split e in
    let test = self e.condition in
    let ifso = self e.truthy in
    let ifnot = Some (self e.falsy) in
    e_cond { test; ifso; ifnot } ~loc


(* ========================== DECLARATIONS ================================= *)

let compile_toplevel_statement : CST.toplevel_statement -> AST.program_entry =
 fun s ->
  match s with
  | Directive d -> program_entry (PE_Preproc_directive d)
  | TopLevel (statement, _semicolon_opt) ->
    let statement = compile_statement statement in
    TODO_unify_in_cst.statement_to_program_entry statement


let compile_program : CST.t -> AST.program =
 fun t -> nseq_to_list t.statements |> List.map ~f:(fun a -> compile_toplevel_statement a)
