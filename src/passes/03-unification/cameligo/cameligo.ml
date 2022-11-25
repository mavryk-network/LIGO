open Simple_utils.Utils
open Simple_utils.Trace
open Unification_shared.Helpers

module CST = Cst.Cameligo
module AST = Ast_unified

module Option = Simple_utils.Option

open AST  (* Brings types and combinators functions *)

module TODO_do_in_parsing = struct
  let _r_split = r_split (* could compute Location directly in Parser *)
  let var ~loc (var:string) = Ligo_prim.Value_var.of_input_var ~loc var
  let tvar ~loc (var:string) = Ligo_prim.Type_var.of_input_var ~loc var
  let mvar ~loc (var:string) = Ligo_prim.Module_var.of_input_var ~loc var
  let labelize x = Label.of_string x
end
module TODO_unify_in_cst = struct
  let conv_attr (attr:CST.attributes) : (AST.attribute * Location.t) list =
    List.map attr ~f:(fun attr_reg ->
      let (key,value_opt),loc = r_split attr_reg in
      let value : string option = Option.map ~f:(function String x -> x) value_opt in
      Temp_prim.Attribute.{ key ; value },loc
      )
  let attach_attr (attr:CST.attributes) (e:AST.expr) : AST.expr =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr,loc) -> e_attr ~loc (attr,e) ())
  let d_attach_attr (attr:CST.attributes) (e:AST.decl) : AST.decl =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr,loc) -> d_attr ~loc (attr,e) ())
  let t_attach_attr (attr:CST.attributes) (e:AST.type_expr) : AST.type_expr =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr,loc) -> t_attr ~loc attr e ())
  let compile_rows = Non_linear_rows.make
  let _compile_disc_rows = Non_linear_disc_rows.make
end

(* ========================== TYPES ======================================== *)

let rec compile_type_expression : CST.type_expr -> AST.type_expr = fun te ->
  let self = compile_type_expression in
  match te with
  | TProd t -> (
    let t, loc = r_split t in
    let t = List.Ne.map self @@ nsepseq_to_nseq t in
    t_prod t ~loc ()
  )
  | TSum t -> (
    let t, loc = r_split t in
    let variants =
      let compile_variant = fun CST.{constr ; arg ; attributes} ->
        ( TODO_do_in_parsing.labelize (r_fst constr)
        , Option.map ~f:(self <@ snd) arg
        , List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let lst = List.map (nsepseq_to_list t.variants) ~f:(compile_variant <@ r_fst) in
      TODO_unify_in_cst.compile_rows lst
    in
    t_sum_raw variants ~loc ()
  )
  | TRecord t -> (
    let CST.{attributes ; ne_elements ; compound=_ ; terminator=_ }, loc = r_split t in
    let fields =
      let field_decls : CST.field_decl nseq = nseq_map r_fst @@ nsepseq_to_nseq ne_elements in
      (* let open Ligo_prim in *)
      let compile_field_decl : int -> CST.field_decl -> AST.type_expr option Non_linear_rows.row =
       fun i {field_name ; field_type ; attributes; _ } ->
        let l = TODO_do_in_parsing.labelize (r_fst field_name) in
        let rows = Non_linear_rows.{
            decl_pos = i
          ; associated_type = Some (self field_type)
          ; attributes = List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst }
        in
        l,rows
      in
      List.mapi ~f:compile_field_decl (nseq_to_list field_decls)
    in
    TODO_unify_in_cst.t_attach_attr attributes (
      t_record_raw ~loc fields ()
    )
  )
  | TApp t -> (
    let t, loc = r_split t in
    let constr, args = t in
    let constr : string = r_fst constr in
    let type_args : type_expr nseq =
      match args with
      | CST.CArg te       -> List.Ne.singleton (self te)
      | CST.CArgTuple tes -> List.Ne.map self @@ nsepseq_to_nseq (r_fst tes).inside
    in
    t_app {constr; type_args} ~loc ()
  )
  | TFun t -> (
    let (te1, _, te2), loc = r_split t in
    let te1 = self te1 in
    let te2 = self te2 in
    t_fun ~loc (te1, te2) ()
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
  | TArg t -> (
    let t, loc = r_split t in
    let t = r_fst t.name in
    t_arg t ~loc ()
  )

(* ========================== PATTERNS ===================================== *)

let rec compile_pattern : CST.pattern -> AST.pattern = fun p ->
  let self = compile_pattern in
  let pat ~loc p = Location.wrap ~loc p in
  match p with
  | PConstr p -> (
    let (ctor, p_opt), loc = r_split p in
    let ctor = TODO_do_in_parsing.labelize ctor.value in
    let p_opt = Option.map ~f:self p_opt in
    pat ~loc (P_variant (ctor,p_opt))
  )
  | PUnit p -> (
    let _, loc = r_split p in
    pat ~loc (P_unit)
  )
  | PVar p -> (
    let p, loc = r_split p in
    let v  = r_fst p.variable in
    pat ~loc (P_var (TODO_do_in_parsing.var ~loc v))
  )
  | PInt p -> (
    let (_s, z), loc = r_split p in
    pat ~loc (P_literal (Literal_int z))
  )
  | PNat p -> (
    let (_s, z), loc = r_split p in
    pat ~loc (P_literal (Literal_nat z))
  )
  | PBytes p -> (
    let (_s, hex), loc = r_split p in
    let bytes_ = Hex.to_bytes hex in
    pat ~loc (P_literal (Literal_bytes bytes_))
  )
  | PString p -> (
    let s, loc = r_split p in
    pat ~loc (P_literal (Literal_string (Simple_utils.Ligo_string.standard s)))
  )
  | PVerbatim p -> (
    let s, loc = r_split p in
    pat ~loc (P_literal (Literal_string (Simple_utils.Ligo_string.verbatim s)))
  )
  | PList p -> (
    let p, loc = match p with
    | CST.PListComp p ->
      let p, loc = r_split p in
      let ps = List.map ~f:self (sepseq_to_list p.elements) in
      List ps, loc
    | CST.PCons p ->
      let (p1, _, p2), loc = r_split p in
      let p1 = self p1 in
      let p2 = self p2 in
      Cons (p1, p2), loc
    in
    pat ~loc (P_list p)
  )
  | PTuple p -> (
    let p, loc = r_split p in
    let p = List.map ~f:self (nsepseq_to_list p) in
    pat ~loc (P_tuple p)
  )
  | PPar p -> (
    self (r_fst p).inside
  )
  | PRecord p -> (
    let p, loc = r_split p in
    let p =
      let compile_field_pattern : CST.field_pattern -> (Label.t, AST.pattern) field = fun fp -> 
        Complete (TODO_do_in_parsing.labelize (r_fst fp.field_name), self fp.pattern)
      in
      List.map ~f:(compile_field_pattern <@ r_fst) (nsepseq_to_list p.ne_elements)
    in
    pat ~loc (P_pun_record p)
  )
  | PTyped    p -> (
    let p, loc = r_split p in
    let ty = compile_type_expression p.type_expr in
    let p = self p.pattern in
    pat ~loc (P_typed (ty,p))
  )


(* ========================== EXPRESSIONS ================================== *)

let translate_selection (sel : CST.selection) : Z.t AST.selection =
  match sel with
  | FieldName name -> let (name, _)      = r_split name in FieldName name
  | Component comp -> let ((_,index), _) = r_split comp in Component index

let translate_projection : CST.projection -> AST.projection = fun proj ->
  let name, loc = r_split proj.struct_name in
  let expr = e_variable (TODO_do_in_parsing.var ~loc name) ~loc () in
  let field_path = nseq_map translate_selection @@ nsepseq_to_nseq proj.field_path in
  {expr; field_path}

let rec compile_expression ~raise : CST.expr -> AST.expr = fun e ->
  let self = compile_expression ~raise in
  let return e = e in
  (* TODO : This function is common to all CST's maybe there's better refactoring for this *)      
  let compile_bin_op (op_type : Ligo_prim.Constant.constant') (op : _ CST.bin_op CST.reg) =
    let op, loc = r_split op in
    let a = self op.arg1 in
    let b = self op.arg2 in
    e_constant ~loc (Const op_type) [a; b]
  in
  let compile_un_op (op_type : Ligo_prim.Constant.constant') (op : _ CST.un_op CST.reg) =
    let op, loc = r_split op in
    let arg = self op.arg in
    e_constant ~loc (Const op_type) [arg]
  in
  let translate_field_assign (fa : CST.field_assign) : (string, AST.expr) AST.field =
    match fa with
    | CST.Property fap -> (
      let s = r_fst fap.field_name in
      let e = self fap.field_expr in
      AST.Complete (s, e)
    )
    | Punned_property fn -> (
      let s = r_fst fn in
      AST.Punned s
    )
  in
  let translate_path : CST.path -> AST.path = function
    | Name name  -> Name (r_fst name)
    | Path proj  -> Path (translate_projection @@ r_fst proj)
  in
  let translate_field_path_assignment : CST.field_path_assignment -> (path, expr) field =
    function
    | CST.Path_property p -> (
      let path = translate_path p.field_path in
      let expr = self p.field_expr in
      AST.Complete (path, expr)
    )
    | CST.Path_punned_property p -> (
      let name = r_fst p in
      AST.Punned (AST.Name name)
    )
  in
  let translate_update : CST.update -> AST.update_cameligo = fun up ->
    let record_path = translate_path up.record in
    let updates = r_fst up.updates in
    let updates = nsepseq_to_nseq updates.ne_elements in
    let updates = nseq_map (translate_field_path_assignment <@ r_fst) updates in
    {record_path; updates}
  in
  let compile_type_params : CST.type_params CST.par CST.reg -> Ligo_prim.Type_var.t nseq =
    fun tp ->
      let lst = nseq_map
        (fun (x:CST.variable) -> TODO_do_in_parsing.tvar ~loc:(Location.lift x.region) x.value)
        (r_fst tp).inside.type_vars
      in 
      lst
  in
  let compile_rhs_type : CST.colon * CST.type_expr -> type_expr =
    fun (_, t) -> compile_type_expression t
  in 
  return @@ match e with
  | EVar var -> (
      let name, loc = r_split var in 
      e_variable (TODO_do_in_parsing.var ~loc name) ~loc ()
    )
  (* we keep parenthesis so that the backward pass which add parenthesis is done only once for all syntaxes (?) *)
  | EPar par -> (
      let par, loc = r_split par in
      e_par (self par.inside) ~loc ()
    )
  | EUnit unit_ -> (
      let _, loc = r_split unit_ in
      e_unit ~loc ()
  ) 
  | EBytes bytes_ -> (
      let bytes_, loc = r_split bytes_ in
      let (_s,b) = bytes_ in
      e_bytes_hex b ~loc
    )
  | EString str -> (
    match str with
    | Cat c ->
      let op,loc = r_split c in
      let e1 = self op.arg1 in
      let e2 = self op.arg2 in
      e_cat e1 e2 ~loc ()
    | String str ->
      let str, loc = r_split str in
      e_string str ~loc ()
    | Verbatim str ->
      let str, loc = r_split str in
      e_verbatim str ~loc ()
    )
  | EArith arth -> (
    match arth with
    | Add   plus  -> compile_bin_op C_ADD plus
    | Sub   minus -> compile_bin_op C_POLYMORPHIC_SUB minus
    | Mult  times -> compile_bin_op C_MUL times
    | Div   slash -> compile_bin_op C_DIV slash
    | Mod   mod_  -> compile_bin_op C_MOD mod_
    | Land  land_ -> compile_bin_op C_AND land_
    | Lor   lor_  -> compile_bin_op C_OR lor_
    | Lxor  lxor_ -> compile_bin_op C_XOR lxor_
    | Lsl   lsl_  -> compile_bin_op C_LSL lsl_
    | Lsr   lsr_  -> compile_bin_op C_LSR lsr_
    | Neg   minus -> compile_un_op C_NEG minus
    | Int   i     -> let (_,i), loc = r_split i in e_int_z   ~loc i
    | Nat   n     -> let (_,n), loc = r_split n in e_nat_z   ~loc n
    | Mutez m     -> let (_,m), loc = r_split m in e_mutez_z ~loc (Z.of_int64 m)
    )
  | ELogic logic -> (
    match logic with
    | BoolExpr be -> (
      match be with
      | Or  or_  -> compile_bin_op C_OR  or_
      | And and_ -> compile_bin_op C_AND and_
      | Not not_ -> compile_un_op  C_NOT not_
      )
    | CompExpr ce -> (
      match ce with
      | Lt    lt -> compile_bin_op C_LT  lt
      | Leq   le -> compile_bin_op C_LE  le
      | Gt    gt -> compile_bin_op C_GT  gt
      | Geq   ge -> compile_bin_op C_GE  ge
      | Equal eq -> compile_bin_op C_EQ  eq
      | Neq   ne -> compile_bin_op C_NEQ ne
      )
    )
  | ERevApp ra -> (
      let ra, loc = r_split ra in
      let x = self ra.arg1 in
      let f = self ra.arg2 in
      e_revapp {x; f} ~loc ()
    )
  | ECall call -> (
      let (func, args), loc = r_split call in
      let func = self func in
      let args = nseq_map self args in
      e_call func args ~loc ()
    )
  | ETuple lst -> (
      let npseq, loc = r_split lst in
      let nseq = nseq_map self (nsepseq_to_nseq npseq) in
      e_tuple nseq ~loc ()
    )
  | ERecord record -> (
      let record, loc = r_split record in
      let fields =
        record.ne_elements
        |> nsepseq_to_nseq
        |> nseq_map (translate_field_assign <@ r_fst)
      in
      e_recordcameligo fields ~loc ()
    )
  | EProj proj -> (
      let proj, loc = r_split proj in
      let proj = translate_projection proj in
      e_proj proj ~loc ()
    )
  | EModA ma -> (
      let ma, loc = r_split ma in
      let module_path = r_fst ma.module_name in
      let field = self ma.field in
      e_moda {module_path; field} ~loc ()
    )
  | EUpdate up -> (
      let up, loc = r_split up in
      let up = translate_update up in
      e_updatecameligo up ~loc ()
    )
  | EFun f -> (
      let f, loc = r_split f in
      let type_params = Option.map ~f:compile_type_params f.type_params in
      let binders     = nseq_map compile_pattern f.binders in
      let rhs_type    = Option.map ~f:compile_rhs_type f.rhs_type in
      let body        = self f.body in
      e_funcameligo {type_params; binders; rhs_type; body} ~loc ()
    )
  | EConstr constr -> (
      let (name, expr), loc = r_split constr in
      let name = r_fst name in
      let expr = Option.map ~f:self expr in
      e_constr (name, expr) ~loc ()
    )
  | ECase case -> (
      let case, loc = r_split case in
      let expr : expression = self case.expr in
      let cases : (pattern,expr) AST.Case.clause nseq =
        let compile_case_clause : CST.expr CST.case_clause -> (pattern,expr) Case.clause =
          fun c -> { pattern = compile_pattern c.pattern; rhs = self c.rhs }
        in
        nseq_map (compile_case_clause <@ r_fst) @@ nsepseq_to_nseq @@ r_fst case.cases
      in
      e_case {expr; cases} ~loc ()
    )
  | EAnnot annot -> (
      let annot, loc = r_split annot in
      let (e, _, te) = annot.inside in
      let e = self e in
      let te = compile_type_expression te in
      e_annot (e, te) ~loc ()
    )
  | ECond cond -> (
      let cond, loc = r_split cond in
      let test = self cond.test in
      let ifso = self cond.ifso in
      let ifnot = Option.map ~f:(self <@ snd) cond.ifnot in
      e_cond {test; ifso; ifnot} ~loc ()
    )
  | EList list -> (
      match list with
      | ECons cons ->
        let cons, loc = r_split cons in
        let arg1 = self cons.arg1 in
        let arg2 = self cons.arg2 in
        e_cons (arg1, arg2) ~loc ()
      | EListComp listcomp ->
        let list, loc = r_split listcomp in
        let elements = List.map ~f:self @@ sepseq_to_list list.elements in
        e_list elements ~loc ()
    )
  | ELetIn li -> (
      let li, loc = r_split li in
      let {kwd_let=_; kwd_rec; binding; kwd_in=_; body; attributes} : CST.let_in = li in
      let {type_params; binders; rhs_type; eq=_; let_rhs} : CST.let_binding = binding in
      let is_rec      = match kwd_rec with Some _ -> true | None -> false in
      let type_params = Option.map ~f:compile_type_params type_params in
      let pattern     = nseq_map compile_pattern binders in
      let rhs_type    = Option.map ~f:compile_rhs_type rhs_type in
      let let_rhs     = self let_rhs in
      let body        = self body in
      TODO_unify_in_cst.attach_attr attributes (
        e_let_in {is_rec; type_params; pattern; rhs_type; let_rhs; body} ~loc ()
      )
    )
  | ETypeIn ti -> (
      let ti, loc = r_split ti in
      let {type_decl={name;type_expr;_}; kwd_in=_; body} : CST.type_in = ti in
      let type_binder = r_fst name in
      let rhs = compile_type_expression type_expr in
      let body = self body in
      e_typein {type_binder; rhs; body} ~loc ()
    )
  | EModIn mi -> (
      let mi, loc = r_split mi in
      let {mod_decl={name;module_; _}; kwd_in=_; body} : CST.mod_in = mi in
      let module_name = r_fst name in
      let rhs = compile_module ~raise module_ in
      let body = self body in
      e_modin {module_name; rhs; body} ~loc ()
    )
  | EModAlias ma -> (
      let ma, loc = r_split ma in
      let {mod_alias={alias;binders;_}; kwd_in=_; body} : CST.mod_alias = ma in
      let module_name = r_fst alias in
      let binders = nseq_map r_fst @@ nsepseq_to_nseq binders in
      let body = self body in
      e_modalias {module_name; binders; body} ~loc ()
  )
  | ECodeInj ci -> (
      let ci, loc = r_split ci in
      let language = r_fst @@ r_fst ci.language in
      let code = self ci.code in
      e_rawcode {language; code} ~loc ()
    )
  | ESeq seq -> (
      let seq, loc = r_split seq in
      let seq = List.map ~f:self @@ sepseq_to_list seq.elements in
      e_seq seq ~loc ()
    )

and compile_declaration ~raise : CST.declaration -> AST.declaration = fun decl ->
  match decl with
  | Directive d -> (
    let loc = Simple_utils.Location.lift (Preprocessor.Directive.to_region d) in
    d_directive d ~loc ()
  )
  | Let e -> (
    let (_kwd_let, kwd_rec, e, attributes), loc = r_split e in
    let is_rec = match kwd_rec with None -> false | Some _ -> true in
    let type_params =
      let compile_type_params : CST.type_params CST.par CST.reg -> AST.Ty_variable.t nseq =
        fun tp -> nseq_map (fun x -> TODO_do_in_parsing.tvar ~loc:(r_snd x) (r_fst x)) (r_fst tp).inside.type_vars
      in
      Option.map ~f:compile_type_params e.type_params
    in
    let pattern = nseq_map compile_pattern e.binders in
    let rhs_type = Option.map ~f:(compile_type_expression <@ snd) e.rhs_type in
    let let_rhs = compile_expression ~raise e.let_rhs in
    TODO_unify_in_cst.d_attach_attr attributes (
      d_let {is_rec; type_params; pattern; rhs_type; let_rhs ; body = ()} ~loc ()
    )
  )
  | TypeDecl d -> (
    let d, loc = r_split d in
    let name : string = r_fst d.name in
    let params : string nseq option =
      let compile_params : CST.type_vars -> string nseq = fun p ->
        let p : CST.type_var nseq =
          match p with
          | QParam x      -> List.Ne.singleton (r_fst x)
          | QParamTuple x -> nseq_map r_fst @@ nsepseq_to_nseq x.value.inside
        in
        nseq_map (fun (p : CST.type_var) -> r_fst p.name) p
      in
      Option.map ~f:compile_params d.params
    in
    let type_expr = compile_type_expression d.type_expr in
    d_type {name; params; type_expr} ~loc ()
  )
  | ModuleDecl d -> (
    let d, loc = r_split d in
    let name : string = r_fst d.name in
    let mod_expr : module_ = compile_module ~raise d.module_ in
    d_module {name; mod_expr} ~loc ()
  )
  | ModuleAlias d -> (
    let d, loc = r_split d in
    let alias : string = r_fst d.alias in
    let binders : string nseq = nseq_map r_fst @@ nsepseq_to_nseq d.binders in
    d_modulealias {alias; binders} ~loc ()
  )

and compile_module ~raise : CST.t -> AST.module_ = fun m ->
  let ds : AST.declaration nseq = nseq_map (compile_declaration ~raise) m.decl in
  let loc =
    (* The region of the module is the union of all its declarations' regions *)
    let locations = nseq_map (fun (d : AST.declaration) -> d.location) ds in
    List.Ne.fold_left locations ~init:Location.dummy ~f:Location.cover
  in
  m_body ds ~loc ()

let compile_program ~raise : CST.ast -> AST.program = fun t ->
  let declarations = nseq_to_list t.decl in
  let declarations = List.map ~f:(fun a ~raise -> compile_declaration ~raise a) declarations in
  let declarations = Simple_utils.Trace.collect ~raise declarations in
  List.map ~f:(fun x -> P_Declaration x) declarations

