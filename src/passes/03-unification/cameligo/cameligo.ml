open Simple_utils.Utils
open Simple_utils.Trace
(* open Unification_shared.Errors *)

module CST = Cst.Cameligo
module AST = Ast_unified

module Helpers = Unification_shared.Helpers

open AST  (* Brings types and combinators functions *)

let r_split = Simple_utils.Location.r_split
let r_fst x = fst (r_split x)

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
      let compile_variant : CST.variant -> AST.variant = fun v ->
        let constr  = r_fst v.constr in
        let arg_opt = Option.apply (self <@ snd) v.arg in
        {constr; arg_opt}
      in
      List.Ne.map (compile_variant <@ r_fst) @@ nsepseq_to_nseq t.variants
    in
    t_sum variants ~loc ()
  )
  | TRecord t -> (
    let t, loc = r_split t in
    let t =
      let field_decls : CST.field_decl nseq = nseq_map r_fst @@ nsepseq_to_nseq t.ne_elements in
      let compile_field_decl : CST.field_decl -> AST.type_expr AST.field_assign = fun fd ->
        let name : string = r_fst fd.field_name in
        let expr : type_expr = self fd.field_type in
        {name; expr}
      in
      nseq_map compile_field_decl field_decls
    in
    t_recordcameligo t ~loc ()
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
    t_fun te1 te2 ~loc ()
  )
  | TPar t -> (
    let t, loc = r_split t in
    let t = self t.inside in
    t_par t ~loc ()
  )
  | TVar t -> (
    let t, loc = r_split t in
    t_var t ~loc ()
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
    let module_name = r_fst t.module_name in
    let field = self t.field in
    t_moda {module_name; field} ~loc ()
  )
  | TArg t -> (
    let t, loc = r_split t in
    let t = r_fst t.name in
    t_arg t ~loc ()
  )

(* ========================== PATTERNS ===================================== *)

let compile_pattern : CST.pattern -> AST.pattern = fun p ->
  match p with
  | _ -> failwith "TODO NP"

(* ========================== EXPRESSIONS ================================== *)

(* AST-agnostic definitions, these could be used on any pass *)
let translate_selection (sel : CST.selection) : Z.t AST.selection =
  match sel with
  | FieldName name -> let (name, _)      = r_split name in FieldName name
  | Component comp -> let ((_,index), _) = r_split comp in Component index

let translate_projection : CST.projection -> AST.projection = fun proj ->
  let name, loc = r_split proj.struct_name in
  let expr = e_uservar name ~loc () in
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
  let translate_field_assign (fa : CST.field_assign) : AST.expr AST.field_assign =
    let name, _ = r_split fa.field_name in
    let expr = self fa.field_expr in
    {name; expr}
  in
  let translate_path : CST.path -> AST.path = function
    | Name name  -> Name (r_fst name)
    | Path proj  -> Path (translate_projection @@ r_fst proj)
  in
  let translate_field_path_assignment : CST.field_path_assignment -> AST.field_path_assignment = fun fpa ->
    let field_path = translate_path fpa.field_path in
    let field_expr = self fpa.field_expr in
    {field_path; field_expr}
  in
  let translate_update : CST.update -> AST.update_cameligo = fun up ->
    let record_path = translate_path up.record in
    let updates = r_fst up.updates in
    let updates = nsepseq_to_nseq updates.ne_elements in
    let updates = nseq_map (translate_field_path_assignment <@ r_fst) updates in
    {record_path; updates}
  in
  let compile_type_params : CST.type_params CST.par CST.reg -> e_verbatim nseq =
    fun tp -> nseq_map r_fst @@ (r_fst tp).inside.type_vars
  in
  let compile_rhs_type : CST.colon * CST.type_expr -> type_expr =
    fun (_, t) -> compile_type_expression t
  in 
  return @@ match e with
  | EVar var -> (
      let name, loc = r_split var in 
      e_uservar name ~loc ()
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
      e_record fields ~loc ()
    )
  | EProj proj -> (
      let proj, loc = r_split proj in
      let proj = translate_projection proj in
      e_proj proj ~loc ()
    )
  | EModA ma -> (
      let ma, loc = r_split ma in
      let module_name = r_fst ma.module_name in
      let field = self ma.field in
      e_moda {module_name; field} ~loc ()
    )
  | EUpdate up -> (
      let up, loc = r_split up in
      let up = translate_update up in
      e_updatecameligo up ~loc ()
    )
  | EFun f -> (
      let f, loc = r_split f in
      let type_params = Option.apply compile_type_params f.type_params in
      let binders     = nseq_map compile_pattern f.binders in
      let rhs_type    = Option.apply compile_rhs_type f.rhs_type in
      let body        = self f.body in
      e_funcameligo {type_params; binders; rhs_type; body} ~loc ()
    )
  | EConstr constr -> (
      let (name, expr), loc = r_split constr in
      let name = r_fst name in
      let expr = Option.apply self expr in
      e_constr (name, expr) ~loc ()
    )
  | ECase case -> (
      let case, loc = r_split case in
      let expr : expression = self case.expr in
      let cases : expr case_clause nseq =
        let compile_case_clause : CST.expr CST.case_clause -> AST.expr AST.case_clause =
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
      let ifnot = Option.apply (self <@ snd) cond.ifnot in
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
      let {kwd_let=_; kwd_rec; binding; kwd_in=_; body; attributes=_} : CST.let_in = li in
      let {type_params; binders; rhs_type; eq=_; let_rhs} : CST.let_binding = binding in
      let is_rec      = match kwd_rec with Some _ -> true | None -> false in
      let type_params = Option.apply compile_type_params type_params in
      let binders     = nseq_map compile_pattern binders in
      let rhs_type    = Option.apply compile_rhs_type rhs_type in
      let let_rhs     = self let_rhs in
      let body        = self body in
      e_letincameligo {is_rec; type_params; binders; rhs_type; let_rhs; body} ~loc ()
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
    let d, loc = Helpers.translate_directive d in
    d_directive d ~loc ()
  )
  | Let e -> (
    let (_kwd_let, kwd_rec, e, _attr), loc = r_split e in
    let is_rec = match kwd_rec with None -> false | Some _ -> true in
    let type_params =
      let compile_type_params : CST.type_params CST.par CST.reg -> string nseq =
        fun tp -> nseq_map r_fst (r_fst tp).inside.type_vars
      in
      Option.apply compile_type_params e.type_params
    in
    let binders = nseq_map compile_pattern e.binders in
    let rhs_type = Option.apply (compile_type_expression <@ snd) e.rhs_type in
    let let_rhs = compile_expression ~raise e.let_rhs in
    d_let {is_rec; type_params; binders; rhs_type; let_rhs} ~loc ()
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
      Option.apply compile_params d.params
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

and compile_module ~raise : CST.t -> AST.module_ = fun t ->
  let () = ignore (t, raise) in { module_content = M_Dummy; location = Location.dummy }

let compile_program ~raise : CST.ast -> AST.program = fun t ->
  let declarations :                           CST.declaration  list = nseq_to_list t.decl in
  let declarations : (raise: ('e, 'w) raise -> AST.declaration) list = List.map ~f:(fun a ~raise -> compile_declaration ~raise a) declarations in
  let declarations :                           AST.declaration  list = Simple_utils.Trace.collect ~raise declarations in
  declarations

