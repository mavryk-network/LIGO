open Simple_utils.Utils
open Simple_utils.Trace
(* open Unification_shared.Errors *)

module CST = Cst.Jsligo
module AST = Ast_unified

module Helpers = Unification_shared.Helpers
module Option = Simple_utils.Option

(* Brings types and combinators functions *)
open AST

let r_split = Simple_utils.Location.r_split  (* TODO NP : Factor with cameligo into helpers *)
let r_fst x = fst (r_split x)
let w_split (x: 'a CST.Wrap.t) : 'a * Location.t =
  (x#payload, Location.lift x#region)

(* ========================== TYPES ======================================== *)

let rec compile_type_expression : CST.type_expr -> AST.type_expr = fun te ->
  match te with
  | _ -> failwith "TODO : Missing case"

(* ========================== PATTERNS ===================================== *)

and compile_pattern : CST.pattern -> AST.pattern = fun p ->
  let () = ignore p in
  P_Dummy

(* ========================== STATEMENTS ================================= *)

and compile_statement ~raise : CST.statement -> AST.statement = fun s ->
  let self = compile_statement ~raise in
  let () = ignore (self, raise) in
  let extract_type_vars : CST.type_vars -> string nseq = fun tv ->
    List.Ne.map r_fst @@ nsepseq_to_nseq (r_fst tv).inside
  in
  let compile_val_binding : CST.val_binding -> AST.let_binding = fun b ->
    let is_rec = false in
    let binders = List.Ne.singleton @@ compile_pattern b.binders in
    let type_params = Option.map b.type_params ~f:(fun (tp : CST.type_generics) ->
      List.Ne.map r_fst @@ nsepseq_to_nseq (r_fst tp).inside)
    in
    let rhs_type = Option.map ~f:(compile_type_expression <@ snd) b.lhs_type in
    let let_rhs = compile_expression ~raise b.expr in
    {is_rec; type_params; binders; rhs_type; let_rhs}
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
    let s, loc = r_split s in
    let bindings = List.Ne.map (compile_val_binding <@ r_fst) @@ nsepseq_to_nseq s.bindings in
    s_let bindings ~loc ()
  )
  | SConst s -> (
    let s, loc = r_split s in
    let bindings = List.Ne.map (compile_val_binding <@ r_fst) @@ nsepseq_to_nseq s.bindings in
    s_const bindings ~loc ()
  )
  | SType s -> (
    let s, loc = r_split s in
    let name      = r_fst s.name in
    let params    = Option.map ~f:extract_type_vars s.params in
    let type_expr = compile_type_expression s.type_expr in
    s_type {name; params; type_expr} ~loc ()
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
    let (_, module_name, statements, _) = s in
    let module_name       = r_fst module_name in
    let namespace_content =
      List.Ne.map self @@ nsepseq_to_nseq (r_fst statements).inside
    in
    s_namespace {module_name; namespace_content} ~loc ()
  )
  | SExport s -> (
    let (_, s), loc = r_split s in
    let s = self s in
    s_export s ~loc ()
  )
  | SImport s -> (
    let s, loc = r_split s in
    let alias       = r_fst s.alias in
    let module_path = List.Ne.map r_fst @@ nsepseq_to_nseq s.module_path in
    s_import {alias; module_path} ~loc ()
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

and compile_expression ~raise : CST.expr -> AST.expr = fun e ->
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
    e_uservar var ~loc ()
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
    e_calljsligo expr args ~loc ()
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
    let module_name = r_fst ma.module_name in
    let field = self ma.field in
    e_moda {module_name; field} ~loc ()
  )
  | EFun f -> (
    let f, loc = r_split f in
    let parameters = self f.parameters in
    let lhs_type   = Option.map ~f:(compile_type_expression <@ snd) f.lhs_type in
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
    let te = compile_type_expression te in
    e_annotjsligo (e, te) ~loc ()
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

let compile_toplevel_statement ~raise : CST.toplevel_statement -> AST.declaration = fun s -> 
  match s with
  | Directive d -> (
    let d, loc = Helpers.translate_directive d in
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
