open Simple_utils.Utils
open Simple_utils.Trace
open Unification_shared.Errors

module CST = Cst.Pascaligo
module AST = Ast_unified

module Helpers = Unification_shared.Helpers

(* Brings types and combinators functions *)
open AST

let r_split = Simple_utils.Location.r_split  (* TODO NP : Factor with cameligo into helpers *)
let r_fst x = fst (r_split x)
let w_split (x: 'a CST.Wrap.t) : 'a * Location.t =
  (x#payload, Location.lift x#region)
let w_fst x = fst (w_split x)

(* ========================== TYPES ======================================== *)

let compile_type_expression : CST.type_expr -> AST.type_expr = fun te ->
  let () = ignore te in
  t_dummy ()

(* ========================== PATTERNS ===================================== *)

let compile_pattern : CST.pattern -> AST.pattern = fun p ->
  let () = ignore p in
  P_Dummy

(* ========================== STATEMENTS ================================= *)

let compile_statement : CST.statement -> AST.statement = fun s ->
  let () = ignore s in
  s_dummy ()

(* ========================== EXPRESSIONS ================================== *)

let extract_tuple : 'a. ('a, CST.comma) nsepseq CST.par CST.reg -> 'a nseq =
  fun t -> nsepseq_to_nseq (r_fst t).inside

let extract_key : 'a. 'a CST.brackets CST.reg -> 'a =
  fun k -> (r_fst k).inside

let rec compile_expression ~raise : CST.expr -> AST.expr = fun e ->
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
  let translate_selection : CST.selection -> AST.Z.t AST.selection = function
  | FieldName name -> let (name, _)      = w_split name in FieldName name
  | Component comp -> let ((_,index), _) = w_split comp in Component index
  in
  let translate_projection : CST.projection -> AST.projection = fun proj ->
    let expr       = self proj.record_or_tuple in
    let field_path = nseq_map translate_selection @@ nsepseq_to_nseq proj.field_path in
    {expr; field_path}
  in
  let extract_type_params : CST.type_params CST.chevrons CST.reg -> string nseq =
    fun tp -> nseq_map w_fst @@ nsepseq_to_nseq @@ (r_fst tp).inside
  in
  let compile_param_decl : CST.param_decl -> AST.param_decl = fun p ->
    let param_kind = match p.param_kind with `Var _ -> `Var | `Const _ -> `Const in
    let pattern    = compile_pattern p.pattern in
    let param_type = Option.apply (compile_type_expression <@ snd) p.param_type in
    {param_kind; pattern; param_type}
  in
  return @@ match e with
  | E_Var var -> (
      let var, loc = w_split var in
      e_uservar var ~loc ()
    )
  | E_Par par -> (
      let par, loc = r_split par in
      let par = self par.inside in
      e_par par ~loc ()
    )
  | E_Bytes bytes_ -> (
      let bytes_, loc = w_split bytes_ in
      let (_s,b) = bytes_ in
      e_bytes_hex b ~loc
    )
  | E_Cat cat -> (
      let cat, loc = r_split cat in
      let e1 = self cat.arg1 in
      let e2 = self cat.arg2 in
      e_cat e1 e2 ~loc ()
    )
  | E_String str -> (
      let str, loc = w_split str in
      e_string str ~loc ()
    )
  | E_Verbatim str -> (
      let str, loc = w_split str in
      e_verbatim str ~loc ()
    )
  | E_Add plus   -> e_constant_of_bin_op_reg C_ADD plus
  | E_Sub minus  -> e_constant_of_bin_op_reg C_POLYMORPHIC_SUB minus
  | E_Mult times -> e_constant_of_bin_op_reg C_MUL times
  | E_Div slash  -> e_constant_of_bin_op_reg C_DIV slash
  | E_Mod mod_   -> e_constant_of_bin_op_reg C_MOD mod_
  | E_Neg minus  -> e_constant_of_un_op_reg C_NEG minus
  | E_Int i      -> let (_,i), loc = w_split i in e_int_z   ~loc i
  | E_Nat n      -> let (_,n), loc = w_split n in e_nat_z   ~loc n
  | E_Mutez m    -> let (_,m), loc = w_split m in e_mutez_z ~loc (Z.of_int64 m)
  | E_Or or_     -> e_constant_of_bin_op_reg C_OR  or_
  | E_And and_   -> e_constant_of_bin_op_reg C_AND and_
  | E_Not not_   -> e_constant_of_un_op_reg C_NOT not_
  | E_Lt lt      -> e_constant_of_bin_op_reg C_LT  lt
  | E_Leq le     -> e_constant_of_bin_op_reg C_LE  le
  | E_Gt gt      -> e_constant_of_bin_op_reg C_GT  gt
  | E_Geq ge     -> e_constant_of_bin_op_reg C_GE  ge
  | E_Equal eq   -> e_constant_of_bin_op_reg C_EQ  eq
  | E_Neq ne     -> e_constant_of_bin_op_reg C_NEQ ne
  | E_Call call -> (
      let (func, args), loc = r_split call in
      let func = self func in
      let args : expr nseq =
        let args, loc = r_split args in
        let compile_args : CST.expr list -> expr nseq = function
        | []      -> e_unit ~loc (), []
        | [e]     -> self e, []
        | e :: es -> nseq_map self (e, es)
        in
        compile_args @@ sepseq_to_list args.inside
      in
      e_call func args ~loc ()
  )
  | E_Tuple lst -> (
      let npseq, loc = r_split lst in
      let nseq = nseq_map self (nsepseq_to_nseq npseq.inside) in
      e_tuple nseq ~loc ()
    )
  | E_Record record -> (
      let record, loc = r_split record in
      let fields =
        let translate_field_assign : (CST.expr, CST.expr) CST.field -> (expr, expr) AST.field = function
        | Punned    p -> Punned (self p.pun)
        | Complete  c -> Complete (self c.field_lhs, self c.field_rhs)
        in
        List.map ~f:(translate_field_assign <@ r_fst) @@ sepseq_to_list record.elements
      in
      e_rec fields ~loc ()
    )
  | E_Proj proj -> (
      let proj, loc = r_split proj in
      let proj = translate_projection proj in
      e_proj proj ~loc ()
    )
  | E_ModPath ma -> (
      let ma, loc = r_split ma in
      let module_path = nseq_map w_fst @@ nsepseq_to_nseq ma.module_path in
      let field       = self ma.field in
      e_modpath {module_path; field} ~loc ()
    )
  | E_Update up -> (
      let up, loc = r_split up in
      let structure = self up.structure in
      let update    = self up.update in
      e_updatepascaligo {structure; update} ~loc ()
    )
  | E_Fun f -> (
      let f, loc = r_split f in
      let type_params = Option.apply extract_type_params f.type_params in
      let parameters  = List.map ~f:(compile_param_decl <@ r_fst)
        @@ sepseq_to_list @@ (r_fst f.parameters).inside
      in
      let ret_type    = Option.apply (compile_type_expression <@ snd) f.ret_type in
      let return      = self f.return in
      e_funpascaligo {type_params; parameters; ret_type; return} ~loc ()
    )
  | E_Ctor ctor -> (
      let ctor, loc = w_split ctor in
      e_constr (ctor, None) ~loc ()
    )
  | E_App app -> (
      let (func, args), loc = r_split app in
      let func = self func in
      let args = Option.apply (nseq_map self <@ extract_tuple) args in
      e_app (func, args) ~loc ()
    )
  | E_Case case -> (
      let case, loc = r_split case in
      let expr = self case.expr in
      let cases : case_clause nseq =
        let compile_case_clause : CST.expr CST.case_clause -> AST.case_clause =
          fun c -> { pattern = compile_pattern c.pattern; rhs = self c.rhs }
        in
        nseq_map (compile_case_clause <@ r_fst) @@ nsepseq_to_nseq case.cases
      in
      e_case {expr; cases} ~loc ()
    )
  | E_Typed annot -> (
      let annot, loc = r_split annot in
      let e, (_, te) = annot.inside in
      let e = self e in
      let te = compile_type_expression te in
      e_annot (e, te) ~loc ()
    )
  | E_Cond cond -> (
      let cond, loc = r_split cond in
      let test = self cond.test in
      let ifso = self cond.if_so in
      let ifnot = Option.apply (self <@ snd) cond.if_not in
      e_cond {test; ifso; ifnot} ~loc ()
    )
  | E_List list -> (
      let list, loc = r_split list in
      let elements = List.map ~f:self @@ sepseq_to_list list.elements in
      e_list elements ~loc ()
    )
  | E_Cons cons -> (
      let cons, loc = r_split cons in
      let arg1 = self cons.arg1 in
      let arg2 = self cons.arg2 in
      e_cons (arg1, arg2) ~loc ()
    )
  | E_Set set -> (
      let set, loc = r_split set in
      let elements = List.map ~f:self @@ sepseq_to_list set.elements in
      e_set elements ~loc ()
    )
  | E_SetMem sm -> (
      let (sm, loc) = r_split sm in
      let set  = self sm.set in
      let elem = self sm.element in
      e_constant ~loc (Const C_SET_MEM) [elem;set]
    )
  | E_MapLookup mlu -> (
      let mlu, loc = r_split mlu in
      let map  = self mlu.map in
      let keys = nseq_map (self <@ extract_key) mlu.keys in
      e_maplookup {map; keys} ~loc ()
    )
  | E_Map m -> (
      let m, loc = r_split m in
      let elements : (expr * expr) list =
        let compile_binding = fun (b : CST.binding) -> self b.key, self b.value in
        List.map ~f:(compile_binding <@ r_fst) @@ sepseq_to_list m.elements
      in
      e_map elements ~loc ()
    ) 
  | E_BigMap m -> (
    let m, loc = r_split m in
    let elements : (expr * expr) list =
      let compile_binding = fun (b : CST.binding) -> self b.key, self b.value in
      List.map ~f:(compile_binding <@ r_fst) @@ sepseq_to_list m.elements
    in
    e_map elements ~loc ()
    ) 
  | E_CodeInj ci -> (
      let ci, loc = r_split ci in
      let language = r_fst @@ r_fst ci.language in
      let code = self ci.code in
      e_rawcode {language; code} ~loc ()
    )
  | E_Block be -> (
      let be, loc = r_split be in
      let block : statement nseq =
        nseq_map compile_statement @@ nsepseq_to_nseq (r_fst be.block).statements
      in
      let expr  = self be.expr in
      e_block {block; expr} ~loc ()
    )
  | E_Nil nil -> (
      let (_,loc) = w_split nil in
      e_list [] ~loc ()
    )
  | E_Attr (attr, expr) -> (
      let attr, loc = r_split attr in
      let attr : AST.attr_pascaligo =
        let key, value = attr in
        let value : string option = (
          match value with
          | None -> None
          | Some (String s) -> Some s
        ) in
        {key; value}
      in
      let expr = self expr in
      e_attr (attr, expr) ~loc ()
    )

(* ========================== DECLARATIONS ================================= *)

and compile_declaration ~raise : CST.declaration -> AST.declaration = fun decl ->
  match decl with
  | D_Directive d -> (
    let d, loc = Helpers.translate_directive d in
    d_directive d ~loc ()
  )
  | D_Type d -> (
    let d, loc = r_split d in
    let name = w_fst d.name in
    let params = Option.apply (fun (tp : _ CST.par CST.reg) ->
      List.Ne.map w_fst @@ nsepseq_to_nseq (r_fst tp).inside
      ) d.params
    in
    let type_expr = compile_type_expression d.type_expr in
    d_type {name; params; type_expr} ~loc ()
  )
  | D_Const d -> (
    let d, loc = r_split d in
    (* TODO NP : Should we really use the 'let_binding' record for D_Const ? *)
    let is_rec = false in
    let type_params =
      let compile_type_params : CST.type_params CST.chevrons Region.reg -> string nseq =
        fun tp -> nseq_map w_fst @@ nsepseq_to_nseq (r_fst tp).inside
      in
      Option.apply compile_type_params d.type_params
    in
    let binders = List.Ne.singleton @@ compile_pattern d.pattern in
    let rhs_type = Option.apply (compile_type_expression <@ snd) d.const_type in
    let let_rhs = compile_expression ~raise d.init in
    d_let {is_rec; type_params; binders; rhs_type; let_rhs} ~loc ()
  )
  | _ -> raise.error @@ other_error "Declaration not supported yet." (* TODO NP : Add other declarations *)

let compile_program ~raise : CST.t -> AST.program = fun t ->
  let declarations :                           CST.declaration  list = nseq_to_list t.decl in
  let declarations : (raise: ('e, 'w) raise -> AST.declaration) list = List.map ~f:(fun a ~raise -> compile_declaration ~raise a) declarations in
  let declarations :                           AST.declaration  list = Simple_utils.Trace.collect ~raise declarations in
  declarations