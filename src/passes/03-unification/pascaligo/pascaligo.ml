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

let translate_attr_pascaligo : CST.Attr.t -> AST.attr_pascaligo = fun attr ->
  let key, value = attr in
  let value : string option = Option.apply (fun (CST.Attr.String s) ->  s) value in
  {key; value}

let extract_type_params : CST.type_params CST.chevrons CST.reg -> string nseq =
  fun tp -> nseq_map w_fst @@ nsepseq_to_nseq @@ (r_fst tp).inside

(* ========================== TYPES ======================================== *)

let rec compile_type_expression ~(raise: ('e, 'w) raise) : CST.type_expr -> AST.type_expr = fun te ->
  let self = compile_type_expression ~raise in
  match te with
  | T_App     t -> (
    let (te, ttuple), loc = r_split t in
    let constr = self te in
    let type_args : type_expr nseq =
      List.Ne.map self @@ nsepseq_to_nseq (r_fst ttuple).inside
    in
    t_apppascaligo {constr; type_args} ~loc ()
  )
  | T_Attr    t -> (
    let attr, te = t in
    let attr, loc = r_split attr in
    let attr = translate_attr_pascaligo attr in
    let te   = self te in
    t_attr attr te ~loc ()
  )
  | T_Cart    t -> (
    let (te, _, tes), loc = r_split t in
    let te = self te in
    let tes = List.Ne.map self @@ nsepseq_to_nseq tes in
    t_cart (te, tes) ~loc ()
  )
  | T_Fun t -> (
    let (te1, _, te2), loc = r_split t in
    let te1 = self te1 in
    let te2 = self te2 in
    t_fun te1 te2 ~loc ()
  )
  | T_Int     t -> (
    let (s, z), loc = w_split t in
    t_int s z ~loc ()
  )
  | T_ModPath t -> (
    let t, loc = r_split t in
    let module_path : string nseq = List.Ne.map w_fst @@ nsepseq_to_nseq t.module_path in
    let field : type_expr = self t.field in
    t_modpath {module_path; field} ~loc ()
  )
  | T_Par     t -> (
    let t, loc = r_split t in
    let t = self t.inside in
    t_par t ~loc ()
  )
  | T_Record  t -> (
    let t, loc = r_split t in
    let t : type_record_opt =
      let compile_field_decl : CST.field_decl -> AST.type_expr option AST.field_assign = fun fd ->
        let name : string = w_fst fd.field_name in
        let expr : type_expr option = Option.apply (self <@ snd) fd.field_type in
        {name; expr}
      in
      List.map ~f:(compile_field_decl <@ r_fst) @@ sepseq_to_list t.elements
    in
    t_recordpascaligo t ~loc ()
  )
  | T_String  t -> (
    let t, loc = w_split t in
    t_string t ~loc ()
  )
  | T_Sum     t -> (
    let t, loc = r_split t in
    let variants =
      let compile_variant : CST.variant -> AST.variant = fun v ->
        let constr  = w_fst v.ctor in
        let arg_opt = Option.apply (self <@ snd) v.ctor_args in
        {constr; arg_opt}
      in
      List.Ne.map (compile_variant <@ r_fst) @@ nsepseq_to_nseq t.variants
    in
    t_sum variants ~loc ()
  )
  | T_Var     t -> (
    let t, loc = w_split t in
    t_var t ~loc ()
  )

(* ========================== PATTERNS ===================================== *)

and compile_pattern ~(raise: ('e, 'w) raise) : CST.pattern -> AST.pattern = fun p ->
  let self = compile_pattern ~raise in
  match p with
  | P_App      p -> (
    let (p, pt_opt), loc = r_split p in
    let p : pattern = self p in
    let pt_opt : pattern nseq option = Option.apply (fun (v : CST.pattern CST.tuple) ->
      nseq_map self @@ nsepseq_to_nseq v.value.inside
    ) pt_opt
    in
    p_app p pt_opt ~loc ()
  )
  | P_Attr     p -> (
    let attr, ptrn = p in
    let attr, loc = r_split attr in
    let attr = translate_attr_pascaligo attr in
    let ptrn = self ptrn in
    p_attr attr ptrn ~loc ()
  )
  | P_Bytes    p -> (
    let (s, hex), loc = w_split p in
    let b = Hex.to_bytes hex in
    p_bytes s b ~loc ()
  )
  | P_Cons     p -> (
    let (p1, _, p2), loc = r_split p in
    let p1 = self p1 in
    let p2 = self p2 in
    p_list (AST.PCons (p1, p2)) ~loc ()
  )
  | P_Ctor     p -> (
    let s, loc = w_split p in
    p_constr s None ~loc ()
  )
  | P_Int      p -> (
    let (s, z), loc = w_split p in
    p_int s z ~loc ()
  )
  | P_List     p -> (
    let p, loc = r_split p in
    let p : pattern list = List.map ~f:self @@ sepseq_to_list p.elements in
    p_list (AST.PListComp p) ~loc ()
  )
  | P_ModPath  p -> (
    let p, loc = r_split p in
    let module_path : string nseq = List.Ne.map w_fst @@ nsepseq_to_nseq p.module_path in
    let field : ptrn = self p.field in
    p_modpath {module_path; field} ~loc ()
  )
  | P_Mutez    p -> (
    let (s, z), loc = w_split p in
    p_mutez s z ~loc ()
  )
  | P_Nat      p -> (
    let (s, z), loc = w_split p in
    p_nat s z ~loc ()
  )
  | P_Nil      p -> (
    let _, loc = w_split p in
    p_nil ~loc ()
  )
  | P_Par      p -> (
    let p, loc = r_split p in
    let p = self p.inside in
    p_par p ~loc ()
  )
  | P_Record   p -> (
    let p, loc = r_split p in
    let fields =
      let translate_field_assign : (CST.pattern, CST.pattern) CST.field -> (ptrn, ptrn) AST.field = function
      | Punned    p -> Punned (self p.pun)
      | Complete  c -> Complete (self c.field_lhs, self c.field_rhs)
      in
      List.map ~f:(translate_field_assign <@ r_fst) @@ sepseq_to_list p.elements
    in
    p_recordpascaligo fields ~loc ()
  )
  | P_String   p -> (
    let s, loc = w_split p in
    p_string s ~loc ()
  )
  | P_Tuple    p -> (
    let p, loc = r_split p in
    let p : ptrn nseq = List.Ne.map self @@ nsepseq_to_nseq p.inside in
    p_tuple p ~loc ()
  )
  | P_Typed    p -> (
    let p, loc = r_split p in
    let ptrn = self p.pattern in
    let te_opt = Some( compile_type_expression ~raise @@ snd p.type_annot ) in
    p_typed ptrn te_opt ~loc ()
  )
  | P_Var      p -> (
    let s, loc = w_split p in
    p_var s ~loc ()
  )
  | P_Verbatim p -> (
    let s, loc = w_split p in
    p_verbatim s ~loc ()
  )

(* ========================== INSTRUCTIONS ================================= *)

and compile_block ~(raise: ('e, 'w) raise) : CST.block -> AST.block = fun b ->
  List.Ne.map (compile_statement ~raise) @@ nsepseq_to_nseq b.statements

and compile_test_clause : raise:_ -> CST.test_clause -> AST.test_clause = fun ~raise c ->
  match c with
  | CST.ClauseInstr i -> AST.ClauseInstr (compile_instruction ~raise i)
  | CST.ClauseBlock b -> AST.ClauseBlock (compile_block ~raise @@ r_fst b)

and compile_case_clause : 'a 'b. raise:_ -> ('a -> 'b) -> 'a CST.case_clause -> 'b AST.case_clause = fun ~raise f c ->
  let pattern = compile_pattern ~raise c.pattern in
  let rhs     = f c.rhs in
  {pattern; rhs}

and compile_case : 'a 'b. raise:_ -> ('a -> 'b) -> 'a CST.case -> 'b AST.case = fun ~raise f c ->
  let expr = compile_expression ~raise c.expr in
  let cases = List.Ne.map (compile_case_clause ~raise f <@ r_fst) @@ nsepseq_to_nseq c.cases in
  {expr; cases}

and compile_cond : 'a 'b. raise:_ -> ('a -> 'b) -> 'a CST.conditional -> 'b AST.cond = fun ~raise f c ->
  let test  = compile_expression ~raise c.test in
  let ifso  = f c.if_so in
  let ifnot = Option.apply (f <@ snd) c.if_not in
  {test; ifso; ifnot}

and compile_for_map ~raise : CST.for_map -> AST.for_map = fun m ->
  let binding =
    let k, _, v = m.binding in
    w_fst k, w_fst v
  in
  let collection = compile_expression ~raise m.collection in
  let block      = compile_block ~raise @@ r_fst m.block in
  {binding; collection; block}

and compile_for_set_or_list ~raise : CST.for_set_or_list -> AST.for_set_or_list = fun s ->
  let var = w_fst s.var in
  let for_kind = match s.for_kind with
  | `Set  _ -> `Set
  | `List _ -> `List
  in
  let collection = compile_expression ~raise s.collection in
  let block      = compile_block ~raise @@ r_fst s.block in
  {var; for_kind; collection; block}

and compile_instruction ~(raise: ('e, 'w) raise) : CST.instruction -> AST.instruction = fun i ->
  let compile_expr = compile_expression ~raise in
  match i with
  | I_Assign i -> (
    let i, loc = r_split i in
    let lhs_expr = compile_expr i.lhs in
    let rhs_expr = compile_expr i.rhs in
    i_assign {lhs_expr; rhs_expr} ~loc ()
  )
  | I_Call i -> (
    let i, loc = r_split i in
    let f, args = i in
    let f = compile_expression ~raise f in
    let args : expr list = List.map ~f:compile_expr @@ sepseq_to_list (r_fst args).inside in
    i_call f args ~loc ()
  )
  | I_Case i -> (
    let i, loc = r_split i in
    let i : test_clause case = compile_case ~raise (compile_test_clause ~raise) i in
    i_case i ~loc ()
  )
  | I_Cond i -> (
    let i, loc = r_split i in
    let i : test_clause cond = compile_cond ~raise (compile_test_clause ~raise) i in
    i_cond i ~loc ()
  )
  | I_For i -> (
    let i, loc = r_split i in
    let index = w_fst i.index in
    let init  = compile_expr i.init in
    let bound = compile_expr i.bound in
    let step = Option.apply (compile_expr <@ snd) i.step in
    let block = compile_block ~raise @@ r_fst i.block in
    i_for {index; init; bound; step; block} ~loc ()
  )
  | I_ForIn  i -> (
    let i, loc =
      match i with
      | ForMap       m -> let m, loc = r_split m in ForMap (compile_for_map ~raise m), loc
      | ForSetOrList s -> let s, loc = r_split s in ForSetOrList (compile_for_set_or_list ~raise s), loc
    in
    i_forin i ~loc ()
  )
  | I_Patch  i -> (
    let i, loc = r_split i in
    let collection = compile_expr i.collection in
    let patch_kind =
      match i.patch_kind with
      | `Map    _ -> `Map
      | `Record _ -> `Record
      | `Set    _ -> `Set
    in
    let patch = compile_expr i.patch in
    i_patch {collection; patch_kind; patch} ~loc ()
  )
  | I_Remove i -> (
    let i, loc = r_split i in
    let item_expr = compile_expr i.item in
    let remove_kind =
      match i.remove_kind with
      | `Set _ -> `Set
      | `Map _ -> `Map
    in
    let collection = compile_expr i.collection in
    i_remove {item_expr; remove_kind; collection} ~loc ()
  )
  | I_Skip   i -> (
    let _, loc = w_split i in
    i_skip ~loc ()
  )
  | I_While  i -> (
    let i, loc = r_split i in
    let cond = compile_expr i.cond in
    let block = compile_block ~raise @@ r_fst i.block in
    i_while {cond; block} ~loc ()
  )

(* ========================== STATEMENTS ================================= *)

and compile_statement ~raise : CST.statement -> AST.statement = fun s ->
  let self = compile_statement ~raise in
  match s with
  | S_Attr (attr, stmt) -> (
    let attr, loc = r_split attr in
    let attr = translate_attr_pascaligo attr in
    let stmt = self stmt in
    s_attr (attr, stmt) ~loc ()
  )
  | S_Decl s -> (
    let s = compile_declaration ~raise s in
    let loc = s.location in
    s_decl s ~loc ()
  )
  | S_Instr s -> (
    let s = compile_instruction ~raise s in
    let loc = s.location in
    s_instr s ~loc ()
  )
  | S_VarDecl s -> (
    let s, loc = r_split s in
    let pattern     = compile_pattern ~raise s.pattern in
    let type_params = Option.apply extract_type_params s.type_params in
    let var_type    = Option.apply (compile_type_expression ~raise <@ snd) s.var_type in
    let init        = compile_expression ~raise s.init in
    s_vardecl {pattern; type_params; var_type; init} ~loc ()
  )

(* ========================== EXPRESSIONS ================================== *)

and extract_tuple : 'a. ('a, CST.comma) nsepseq CST.par CST.reg -> 'a nseq =
  fun t -> nsepseq_to_nseq (r_fst t).inside

and extract_key : 'a. 'a CST.brackets CST.reg -> 'a =
  fun k -> (r_fst k).inside

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
  let translate_selection : CST.selection -> AST.Z.t AST.selection = function
  | FieldName name -> let (name, _)      = w_split name in FieldName name
  | Component comp -> let ((_,index), _) = w_split comp in Component index
  in
  let translate_projection : CST.projection -> AST.projection = fun proj ->
    let expr       = self proj.record_or_tuple in
    let field_path = nseq_map translate_selection @@ nsepseq_to_nseq proj.field_path in
    {expr; field_path}
  in
  let compile_param_decl : CST.param_decl -> AST.param_decl = fun p ->
    let param_kind = match p.param_kind with `Var _ -> `Var | `Const _ -> `Const in
    let pattern    = compile_pattern ~raise p.pattern in
    let param_type = Option.apply (compile_type_expression ~raise <@ snd) p.param_type in
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
      let ret_type    = Option.apply (compile_type_expression ~raise <@ snd) f.ret_type in
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
      let case = compile_case ~raise self case in
      e_case case ~loc ()
    )
  | E_Typed annot -> (
      let annot, loc = r_split annot in
      let e, (_, te) = annot.inside in
      let e = self e in
      let te = compile_type_expression ~raise te in
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
        nseq_map (compile_statement ~raise) @@ nsepseq_to_nseq (r_fst be.block).statements
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
      let attr = translate_attr_pascaligo attr in
      let expr = self expr in
      e_attr (attr, expr) ~loc ()
    )

(* ========================== DECLARATIONS ================================= *)

and compile_declaration ~(raise: ('e, 'w) raise) : CST.declaration -> AST.declaration = fun decl ->
  let self = compile_declaration ~raise in
  let compile_type_params : CST.type_params CST.chevrons Region.reg -> string nseq =
    fun tp -> nseq_map w_fst @@ nsepseq_to_nseq (r_fst tp).inside
  in
  match decl with
  | D_Directive d -> (
    let loc = Simple_utils.Location.lift (Preprocessor.Directive.to_region d) in
    d_directive d ~loc ()
  )
  | D_Type d -> (
    let d, loc = r_split d in
    let name = w_fst d.name in
    let params = Option.apply (fun (tp : _ CST.par CST.reg) ->
      List.Ne.map w_fst @@ nsepseq_to_nseq (r_fst tp).inside
      ) d.params
    in
    let type_expr = compile_type_expression ~raise d.type_expr in
    d_type {name; params; type_expr} ~loc ()
  )
  | D_Const d -> (
    let d, loc = r_split d in
    (* TODO NP : Should we really use the 'let_binding' record for D_Const ? *)
    let is_rec = false in
    let type_params = Option.apply compile_type_params d.type_params in
    let binders = List.Ne.singleton @@ compile_pattern ~raise d.pattern in
    let rhs_type = Option.apply (compile_type_expression ~raise <@ snd) d.const_type in
    let let_rhs = compile_expression ~raise d.init in
    d_let {is_rec; type_params; binders; rhs_type; let_rhs} ~loc ()
  )
  | D_Attr d -> (
    let attr, decl = d in
    let attr, loc = r_split attr in
    let attr = translate_attr_pascaligo attr in
    let decl = self decl in
    d_attr (attr, decl) ~loc ()
  )
  | D_Fun d -> (
    let d, loc = r_split d in
    let is_rec = match d.kwd_recursive with Some _ -> true | None -> false in
    let fun_name = w_fst d.fun_name in
    let type_params = Option.apply compile_type_params d.type_params in
    let parameters =
      let compile_param_decl : CST.param_decl -> AST.param_decl = fun pd ->
        let param_kind = (
          match pd.param_kind with
          | `Const _ -> `Const
          | `Var   _ -> `Var
        ) in
        let pattern = compile_pattern ~raise pd.pattern in
        let param_type = Option.apply (compile_type_expression ~raise <@ snd) pd.param_type in
        {param_kind; pattern; param_type}
      in
      List.map ~f:(compile_param_decl <@ r_fst) @@ sepseq_to_list (r_fst d.parameters).inside
    in
    let ret_type = Option.apply (compile_type_expression ~raise <@ snd) d.ret_type in
    let return = compile_expression ~raise d.return in
    d_fun {is_rec; fun_name; type_params; parameters; ret_type; return} ~loc ()
  )
  | D_Module d -> (
    let d, loc = r_split d in
    let name = w_fst d.name in
    let mod_expr = compile_module ~raise d.module_expr in
    d_module {name; mod_expr} ~loc ()
  )

(* ========================== MODULES ===================================== *)

and compile_module ~(raise: ('e, 'w) raise) : CST.module_expr -> AST.module_ = fun m ->
  match m with
  | M_Body m -> (
    let m, loc = r_split m in
    let ds = nseq_map (compile_declaration ~raise) m.declarations in
    m_body ds ~loc ()
  )
  | M_Path m -> (
    let m, loc = r_split m in
    let module_path = nseq_map w_fst @@ nsepseq_to_nseq m.module_path in
    let field = w_fst m.field in
    m_path {module_path; field} ~loc ()
  )
  | M_Var  m -> (
    let s, loc = w_split m in
    m_var s ~loc ()
  )

(* ========================== PROGRAM ===================================== *)
let compile_program ~raise : CST.t -> AST.program = fun t ->
  let declarations :                           CST.declaration  list = nseq_to_list t.decl in
  let declarations : (raise: ('e, 'w) raise -> AST.declaration) list = List.map ~f:(fun a ~raise -> compile_declaration ~raise a) declarations in
  let declarations :                           AST.declaration  list = Simple_utils.Trace.collect ~raise declarations in
  declarations