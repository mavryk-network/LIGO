open Simple_utils.Utils
open Simple_utils.Trace
open Unification_shared.Helpers

module CST = Cst.Pascaligo
module AST = Ast_unified

module Option = Simple_utils.Option
module Region  = Simple_utils.Region

open AST  (* Brings types and combinators functions *)

module type X = module type of AST.Combinators

module TODO_do_in_parsing = struct
  let r_split = r_split (* could compute Location directly in Parser *)
  let _lift = Location.lift
  let var ~loc var = Ligo_prim.Value_var.of_input_var ~loc var
  let tvar ~loc var = Ligo_prim.Type_var.of_input_var ~loc var
  let mvar ~loc var = Ligo_prim.Module_var.of_input_var ~loc var
  let need_rework _ y = (*most probably a node that we should avoid, maybe ?*) failwith y
  let six_to_z x = Z.of_int64 x (* not sure who's right ? *)
  let rec compile_pattern_record_lhs (p:CST.pattern) : CST.variable =
    match p with
    | CST.P_Par x -> compile_pattern_record_lhs x.value.inside
    | CST.P_Var x -> x
    | _ -> failwith "CST.field_pattern is wrong ?  should be '(string, pattern) field' ?"
  let weird_attributes _ =
    (* I don't know what to do with those attributes *)
    ()
  let extract_arg_from_app self p =
    (* other syntax do not have the App thing ?
       also have an optional 'carg' ?
    *)
    let ((constr,p_opt), loc) = r_split p in
    let pat ~loc p = Location.wrap ~loc p in
    let rec get_ctor : CST.pattern -> string option = function
      | P_Par x -> get_ctor x.value.inside
      | P_Ctor x -> Some x#payload
      | _ -> None
    in
    match get_ctor constr with
    | Some "Unit" -> pat ~loc P_unit
    | Some label ->
      let carg = match p_opt with
        | Some p -> self (CST.P_Tuple p)
        | None -> pat ~loc P_unit
      in
      pat ~loc @@ P_variant (Label label, Some carg)
    | None -> failwith "impossible ?"
end
module TODO_unify_in_cst = struct
  let compile_rows = Non_linear_rows.make
  let vardecl_as_decl ~loc x =
    (* https://tezos-dev.slack.com/archives/GMHV0U3Q9/p1669146559008189 *)
    s_decl ~loc (d_var ~loc x ()) ()
  let for_in compile_for_map compile_for_set_or_list i =
    (* could be done directyu in Parser.mly *)
    let open AST.For_collection in
    match i with
    | CST.ForMap       m -> let m, loc = r_split m in ForMap (compile_for_map m), loc
    | CST.ForSetOrList s -> let s, loc = r_split s in ForSetOrList (compile_for_set_or_list s), loc
end

let translate_attr_pascaligo : CST.Attr.t -> AST.attribute = fun attr ->
  let key, value = attr in
  let value : string option = Option.map ~f:(fun (CST.Attr.String s) ->  s) value in
  {key; value}

let extract_type_params : CST.type_params CST.chevrons CST.reg -> Ligo_prim.Type_var.t nseq =
  fun tp ->
    nseq_map
      (fun x ->
        let x,loc = w_split x in
        TODO_do_in_parsing.tvar ~loc x
      )
      (nsepseq_to_nseq @@ (r_fst tp).inside)

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
    t_app {constr; type_args} ~loc ()
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
    let hd,tl = List.Ne.map self @@ nsepseq_to_nseq tes in
    t_prod (te, hd::tl) ~loc ()
  )
  | T_Fun t -> (
    let (te1, _, te2), loc = r_split t in
    let te1 = self te1 in
    let te2 = self te2 in
    t_fun ~loc (te1, te2) ()
  )
  | T_Int     t -> (
    let (s, z), loc = w_split t in
    t_int s z ~loc ()
  )
  | T_ModPath t -> (
    let t, loc = r_split t in
    let module_path = List.Ne.map
      (fun t ->
        let x, loc = w_split t in
        TODO_do_in_parsing.mvar ~loc x)
        (nsepseq_to_nseq t.module_path)
    in
    let field : type_expr = self t.field in
    t_modpath {module_path; field} ~loc ()
  )
  | T_Par     t -> (
    self (r_fst t).inside
  )
  | T_Record t -> (
    let t, loc = r_split t in
    let fields =
      let destruct =
       fun CST.{field_type ; field_name ; attributes } ->
        ( Label.of_string (w_fst field_name)
        , Option.map ~f:(self <@ snd) field_type
        , List.map attributes ~f:(translate_attr_pascaligo <@ r_fst) )
      in
      let lst = List.map ~f:(destruct <@ r_fst) @@ sepseq_to_list t.elements in
      TODO_unify_in_cst.compile_rows lst
    in
    t_record_raw fields ~loc ()
  )
  | T_String  t -> (
    let t, loc = w_split t in
    t_string t ~loc ()
  )
  | T_Sum t -> (
    let t, loc = r_split t in
    let variants =
      let destruct =
       fun CST.{ctor ; ctor_args ; attributes} ->
        ( Label.of_string (w_fst ctor)
        , Option.map ~f:(self <@ snd) ctor_args
        , List.map attributes ~f:(translate_attr_pascaligo <@ r_fst) )
      in
      let lst = List.map ~f:(destruct <@ r_fst) (nsepseq_to_list t.variants) in
      TODO_unify_in_cst.compile_rows lst
    in
    t_sum_raw variants ~loc ()
  )
  | T_Var t -> (
    let t, loc = w_split t in
    t_var (TODO_do_in_parsing.tvar ~loc t) ~loc ()
  )

(* ========================== PATTERNS ===================================== *)

and compile_pattern ~(raise: ('e, 'w) raise) : CST.pattern -> AST.pattern = fun p ->
  let self = compile_pattern ~raise in
  let pat ~loc p = Location.wrap ~loc p in
  match p with
  | P_Ctor p -> TODO_do_in_parsing.need_rework p "never emited alone"
  | P_App p -> (
    TODO_do_in_parsing.extract_arg_from_app self p
  )
  | P_Attr p -> (
    let attr, ptrn = p in
    let attr, loc = r_split attr in
    let attr = translate_attr_pascaligo attr in
    let ptrn = self ptrn in
    pat ~loc (P_attr (attr,ptrn))
  )
  | P_Bytes p -> (
    let (_s, hex), loc = w_split p in
    let b = Hex.to_bytes hex in
    pat ~loc (P_literal (Literal_bytes b))
  )
  | P_Cons p -> (
    let (p1, _, p2), loc = r_split p in
    let p1 = self p1 in
    let p2 = self p2 in
    pat ~loc (P_list (Cons (p1, p2)))
  )
  | P_Int p -> (
    let (_s, z), loc = w_split p in
    pat ~loc (P_literal (Literal_int z))
  )
  | P_List p -> (
    let p, loc = r_split p in
    let ps = List.map ~f:self (sepseq_to_list p.elements) in
    pat ~loc (P_list (List ps))
  )
  | P_ModPath p -> (
    let p, loc = r_split p in
    let module_path = List.Ne.map
      (fun x -> let x,loc = w_split x in TODO_do_in_parsing.mvar ~loc x)
      (nsepseq_to_nseq p.module_path)
    in
    let field = self p.field in
    pat ~loc (P_mod_access Mod_access.{module_path; field})
  )
  | P_Mutez p -> (
    let (_s, z), loc = w_split p in
    pat ~loc (P_literal (Literal_mutez (TODO_do_in_parsing.six_to_z z)))
  )
  | P_Nat p -> (
    let (_s, z), loc = w_split p in
    pat ~loc (P_literal (Literal_nat z))
  )
  | P_Nil p -> (
    let _, loc = w_split p in
    pat ~loc (P_list (List []))
  )
  | P_Par p -> (
    self (r_fst p).inside
  )
  | P_Record p -> (
    let p, loc = r_split p in
    let fields =
      let translate_field_assign : CST.field_pattern -> (Label.t, AST.pattern) AST.field = function
      | Punned { pun; attributes } ->
        TODO_do_in_parsing.weird_attributes attributes;
        let pun,_ = w_split (TODO_do_in_parsing.compile_pattern_record_lhs pun) in
        Punned (Label.of_string pun)
      | Complete { field_lhs; field_lens = _ ; field_rhs; attributes } ->
        TODO_do_in_parsing.weird_attributes attributes;
        let lhs,_ = w_split (TODO_do_in_parsing.compile_pattern_record_lhs field_lhs) in
        Complete (Label.of_string lhs, self field_rhs)
      in
      List.map ~f:(translate_field_assign <@ r_fst) @@ sepseq_to_list p.elements
    in
    pat ~loc (P_pun_record fields)
  )
  | P_String p -> (
    let s, loc = w_split p in
    pat ~loc (P_literal (Literal_string (Standard s)))
  )
  | P_Tuple    p -> (
    let p, loc = r_split p in
    let p = List.map ~f:self (nsepseq_to_list p.inside) in
    pat ~loc (P_tuple p)
  )
  | P_Typed    p -> (
    let p, loc = r_split p in
    let ptrn = self p.pattern in
    let ty = compile_type_expression ~raise @@ snd p.type_annot in
    pat ~loc (P_typed (ty,ptrn))
  )
  | P_Var      p -> (
    let s, loc = w_split p in
    pat ~loc (P_var (TODO_do_in_parsing.var ~loc s))
  )
  | P_Verbatim p -> (
    let s, loc = w_split p in
    pat ~loc (P_literal (Literal_string (Standard s)))
  )

(* ========================== INSTRUCTIONS ================================= *)

and compile_block ~(raise: ('e, 'w) raise) : CST.block -> AST.statement nseq = fun b ->
  List.Ne.map (compile_statement ~raise) @@ nsepseq_to_nseq b.statements

and compile_test_clause : raise:_ -> CST.test_clause -> (instruction,statement) AST.Test_clause.t = fun ~raise c ->
  match c with
  | CST.ClauseInstr i -> ClauseInstr (compile_instruction ~raise i)
  | CST.ClauseBlock b -> ClauseBlock (compile_block ~raise @@ r_fst b)

and compile_case_clause : type a b . raise:_ -> (a -> b) -> a CST.case_clause -> (_,b) AST.Case.clause = fun ~raise f c ->
  let pattern = compile_pattern ~raise c.pattern in
  let rhs     = f c.rhs in
  {pattern; rhs}

and compile_case : type a b . raise:_ -> (a -> b) -> a CST.case -> (_,_,b) AST.Case.t = fun ~raise f c ->
  let expr = compile_expression ~raise c.expr in
  let cases = List.Ne.map (compile_case_clause ~raise f <@ r_fst) @@ nsepseq_to_nseq c.cases in
  {expr; cases}

and compile_cond : 'a 'b. raise:_ -> ('a -> 'b) -> 'a CST.conditional -> (_,'b) AST.Cond.t = fun ~raise f c ->
  let test  = compile_expression ~raise c.test in
  let ifso  = f c.if_so in
  let ifnot = Option.map ~f:(f <@ snd) c.if_not in
  {test; ifso; ifnot}

and compile_for_map ~raise : CST.for_map -> (_,_) AST.For_collection.for_map = fun m ->
  let binding =
    let k, _, v = m.binding in
    TODO_do_in_parsing.var ~loc:(w_snd k) (w_fst k),
    TODO_do_in_parsing.var ~loc:(w_snd v) (w_fst v)
  in
  let collection = compile_expression ~raise m.collection in
  let block      = compile_block ~raise @@ r_fst m.block in
  {binding; collection; block}

and compile_for_set_or_list ~raise : CST.for_set_or_list -> (_,_) AST.For_collection.for_set_or_list = fun s ->
  let var = TODO_do_in_parsing.var ~loc:(w_snd s.var) (w_fst s.var) in
  let for_kind = match s.for_kind with
  | `Set  _ -> `Set
  | `List _ -> `List
  in
  let collection = compile_expression ~raise s.collection in
  let block      = compile_block ~raise (r_fst s.block) in
  {var; for_kind; collection; block}

and compile_instruction ~(raise: ('e, 'w) raise) : CST.instruction -> AST.instruction = fun i ->
  let compile_expr = compile_expression ~raise in
  match i with
  | I_Assign i -> (
    let i, loc = r_split i in
    let lhs_expr = compile_expr i.lhs in
    let rhs_expr = compile_expr i.rhs in
    i_struct_assign {lhs_expr; rhs_expr} ~loc ()
  )
  | I_Call i -> (
    let i, loc = r_split i in
    let f, args = i in
    let f = compile_expression ~raise f in
    let args : expr list = List.map ~f:compile_expr @@ sepseq_to_list (r_fst args).inside in
    i_call ~loc (f, args) ()
  )
  | I_Case i -> (
    let i, loc = r_split i in
    let i = compile_case ~raise (compile_test_clause ~raise) i in
    i_case i ~loc ()
  )
  | I_Cond i -> (
    let i, loc = r_split i in
    let i = compile_cond ~raise (compile_test_clause ~raise) i in
    i_cond i ~loc ()
  )
  | I_For i -> (
    let i, loc = r_split i in
    let index = TODO_do_in_parsing.var ~loc:(w_snd i.index) (w_fst i.index) in
    let init  = compile_expr i.init in
    let bound = compile_expr i.bound in
    let step = Option.map ~f:(compile_expr <@ snd) i.step in
    let block = compile_block ~raise @@ r_fst i.block in
    i_for ~loc {index; init; bound; step; block} ()
  )
  | I_ForIn  i -> (
    let i, loc = TODO_unify_in_cst.for_in (compile_for_map ~raise) (compile_for_set_or_list ~raise) i in
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
    let type_params = Option.map ~f:extract_type_params s.type_params in
    let rhs_type    = Option.map ~f:(compile_type_expression ~raise <@ snd) s.var_type in
    let let_rhs        = compile_expression ~raise s.init in
    TODO_unify_in_cst.vardecl_as_decl ~loc {pattern; type_params; rhs_type; let_rhs}
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
    let param_type = Option.map ~f:(compile_type_expression ~raise <@ snd) p.param_type in
    {param_kind; pattern; param_type}
  in
  return @@ match e with
  | E_Var var -> (
      let var, loc = w_split var in
      e_variable (TODO_do_in_parsing.var ~loc var) ~loc ()
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
      e_recordpascaligo fields ~loc ()
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
      let type_params = Option.map ~f:extract_type_params f.type_params in
      let parameters  = List.map ~f:(compile_param_decl <@ r_fst)
        @@ sepseq_to_list @@ (r_fst f.parameters).inside
      in
      let ret_type    = Option.map ~f:(compile_type_expression ~raise <@ snd) f.ret_type in
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
      let args = Option.map ~f:(nseq_map self <@ extract_tuple) args in
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
      let ifnot = Option.map ~f:(self <@ snd) cond.if_not in
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
      let block =
        nseq_map (compile_statement ~raise) @@ nsepseq_to_nseq (r_fst be.block).statements
      in
      let expr  = self be.expr in
      e_blockpascaligo {block; expr} ~loc ()
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
  let compile_type_params : CST.type_params CST.chevrons Region.reg -> AST.Ty_variable.t nseq =
     fun tp ->
      nseq_map
        (fun x -> TODO_do_in_parsing.tvar ~loc:(w_snd x) (w_fst x))
        (nsepseq_to_nseq (r_fst tp).inside)
  in
  match decl with
  | D_Directive d -> (
    let loc = Simple_utils.Location.lift (Preprocessor.Directive.to_region d) in
    d_directive d ~loc ()
  )
  | D_Type d -> (
    let d, loc = r_split d in
    let name = TODO_do_in_parsing.tvar ~loc:(w_snd d.name) (w_fst d.name) in
    let params = Option.map ~f:(fun (tp : _ CST.par CST.reg) ->
      List.Ne.map
        (fun x -> TODO_do_in_parsing.tvar ~loc:(w_snd x) (w_fst x))
        (nsepseq_to_nseq (r_fst tp).inside)
      ) d.params
    in
    let type_expr = compile_type_expression ~raise d.type_expr in
    d_type {name; params; type_expr} ~loc ()
  )
  | D_Const d -> (
    let d, loc = r_split d in
    let type_params = Option.map ~f:compile_type_params d.type_params in
    let pattern = compile_pattern ~raise d.pattern in
    let rhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) d.const_type in
    let let_rhs = compile_expression ~raise d.init in
    d_const ~loc {pattern; type_params; rhs_type; let_rhs} ()
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
    let fun_name = TODO_do_in_parsing.var ~loc:(w_snd d.fun_name) (w_fst d.fun_name) in
    let type_params = Option.map ~f:compile_type_params d.type_params in
    let parameters =
      let compile_param_decl : CST.param_decl -> AST.param_decl = fun pd ->
        let param_kind = (
          match pd.param_kind with
          | `Const _ -> `Const
          | `Var   _ -> `Var
        ) in
        let pattern = compile_pattern ~raise pd.pattern in
        let param_type = Option.map ~f:(compile_type_expression ~raise <@ snd) pd.param_type in
        {param_kind; pattern; param_type}
      in
      List.map ~f:(compile_param_decl <@ r_fst) @@ sepseq_to_list (r_fst d.parameters).inside
    in
    let ret_type = Option.map ~f:(compile_type_expression ~raise <@ snd) d.ret_type in
    let return = compile_expression ~raise d.return in
    d_fun {is_rec; fun_name; type_params; parameters; ret_type; return} ~loc ()
  )
  | D_Module d -> (
    let d, loc = r_split d in
    let name = TODO_do_in_parsing.mvar ~loc:(w_snd d.name) (w_fst d.name) in
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
    let module_path = List.Ne.map
      (fun t -> TODO_do_in_parsing.mvar ~loc:(w_snd t) (w_fst t))
      (nsepseq_to_nseq m.module_path)
    in
    let field = TODO_do_in_parsing.mvar ~loc:(w_snd m.field) (w_fst m.field) in
    m_path (List.Ne.append module_path (field,[])) ~loc ()
  )
  | M_Var  m -> (
    let s, loc = w_split m in
    let v = TODO_do_in_parsing.mvar ~loc s in
    m_var v ~loc ()
  )

(* ========================== PROGRAM ===================================== *)
let compile_program ~raise : CST.t -> AST.program = fun t ->
  let declarations = nseq_to_list t.decl in
  let declarations = List.map ~f:(fun a ~raise -> compile_declaration ~raise a) declarations in
  let declarations = Simple_utils.Trace.collect ~raise declarations in
  List.map ~f:(fun x -> P_Declaration x) declarations