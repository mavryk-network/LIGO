open Simple_utils.Utils
module List = Simple_utils.List
open Unification_shared.Helpers
module Option = Simple_utils.Option
module O = Ast_unified
module I = Cst.Jsligo

let ghost : string I.wrap = I.Wrap.ghost ""

module TODO_do_in_parsing = struct
  let conv_attr attr_reg =
    let (key, value), _loc = w_split attr_reg in
    let f = function
      | I.Attr.String x -> x
      | Ident x -> x
    in
    Nano_prim.Attribute.{ key; value = Option.map ~f value }


  let conv_attrs = List.map ~f:conv_attr

  let constructor_element arg_opt =
    Option.value ~default:(I.EUnit (Region.wrap_ghost (ghost, ghost))) arg_opt


  let weird_attr _ = ()

  let labelize_pattern p =
    match p with
    (* would be better to emit a label/string directly ? *)
    | I.PVar var ->
      Location.wrap ~loc:(r_snd var) @@ O.Label.of_string @@ var.value.variable#payload
    | _ -> failwith "labelize_pattern: impossible??"


  let unused_node () = failwith "unused node, can we clean ?"
  let labelize x = O.Label.of_string x

  let t_disc_locs (objs : (I.obj_type, I.vbar) nsepseq) =
    (* The region of the discriminated union TDisc
      is the union of all its objects' regions *)
    let locations = List.Ne.map (fun obj -> snd @@ r_split obj) (nsepseq_to_nseq objs) in
    List.Ne.fold_left locations ~init:Location.dummy ~f:Location.cover


  let pattern_to_param pattern = O.Param.{ pattern; param_kind = `Const }

  let field_as_open_t (ma : I.type_expr) =
    (* here, we should use module expressions, maybe ? *)
    match ma with
    | I.TPar _ -> true
    | _ -> false


  let flatten_moda ({ module_name; selector = _; field } : I.expr I.module_access) =
    let rec aux : I.module_name List.Ne.t -> I.expr -> I.module_name List.Ne.t * I.expr =
     fun acc expr ->
      match expr with
      | EModA { value = { module_name; field; _ }; _ } ->
        aux (List.Ne.append acc (List.Ne.singleton module_name)) field
      | _ -> acc, expr
    in
    let path, field = aux (List.Ne.singleton module_name) field in
    let is_open =
      match field with
      | I.EPar _ -> true
      | _ -> false
    in
    (path, field), is_open


  let control_flow_clause compile_statement (x : I.statement) =
    (* if the statement is a block containing a single instruction,
       we do not want to emit a ClauseBlock, but a ClauseInstr *)
    let single_stmt_block (x : I.statement) =
      nsepseq_of_nseq ~sep:ghost (List.Ne.singleton x)
    in
    match Location.unwrap @@ compile_statement x with
    | O.S_instr (I.SBlock { value = { inside; _ }; _ }) ->
      (match nsepseq_to_list inside with
      | [ one ] ->
        (match Location.unwrap @@ compile_statement one with
        | S_instr i -> O.Test_clause.ClauseInstr i
        | _ -> O.Test_clause.ClauseBlock (single_stmt_block x))
      | _ -> O.Test_clause.ClauseBlock inside)
    | S_instr i -> O.Test_clause.ClauseInstr i
    | _ -> O.Test_clause.ClauseBlock (single_stmt_block x)


  let mvar x = Ligo_prim.Module_var.of_input_var ~loc:(Location.File x#region) x#payload
  let var x = Ligo_prim.Value_var.of_input_var ~loc:(Location.File x#region) x#payload
  let tvar x = Ligo_prim.Type_var.of_input_var ~loc:(Location.File x#region) x#payload
end

module Eq = struct
  type expr = I.expr
  type ty_expr = I.type_expr

  (*
    The most troubling thing with jsligo: functions parameters is a single expression
    `<parameters:expr> : <lhs_type:type> => ..` (see EFun node bellow)
     I believe they should really be parsed as a pattern list

    note:
      as of today, this is a value expression `([x ,...[y, ...z]]) => x`
      while this is not a valid declaration `const [x ,...[y, ...z]] = toto`.
      in the first case, it's an "expression as a pattern". In the second case, it's a
      pattern
  *)
  type pattern =
    [ `Pattern of I.pattern
    | `Expr of I.expr
    ]

  type statement = I.statement
  type block = I.statements
  type mod_expr = I.statements
  type instruction = I.statement
  type declaration = I.statement
  type program_entry = I.toplevel_statement
  type program = I.t
  type sig_expr = I.interface_expr
  type sig_entry = I.interface_entry
end

let pattern_of_expr x = `Expr x
let pattern_of_pattern x = `Pattern x

module Folding = Folding (Eq)

let rec expr : Eq.expr -> Folding.expr =
 fun e ->
  let loc = Location.lift (I.expr_to_region e) in
  let return = Location.wrap ~loc in
  let compile_bin_op (sign : O.Operators.op) (op : _ I.bin_op Region.reg) =
    let I.{ op = _; arg1; arg2 } = r_fst op in
    O.E_binary_op { operator = Location.wrap ~loc sign; left = arg1; right = arg2 }
  in
  let compile_unary_op (sign : AST.Operators.op) op =
    let I.{ op = _; arg } = r_fst op in
    O.E_unary_op { operator = Location.wrap ~loc sign; arg }
  in
  match e with
  | EVar var -> return @@ O.E_variable (TODO_do_in_parsing.var var)
  | EPar par -> expr par.value.inside
  | EUnit _ -> return @@ E_literal Literal_unit
  | EBytes b ->
    let _lexeme, b = b#payload in
    return @@ E_literal (Literal_bytes (Hex.to_bytes b))
  | EString str ->
    let str =
      match str with
      | String str -> Simple_utils.Ligo_string.Standard str#payload
      | Verbatim str -> Simple_utils.Ligo_string.Verbatim str#payload
    in
    return @@ E_literal (Literal_string str)
  | EArith arth ->
    return
    @@
    (match arth with
    | Add plus -> compile_bin_op PLUS plus
    | Sub minus -> compile_bin_op MINUS minus
    | Mult times -> compile_bin_op STAR times
    | Div slash -> compile_bin_op SLASH slash
    | Mod mod_ -> compile_bin_op PRCENT mod_
    | Neg minus -> compile_unary_op MINUS minus
    | Int i -> E_literal (Literal_int (snd i#payload)))
  | ELogic logic ->
    (match logic with
    | BoolExpr be ->
      return
      @@
      (match be with
      | Or or_ -> compile_bin_op DPIPE or_
      | And and_ -> compile_bin_op DAMPERSAND and_
      | Not not_ -> compile_unary_op EX_MARK not_)
    | CompExpr ce ->
      return
      @@
      (match ce with
      | Lt lt -> compile_bin_op LT lt
      | Leq le -> compile_bin_op LE le
      | Gt gt -> compile_bin_op GT gt
      | Geq ge -> compile_bin_op GE ge
      | Equal eq -> compile_bin_op DEQ eq
      | Neq ne -> compile_bin_op EQ_SLASH_EQ ne))
  | ECall { value = expr, args; _ } ->
    let args =
      match args with
      | Unit reg ->
        let loc = r_snd reg in
        Location.wrap ~loc []
      | Multiple args ->
        let loc = r_snd args in
        Location.wrap ~loc @@ nsepseq_to_list (r_fst args).inside
    in
    return @@ E_call (expr, args)
  | EConstr { value = ctor, arg_opt; _ } ->
    let element = TODO_do_in_parsing.constructor_element arg_opt in
    return
    @@ E_applied_constructor { constructor = O.Label.of_string ctor#payload; element }
  | EArray { value = items; _ } ->
    let items =
      let translate_array_item : I.array_item -> _ AST.Array_repr.item = function
        | Expr_entry e -> Expr_entry e
        | Rest_entry e -> Rest_entry (r_fst e).expr
      in
      Option.value_map items.inside ~default:[] ~f:(fun lst ->
          List.map ~f:translate_array_item (nsepseq_to_list lst))
    in
    return @@ E_array items
  | EObject { value; _ } ->
    let props : _ AST.Object_.t =
      let translate_property : I.property -> _ O.Object_.property = function
        | Punned_property e -> Punned_property (r_fst e)
        | Property p ->
          let I.{ name; value; _ } = p.value in
          Property (name, value)
        | Property_rest p -> Property_rest p.value.expr
      in
      nseq_map translate_property @@ nsepseq_to_nseq value.inside
    in
    return @@ E_object props
  | EProj { value = { expr; selection }; _ } ->
    let path : _ O.Selection.t =
      match selection with
      | FieldName name ->
        let name = (r_fst name).value#payload in
        FieldName (O.Label.of_string name)
      | Component comp ->
        let comp = (r_fst comp).inside in
        Component_expr comp
    in
    return @@ E_proj { struct_ = expr; path }
  | EModA { value = ma; _ } ->
    let (module_path, field), field_as_open = TODO_do_in_parsing.flatten_moda ma in
    let module_path = List.Ne.map TODO_do_in_parsing.mvar module_path in
    return @@ E_module_open_in { module_path; field; field_as_open }
  | EFun f ->
    let I.{ type_params; parameters; lhs_type; arrow = _; body } = f.value in
    let type_params =
      Option.map type_params ~f:(fun (tp : I.type_generics) ->
          List.Ne.map TODO_do_in_parsing.tvar (nsepseq_to_nseq (r_fst tp).inside))
    in
    let parameters =
      match parameters with
      | EPar { value = { inside = ESeq x; _ }; _ } ->
        List.map
          ~f:(TODO_do_in_parsing.pattern_to_param <@ pattern_of_expr)
          (nsepseq_to_list x.value)
      | EPar { value = { inside = x; _ }; _ } | x ->
        [ x |> pattern_of_expr |> TODO_do_in_parsing.pattern_to_param ]
    in
    let ret_type = Option.map ~f:snd lhs_type in
    (match body with
    | FunctionBody body ->
      return
      @@ E_block_poly_fun { type_params; parameters; ret_type; body = body.value.inside }
    | ExpressionBody body ->
      return @@ E_poly_fun { type_params; parameters; ret_type; body })
  | EAnnot a ->
    let e, _, te = a.value in
    return @@ E_annot (e, te)
  | ECodeInj { value = { language; code; _ }; _ } ->
    let language = w_fst language in
    return @@ E_raw_code { language; code }
  | ESeq seq -> return @@ E_sequence (nsepseq_to_list seq.value)
  | EAssign (expr1, op, expr2) ->
    let op =
      O.Assign_chainable.(
        match op.value with
        | I.Eq -> Eq
        | Assignment_operator aop ->
          Assignment_operator
            (match aop with
            | Times_eq -> Times_eq
            | Div_eq -> Div_eq
            | Min_eq -> Min_eq
            | Plus_eq -> Plus_eq
            | Mod_eq -> Mod_eq))
    in
    (* Parser miscomputes location here *)
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region expr1) (I.expr_to_region expr2)
    in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1; op; expr2 }
  | ETernary { value = { condition; truthy; falsy; _ }; _ } ->
    let ifnot = Some falsy in
    return @@ E_cond { test = condition; ifso = truthy; ifnot }
  | EContract { value = c; _ } ->
    let lst = List.Ne.map TODO_do_in_parsing.mvar (nsepseq_to_nseq c) in
    return @@ E_contract lst
  | EPrefix { region = _; value = { update_type = Increment op; variable } } ->
    let loc = Location.lift op#region in
    let pre_op = Location.wrap ~loc O.Prefix_postfix.Increment in
    let variable = TODO_do_in_parsing.var variable in
    return @@ E_prefix { pre_op; variable }
  | EPrefix { region = _; value = { update_type = Decrement op; variable } } ->
    let loc = Location.lift op#region in
    let pre_op = Location.wrap ~loc O.Prefix_postfix.Decrement in
    let variable = TODO_do_in_parsing.var variable in
    return @@ E_prefix { pre_op; variable }
  | EPostfix { region = _; value = { update_type = Increment op; variable } } ->
    let loc = Location.lift op#region in
    let post_op = Location.wrap ~loc O.Prefix_postfix.Increment in
    let variable = TODO_do_in_parsing.var variable in
    return @@ E_postfix { post_op; variable }
  | EPostfix { region = _; value = { update_type = Decrement op; variable } } ->
    let loc = Location.lift op#region in
    let post_op = Location.wrap ~loc O.Prefix_postfix.Decrement in
    let variable = TODO_do_in_parsing.var variable in
    return @@ E_postfix { post_op; variable }


let rec ty_expr : Eq.ty_expr -> Folding.ty_expr =
 fun t ->
  let loc = Location.lift (I.type_expr_to_region t) in
  let return = Location.wrap ~loc in
  let return_attr attributes ~attr ~no_attr =
    match attributes with
    | [] -> return @@ no_attr
    | hd :: tl ->
      let hd = TODO_do_in_parsing.conv_attr hd in
      return @@ O.T_attr (hd, attr tl)
  in
  match t with
  | TProd { inside; attributes } ->
    let t = nsepseq_to_nseq inside.value.inside in
    return_attr attributes ~no_attr:(T_prod t) ~attr:(fun attributes ->
        I.TProd { inside; attributes })
  | TSum { value = { variants; attributes; _ } as v; region } ->
    let destruct : I.variant -> _ =
     fun { tuple; attributes } ->
      let I.{ constr; params } = (r_fst tuple).inside in
      let ty =
        Option.map
          ~f:(fun lst ->
            let p = (nsepseq_to_nseq <@ snd) lst in
            match p with
            | x, [] -> x
            | _ ->
              let inside =
                Region.wrap_ghost
                @@ I.{ lbracket = ghost; inside = snd lst; rbracket = ghost }
              in
              I.TProd { inside; attributes = [] })
          params
      in
      ( TODO_do_in_parsing.labelize constr#payload
      , ty
      , TODO_do_in_parsing.conv_attrs attributes )
    in
    return_attr
      attributes
      ~no_attr:
        (let variants =
           r_fst variants
           |> nsepseq_to_list
           |> List.map ~f:(destruct <@ r_fst)
           |> O.Non_linear_rows.make
         in
         T_sum_raw variants)
      ~attr:(fun attributes -> I.TSum { value = { v with variants; attributes }; region })
  | TObject { value = { ne_elements; attributes; _ } as v; region } ->
    let fields =
      let destruct I.{ field_name; field_type; attributes; _ } =
        ( TODO_do_in_parsing.labelize field_name#payload
        , Some field_type
        , TODO_do_in_parsing.conv_attrs attributes )
      in
      let lst = List.map ~f:(destruct <@ r_fst) @@ nsepseq_to_list ne_elements in
      O.Non_linear_rows.make lst
    in
    return_attr attributes ~no_attr:(T_record_raw fields) ~attr:(fun attributes ->
        I.TObject { value = { v with ne_elements; attributes }; region })
  | TApp t ->
    let constr, args = t.value in
    let constr = I.TVar constr in
    let type_args = nsepseq_to_nseq (r_fst args).inside in
    return @@ T_app { constr; type_args }
  | TFun { value = fta, _, te2; _ } ->
    let fun_type_args =
      let compile_fun_type_arg : I.fun_type_arg -> _ O.Named_fun.fun_type_arg =
       fun { name; type_expr; _ } -> { name = name#payload; type_expr }
      in
      List.map ~f:compile_fun_type_arg (nsepseq_to_list fta.inside)
    in
    let type_expr = te2 in
    return @@ T_named_fun (fun_type_args, type_expr)
  | TPar t -> ty_expr (r_fst t).inside
  | TVar t -> return @@ T_var (TODO_do_in_parsing.tvar t)
  | TString t -> return @@ T_string t#payload
  | TInt t ->
    let s, z = t#payload in
    return @@ T_int (s, z)
  | TModA { value = { module_name; field; _ }; _ } ->
    let module_path = TODO_do_in_parsing.mvar module_name in
    let field_as_open = TODO_do_in_parsing.field_as_open_t t in
    return @@ T_module_open_in { module_path; field; field_as_open }
  | TParameter x ->
    let path = nsepseq_map TODO_do_in_parsing.mvar x.value in
    return @@ T_contract_parameter (nsepseq_to_nseq path)
  | TDisc t ->
    let fields =
      let destruct_obj (x : I.obj_type) =
        let I.{ attributes; ne_elements; _ } = x.value in
        let obj =
          Region.wrap_ghost
            I.
              { compound = None (* (I.field_decl Region.reg, I.semi) nsepseq *)
              ; ne_elements
              ; terminator = None
              ; attributes = []
              }
        in
        ( () (* , t_record_raw ~loc (Non_linear_rows.make lst) *)
        , I.TObject obj
        , TODO_do_in_parsing.conv_attrs attributes )
      in
      let lst = List.map ~f:destruct_obj (nsepseq_to_list t) in
      O.Non_linear_disc_rows.make lst
    in
    (* locs miscomputed here *)
    let loc = TODO_do_in_parsing.t_disc_locs t in
    Location.wrap ~loc @@ O.T_disc_union fields


let rec pattern : Eq.pattern -> Folding.pattern =
 fun p ->
  match p with
  | `Pattern p ->
    let loc = Location.lift (I.pattern_to_region p) in
    let return = Location.wrap ~loc in
    (match p with
    | PConstr _p -> TODO_do_in_parsing.unused_node ()
    | PAssign _p -> TODO_do_in_parsing.unused_node ()
    | PDestruct _p -> TODO_do_in_parsing.unused_node ()
    | PRest p -> return @@ O.P_rest (TODO_do_in_parsing.labelize p.value.rest#payload)
    | PVar p ->
      let I.{ variable; attributes } = r_fst p in
      (match attributes with
      | [] -> return @@ P_var (TODO_do_in_parsing.var variable)
      | hd :: tl ->
        let attr = TODO_do_in_parsing.conv_attr hd in
        let p = { p with value = { p.value with attributes = tl } } in
        return @@ P_attr (attr, pattern_of_pattern @@ I.PVar p))
    | PObject o ->
      let lps =
        List.map
          ~f:(fun p ->
            let l = TODO_do_in_parsing.labelize_pattern p in
            O.Field.Punned l)
          (Utils.nsepseq_to_list o.value.inside)
      in
      return @@ P_pun_record lps
    | PArray p ->
      let p = nsepseq_to_list p.value.inside in
      return @@ P_tuple (List.map ~f:pattern_of_pattern p))
  | `Expr e ->
    let loc = Location.lift (I.expr_to_region e) in
    let return = Location.wrap ~loc in
    (match e with
    | EPar x -> pattern (pattern_of_expr x.value.inside)
    | EVar v -> return @@ O.P_var (TODO_do_in_parsing.var v)
    | EUnit _ -> return @@ P_unit
    | EAnnot { value = expr, _, type_expr; _ } ->
      return @@ P_typed (type_expr, pattern_of_expr expr)
    | EArray { value = items; _ } ->
      (match sepseq_to_list items.inside with
      | [] -> return @@ P_list (List [])
      | [ Expr_entry hd; Rest_entry tl ] ->
        (* [x ,...[y, ...z]] *)
        (* see https://tezos-dev.slack.com/archives/GMHV0U3Q9/p1670929406569099 *)
        return @@ P_list (Cons (pattern_of_expr hd, pattern_of_expr tl.value.expr))
      | lst ->
        return
        @@ P_tuple
             (List.map lst ~f:(function
                 | Expr_entry x -> pattern_of_expr x
                 | _ -> failwith "incorrect pattern")))
    | EObject obj ->
      let lst = nsepseq_to_list obj.value.inside in
      let aux : I.property -> (_, _) O.Field.t = function
        | Punned_property { value = EVar v; _ } ->
          let loc = Location.File v#region in
          Punned (Location.wrap ~loc @@ O.Label.of_string v#payload)
        | Property { value = { name = EVar v; value; _ }; _ } ->
          Complete (O.Label.of_string v#payload, pattern_of_expr value)
        | _ -> failwith "unrecognized pattern"
      in
      return @@ P_pun_record (List.map ~f:aux lst)
    | _ -> failwith "unrecognized pattern")


(* in JSLIGO, instruction ; statements and declaration are all statement *)

let block : Eq.block -> Folding.block =
 fun stmts ->
  let loc =
    nsepseq_foldl
      Location.cover
      Location.generated
      (nsepseq_map (Location.lift <@ I.statement_to_region) stmts)
  in
  Location.wrap ~loc (nsepseq_to_nseq stmts)


let mod_expr : Eq.mod_expr -> Folding.mod_expr =
 fun stmts ->
  let loc =
    Location.(
      stmts
      |> nsepseq_map (lift <@ I.statement_to_region)
      |> nsepseq_foldl cover generated)
  in
  let stmts = stmts |> nsepseq_to_nseq |> nseq_map (fun x -> I.TopLevel (x, None)) in
  Location.wrap ~loc (O.M_body I.{ statements = stmts; eof = ghost })


let rec statement : Eq.statement -> Folding.statement =
 fun s ->
  let loc = Location.lift (I.statement_to_region s) in
  let return = Location.wrap ~loc in
  match s with
  | SInterface _ | SNamespace _ | SImport _ | SExport _ | SLet _ | SConst _ | SType _ ->
    return @@ O.S_decl s
  | SBlock _
  | SExpr _
  | SCond _
  | SReturn _
  | SSwitch _
  | SBreak _
  | SWhile _
  | SForOf _
  | SFor _ -> return @@ S_instr s


and instruction : Eq.instruction -> Folding.instruction =
 fun i ->
  let loc = Location.lift (I.statement_to_region i) in
  let return = Location.wrap ~loc in
  let single_stmt_block (x : I.statement) =
    nsepseq_of_nseq ~sep:ghost (List.Ne.singleton x)
  in
  match i with
  | SBlock s -> return @@ O.I_block s.value.inside
  | SExpr (attr, expr) ->
    TODO_do_in_parsing.weird_attr attr;
    return @@ I_expr expr
  | SCond c ->
    let c = c.value in
    let I.{ ifso; ifnot; test; _ } = c in
    let ifso = TODO_do_in_parsing.control_flow_clause statement ifso in
    let ifnot =
      Option.map ifnot ~f:(TODO_do_in_parsing.control_flow_clause statement <@ snd)
    in
    return @@ I_cond { test = test.inside; ifso; ifnot }
  | SReturn s -> return @@ I_return s.value.expr
  | SSwitch s ->
    let cases =
      List.Ne.map
        (function
          | I.Switch_case { expr; statements; _ } ->
            O.Switch.Switch_case (expr, statements)
          | I.Switch_default_case { statements; _ } ->
            O.Switch.Switch_default_case statements)
        s.value.cases
    in
    return @@ I_switch { switchee = s.value.expr; cases }
  | SBreak _ -> return @@ I_break
  | SWhile s ->
    let cond = s.value.expr in
    let block = single_stmt_block s.value.statement in
    return @@ I_while { cond; block }
  | SForOf s ->
    let I.{ index_kind; index; expr; statement; _ } = r_fst s in
    let index_kind =
      match index_kind with
      | `Let _ -> `Let
      | `Const _ -> `Const
    in
    let index = TODO_do_in_parsing.var index in
    return @@ I_for_of { index_kind; index; expr; for_stmt = statement }
  | SFor s ->
    let I.{ initialiser; condition; afterthought; statement; _ } = r_fst s in
    let afterthought = Option.map afterthought ~f:Utils.nsepseq_to_nseq in
    return @@ I_for_stmt { initialiser; condition; afterthought; statement }
  (* impossible, if triggered, look at functions 'statement' *)
  | SLet _ | SConst _ | SType _ | SInterface _ | SNamespace _ | SExport _ | SImport _ ->
    assert false


and declaration : Eq.declaration -> Folding.declaration =
 fun d ->
  let loc = Location.lift (I.statement_to_region d) in
  let return = Location.wrap ~loc in
  let compile_val_binding
      : I.val_binding -> (Eq.pattern, I.expr, I.type_expr) O.Simple_decl.t
    =
   fun { binders; type_params; lhs_type; eq = _; expr } ->
    let pattern = `Pattern binders in
    let type_params =
      Option.map type_params ~f:(fun (tp : I.type_generics) ->
          nseq_map TODO_do_in_parsing.tvar (nsepseq_to_nseq (r_fst tp).inside))
    in
    let rhs_type = Option.map ~f:snd lhs_type in
    { type_params; pattern; rhs_type; let_rhs = expr }
  in
  let return_attr attributes ~attr ~no_attr =
    match attributes with
    | [] -> return @@ no_attr
    | hd :: tl ->
      let hd = TODO_do_in_parsing.conv_attr hd in
      return @@ O.D_attr (hd, attr tl)
  in
  match d with
  | SNamespace ({ value; _ } as n) ->
    let kwd, module_name, interface_annotation, statements, attributes = value in
    let annotation =
      Option.map ~f:(fun { region = _; value = _, value } -> value) interface_annotation
    in
    return_attr
      attributes
      ~no_attr:
        (let name = TODO_do_in_parsing.mvar module_name in
         O.D_module { name; mod_expr = statements.value.inside; annotation })
      ~attr:(fun attributes ->
        I.SNamespace
          { n with
            value = kwd, module_name, interface_annotation, statements, attributes
          })
  | SImport { value = s; _ } ->
    let import =
      match s with
      | Import_rename { alias; module_path; _ } ->
        let alias = TODO_do_in_parsing.mvar alias in
        let module_path =
          List.Ne.map TODO_do_in_parsing.mvar (nsepseq_to_nseq module_path)
        in
        O.Import.Import_rename { alias; module_path }
      | Import_all_as s ->
        let alias = TODO_do_in_parsing.mvar s.alias in
        let module_str = s.module_path#payload in
        O.Import.Import_all_as { alias; module_str }
      | Import_selected { imported; module_path; _ } ->
        let imported =
          List.Ne.map TODO_do_in_parsing.var (nsepseq_to_nseq (r_fst imported).inside)
        in
        let module_str = module_path#payload in
        O.Import.Import_selected { imported; module_str }
    in
    return @@ D_import import
  | SInterface ({ value; _ } as n) ->
    let kwd, module_name, interface_body, attributes = value in
    return_attr
      attributes
      ~no_attr:
        (let name = TODO_do_in_parsing.mvar module_name in
         O.D_signature { name; sig_expr = IInterface interface_body })
      ~attr:(fun attributes ->
        I.SInterface { n with value = kwd, module_name, interface_body, attributes })
  | SExport { value = _, statement; _ } -> return @@ D_export statement
  | SLet ({ value = { bindings; attributes; _ }; _ } as l) ->
    return_attr
      attributes
      ~no_attr:
        (let bindings =
           List.Ne.map (compile_val_binding <@ r_fst) (nsepseq_to_nseq bindings)
         in
         D_multi_var bindings)
      ~attr:(fun attributes -> I.SLet { l with value = { l.value with attributes } })
  | SConst ({ value = { bindings; attributes; _ }; _ } as l) ->
    return_attr
      attributes
      ~no_attr:
        (let bindings =
           List.Ne.map (compile_val_binding <@ r_fst) (nsepseq_to_nseq bindings)
         in
         D_multi_const bindings)
      ~attr:(fun attributes -> I.SConst { l with value = { l.value with attributes } })
  | SType { value = { attributes; name; params; type_expr; _ } as v; region } ->
    return_attr
      attributes
      ~no_attr:
        (let name = TODO_do_in_parsing.tvar name in
         let params =
           Option.map
             ~f:(fun tv ->
               List.Ne.map TODO_do_in_parsing.tvar (nsepseq_to_nseq (r_fst tv).inside))
             params
         in
         D_type_abstraction { name; params; type_expr })
      ~attr:(fun attributes -> SType { value = { v with attributes }; region })
  (* impossible, if triggered, look at functions 'statement' *)
  | _ -> assert false


and program_entry : Eq.program_entry -> Folding.program_entry = function
  | I.TopLevel (s, _) ->
    (match Location.unwrap @@ statement s with
    | O.S_decl _ -> PE_declaration s
    | O.S_instr _ -> PE_top_level_instruction s
    | _ -> assert false)
  | I.Directive _ -> PE_preproc_directive ()


and program : Eq.program -> Folding.program = fun x -> List.Ne.to_list x.statements

and sig_expr : Eq.sig_expr -> Folding.sig_expr = function
  | IInterface { value = { inside; lbrace = _; rbrace = _ }; region } ->
    let loc = Location.lift region in
    let sig_items = nsepseq_to_list inside in
    Location.wrap ~loc @@ O.S_body sig_items
  | IPath { value; region } ->
    let loc = Location.lift region in
    let value = nsepseq_to_nseq value in
    let value = List.Ne.map TODO_do_in_parsing.mvar value in
    Location.wrap ~loc @@ O.S_path value


and sig_entry : Eq.sig_entry -> Folding.sig_entry =
 fun se ->
  let return ~loc = Location.wrap ~loc in
  let return_attr attributes ~loc ~attr ~no_attr =
    match attributes with
    | [] -> return ~loc @@ no_attr
    | hd :: tl ->
      let hd = TODO_do_in_parsing.conv_attr hd in
      return ~loc @@ (S_attr (hd, attr tl) : _ O.sig_entry_content_)
  in
  match se with
  | IType { region; value = attributes, kwd_type, v, equal, ty } ->
    let loc = Location.lift region in
    return_attr
      attributes
      ~loc
      ~no_attr:(O.S_type (TODO_do_in_parsing.tvar v, ty))
      ~attr:(fun attributes ->
        I.IType { region; value = attributes, kwd_type, v, equal, ty })
  | IType_var { region; value = attributes, kwd_type, v } ->
    let loc = Location.lift region in
    return_attr
      attributes
      ~loc
      ~no_attr:(O.S_type_var (TODO_do_in_parsing.tvar v))
      ~attr:(fun attributes -> I.IType_var { region; value = attributes, kwd_type, v })
  | IConst { region; value = attributes, kwd_type, v, equal, ty } ->
    let loc = Location.lift region in
    return_attr
      attributes
      ~loc
      ~no_attr:(O.S_value (TODO_do_in_parsing.var v, ty))
      ~attr:(fun attributes ->
        I.IConst { region; value = attributes, kwd_type, v, equal, ty })
