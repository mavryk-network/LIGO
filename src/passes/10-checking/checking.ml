module Trace = Simple_utils.Trace
open Trace
module Errors = Errors
open Errors
module I = Ast_core
module O = Ast_typed
open O.Combinators
open Subtyping
module TypeVar = Stage_common.Types.TypeVar
module Pair = Simple_utils.Pair

type context = Context.t

let untype_expression = Untyper.untype_expression
let untype_program = Untyper.untype_program
let assert_type_expression_eq = Helpers.assert_type_expression_eq

(*
  This function operates on the return type of Context.get_sum.
  If type match the constructor label and its argument type, warns user about ambiguous constructor
*)
let warn_ambiguous_constructor ~raise loc (var_chosen, c_arg_t) ignored =
  let ignored_match =
    List.find
      ~f:(fun (_, _, a, _) ->
        Option.is_some (O.Misc.assert_type_expression_eq (c_arg_t, a)))
      ignored
  in
  match ignored_match with
  | Some (var_ignored, _, _, _) ->
    raise.warning (`Checking_ambiguous_constructor (loc, var_chosen, var_ignored))
  | None -> ()


let t_subst t ~tvar ~type_ = O.Helpers.subst_no_capture_type tvar type_ t

let t_exists (evar : Exists_var.t) =
  t_variable ~loc:(Exists_var.loc evar) (evar :> O.type_variable) ()


let t_subst_var t ~tvar ~tvar' = t_subst t ~tvar ~type_:(t_variable tvar' ())
let t_subst_evar t ~tvar ~evar = t_subst t ~tvar ~type_:(t_exists evar)

let rec evaluate_type ~raise ~(ctx : Context.t) (type_ : I.type_expression)
  : O.type_expression
  =
  let self ?(ctx = ctx) = evaluate_type ~raise ~ctx in
  let return content = make_t ~loc:type_.location content (Some type_) in
  match type_.type_content with
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow { type1; type2 }
  | T_sum m ->
    let rows =
      let layout = Option.value ~default:default_layout m.layout in
      let aux ({ associated_type; michelson_annotation; decl_pos } : I.row_element) =
        let associated_type = self associated_type in
        ({ associated_type; michelson_annotation; decl_pos } : O.row_element)
      in
      let content = O.LMap.map aux m.fields in
      O.{ content; layout }
    in
    return @@ T_sum rows
  | T_record m ->
    let rows =
      let layout = Option.value ~default:default_layout m.layout in
      let aux ({ associated_type; michelson_annotation; decl_pos } : I.row_element) =
        let associated_type = self associated_type in
        ({ associated_type; michelson_annotation; decl_pos } : O.row_element)
      in
      let content = O.LMap.map aux m.fields in
      O.{ content; layout }
    in
    return @@ T_record rows
  | T_variable name ->
    (match Context.get_type ctx name with
     | Some x -> x
     | None -> raise.error (unbound_type_variable name type_.location))
  | T_app { type_operator; arguments } ->
    (* TODO: Remove strong normalization (GA) *)
    let operator =
      trace_option ~raise (unbound_type_variable type_operator type_.location)
      @@ Context.get_type ctx type_operator
    in
    let is_fully_applied location (t : O.type_expression) =
      match t.type_content with
      | T_abstraction x ->
        let rec aux : O.type_expression * int -> O.type_expression * int =
         fun (t, i) ->
          match t.type_content with
          | T_abstraction x -> aux (x.type_, i + 1)
          | _ -> t, i
        in
        let expected = snd @@ aux (x.type_, 1) in
        raise.error (type_app_wrong_arity None expected 0 location)
      | _ -> ()
    in
    let aux : I.type_expression -> O.type_expression =
     fun t ->
      let t' = self t in
      is_fully_applied t.location t';
      t'
    in
    let arguments = List.map ~f:aux arguments in
    let vars, ty_body = O.Helpers.destruct_type_abstraction operator in
    let vargs =
      match List.zip vars arguments with
      | Unequal_lengths ->
        let actual = List.length arguments in
        let expected = List.length vars in
        raise.error
          (type_app_wrong_arity (Some type_operator) expected actual type_.location)
      | Ok x -> x
    in
    let res =
      (* Note:
        Currently, there is no way for ty_body to look like `fun 'a 'b -> forall 'a 'b . <some type>` `fun 'a 'b -> 'a * (fun 'b -> <type>)`
        so it is fine to use `psubst_type`. If this changes, we should use `subst_type` and capture the FV in the right element of vargs *)
      let table = O.Helpers.TMap.of_list vargs in
      O.Helpers.psubst_type table ty_body
    in
    return res.type_content
  | T_module_accessor { module_path; element } ->
    let f acc el =
      trace_option
        ~raise
        (unbound_module_variable el type_.location)
        (Context.get_module acc el)
    in
    let module_ = List.fold ~init:ctx ~f module_path in
    trace_option
      ~raise
      (unbound_type_variable element type_.location)
      (Context.get_type module_ element)
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x ->
    let ctx = Context.add_type_var ctx x.ty_binder x.kind in
    let type_ = self ~ctx x.type_ in
    return @@ T_abstraction { x with type_ }
  | T_for_all x ->
    let ctx = Context.add_type_var ctx x.ty_binder x.kind in
    let type_ = self ~ctx x.type_ in
    return @@ T_for_all { x with type_ }


let infer_literal ~loc lit =
  let return type_ = type_, O.make_e ~location:loc (E_literal lit) type_ in
  match lit with
  | Literal_unit -> return @@ t_unit ()
  | Literal_string _ -> return @@ t_string ()
  | Literal_key _ -> return @@ t_key ()
  | Literal_key_hash _ -> return @@ t_key_hash ()
  | Literal_chain_id _ -> return @@ t_chain_id ()
  | Literal_signature _ -> return @@ t_signature ()
  | Literal_bytes _ -> return @@ t_bytes ()
  | Literal_int _ -> return @@ t_int ()
  | Literal_nat _ -> return @@ t_nat ()
  | Literal_timestamp _ -> return @@ t_timestamp ()
  | Literal_mutez _ -> return @@ t_mutez ()
  | Literal_address _ -> return @@ t_address ()
  | Literal_operation _ -> return @@ t_operation ()
  | Literal_bls12_381_g1 _ -> return @@ t_bls12_381_g1 ()
  | Literal_bls12_381_g2 _ -> return @@ t_bls12_381_g2 ()
  | Literal_bls12_381_fr _ -> return @@ t_bls12_381_fr ()
  | Literal_chest _ | Literal_chest_key _ ->
    failwith "chest / chest_key not allowed in the syntax (only tests need this type)"


let equal_lmap_doms lmap1 lmap2 =
  let open O in
  let dom lmap = LSet.of_list (LMap.keys lmap) in
  LSet.equal (dom lmap1) (dom lmap2)


type nonrec raise = (Errors.typer_error, Main_warnings.all) raise

let rec check_expression
  ~(raise : raise)
  ~options
  ~ctx
  (expr : I.expression)
  (type_ : O.type_expression)
  : Context.t * O.expression
  =
  Context.Hashes.set_context ctx;
  let loc = expr.location in
  let return content = O.make_e ~location:loc content type_ in
  let check ?(raise = raise) ?(options = options) ?(ctx = ctx) expr type_ =
    check_expression ~raise ~options ~ctx expr type_
  and infer ?(raise = raise) ?(options = options) ?(ctx = ctx) expr =
    infer_expression ~raise ~options ~ctx expr
  in
  match expr.expression_content, type_.type_content with
  | E_literal lit, _ ->
    let lit_type, expr = infer_literal ~loc:expr.location lit in
    Assert.assert_true
      ~raise
      (assert_equal loc lit_type type_)
      (O.type_expression_eq (lit_type, type_));
    ctx, expr
  | _, T_for_all { ty_binder = tvar; kind; type_ } ->
    let tvar' = TypeVar.fresh_like tvar in
    let ctx, result =
      check
        ~ctx:Context.(ctx |:: C_type_var (tvar', kind))
        expr
        (t_subst_var type_ ~tvar ~tvar')
    in
    ( Context.drop_until ctx ~at:(C_type_var (tvar', kind))
    , return @@ E_type_abstraction { type_binder = tvar; result } )
  | E_lambda lambda, T_arrow { type1 = arg_type; type2 = ret_type } ->
    let ctx, lambda = check_lambda ~raise ~options ~ctx lambda arg_type ret_type in
    ctx, return @@ E_lambda lambda
  | E_record record, T_record row ->
    (* Check domain of record and row are consistent *)
    if not (equal_lmap_doms record row.content) then raise.error (assert false);
    (* Type check record using row *)
    let ctx, record =
      O.LMap.fold_map record ~init:ctx ~f:(fun label expr ctx ->
        let expr_type = O.LMap.find label row.content in
        check ~ctx expr expr_type.associated_type)
    in
    ctx, return @@ E_record record
  | E_record_update { record; path; update }, T_record row ->
    let ctx, record = check ~ctx record type_ in
    let field_row_elem =
      trace_option ~raise (assert false) @@ O.LMap.find_opt path row.content
    in
    let ctx, update = check ~ctx update field_row_elem.associated_type in
    ctx, return @@ E_record_update { record; path; update }
  | E_constructor { constructor; element }, T_sum row ->
    (* Find row element *)
    let constructor_row_elem =
      trace_option ~raise (assert false) @@ O.LMap.find_opt constructor row.content
    in
    (* Type check element *)
    let ctx, element = check ~ctx element constructor_row_elem.associated_type in
    ctx, return @@ E_constructor { constructor; element }
  | E_matching { matchee; cases }, _ ->
    (* Infer type of matchee *)
    let ctx, matchee_type, matchee = infer ~ctx matchee in
    (* Type check the match cases *)
    let ctx, cases = check_cases ~raise ~options ~ctx cases matchee_type type_ in
    (* Elaborate (by compiling pattern) *)
    ctx, return @@ compile_match ~raise ~options ~loc ~ctx matchee cases matchee_type
  | _ ->
    let ctx, type_', expr = infer expr in
    let ctx, f =
      subtype
        ~raise
        ~ctx
        ~recieved:(Context.apply ctx type_')
        ~expected:(Context.apply ctx type_)
    in
    ctx, f expr


and infer_expression ~(raise : raise) ~options ~ctx (expr : I.expression)
  : Context.t * O.type_expression * O.expression
  =
  Context.Hashes.set_context ctx;
  let loc = expr.location in
  let return content type_ = O.make_e ~location:loc content type_ in
  let lift (expr : O.expression) = return expr.expression_content expr.type_expression in
  let check ?(raise = raise) ?(options = options) ?(ctx = ctx) expr type_ =
    check_expression ~raise ~options ~ctx expr type_
  and infer ?(raise = raise) ?(options = options) ?(ctx = ctx) expr =
    infer_expression ~raise ~options ~ctx expr
  in
  match expr.expression_content with
  | E_literal lit ->
    let type_, expr = infer_literal ~loc lit in
    ctx, type_, expr
  | E_constant { cons_name = const; arguments = args } ->
    infer_constant ~raise ~options ~ctx ~loc const args
  | E_variable var ->
    let type_ =
      trace_option ~raise (unbound_variable var expr.location)
      @@ Context.get_value ctx var
    in
    ctx, type_, return (E_variable var) type_
  | E_lambda lambda ->
    let ctx, type_, lambda = infer_lambda ~raise ~options ~ctx lambda in
    ctx, type_, return (E_lambda lambda) type_
  | E_application { lamb; args } ->
    let ctx, lamb_type, lamb = infer lamb in
    let ctx, ret_type, f, args = infer_application ~raise ~options ~ctx lamb_type args in
    ctx, ret_type, return (E_application { lamb = f lamb; args }) ret_type
  | E_type_abstraction { type_binder = tvar; result } ->
    Context.Generalization.enter ~ctx ~in_:(fun ctx ->
      let ctx, ret_type, result =
        infer ~ctx:Context.(ctx |:: C_type_var (tvar, Type)) result
      in
      let ret_type = O.t_for_all tvar Type ret_type in
      ctx, ret_type, return (E_type_abstraction { type_binder = tvar; result }) ret_type)
  | E_let_in { let_binder = { var; ascr = rhs_ascr; attributes }; rhs; let_result; attr }
    ->
    let rhs =
      Option.value_map
        rhs_ascr
        ~f:(fun rhs_ascr -> I.e_ascription rhs rhs_ascr)
        ~default:rhs
    in
    let ctx, rhs_type, rhs = infer rhs in
    let rhs_type = Context.apply ctx rhs_type in
    let ctx, res_type, let_result =
      Context.enter
        ~ctx
        ~at:(C_value (var, rhs_type))
        ~in_:(fun ctx -> infer ~ctx:Context.(ctx |:: C_value (var, rhs_type)) let_result)
    in
    ( ctx
    , res_type
    , return
        (E_let_in
           { let_binder = { var; ascr = Some rhs_type; attributes }
           ; rhs
           ; let_result
           ; attr
           })
        res_type )
  | E_type_in { type_binder = tvar; rhs; let_result } ->
    let rhs = evaluate_type ~raise ~ctx rhs in
    let ctx, res_type, let_result =
      infer ~ctx:Context.(ctx |:: C_type (tvar, rhs)) let_result
    in
    ctx, res_type, lift let_result
  | E_raw_code { language; code } ->
    let ctx, code_type, code = infer code in
    ctx, code_type, return (E_raw_code { language; code }) code_type
  | E_ascription { anno_expr; type_annotation } ->
    let ascr = evaluate_type ~raise ~ctx type_annotation in
    let ctx, expr = check anno_expr ascr in
    ctx, ascr, lift expr
  | E_recursive { fun_name; fun_type; lambda } ->
    let fun_type = evaluate_type ~raise ~ctx fun_type in
    let ctx, lambda =
      check
        ~ctx:Context.(ctx |:: C_value (fun_name, fun_type))
        (I.make_e (E_lambda lambda))
        fun_type
    in
    let lambda =
      trace_option ~raise (corner_case "Annotated lambda should return lambda")
      @@ O.get_e_lambda_opt lambda
    in
    ctx, fun_type, return (E_recursive { fun_name; fun_type; lambda }) fun_type
  | E_record record ->
    let (ctx, row_content), record =
      O.LMap.fold_map
        record
        ~init:(ctx, O.LMap.empty)
        ~f:(fun label expr (ctx, row_content) ->
        let ctx, expr_type, expr = infer ~ctx expr in
        let row_content = O.LMap.add label expr_type row_content in
        (ctx, row_content), expr)
    in
    let _, row =
      (* No fold_mapi in utils :cry *)
      O.LMap.fold_map row_content ~init:0 ~f:(fun label associated_type i ->
        let decl_pos =
          let (Label str) = label in
          match Int.of_string str with
          | i -> i
          | exception _ -> i
        in
        i + 1, { O.associated_type; michelson_annotation = None; decl_pos })
    in
    let record_type =
      match Context.get_record row ctx with
      | None -> t_record ~layout:default_layout row
      | Some (orig_var, row) -> make_t_orig_var (T_record row) None orig_var
    in
    ctx, record_type, return (E_record record) record_type
  | E_record_accessor { record; path = field } ->
    let ctx, record_type, record = infer ~ctx record in
    let row =
      trace_option ~raise (expected_record loc record_type) @@ get_t_record record_type
    in
    let field_row_elem =
      trace_option ~raise (bad_record_access field record loc)
      @@ O.LMap.find_opt field row.content
    in
    let field_type = field_row_elem.associated_type in
    ctx, field_type, return (E_record_accessor { record; path = field }) field_type
  | E_record_update { record; path; update } ->
    let ctx, record_type, record = infer ~ctx record in
    let row =
      trace_option ~raise (expected_record loc record_type) @@ get_t_record record_type
    in
    let field_row_elem =
      trace_option ~raise (bad_record_access path record loc)
      @@ O.LMap.find_opt path row.content
    in
    let ctx, update = check ~ctx update field_row_elem.associated_type in
    ctx, record_type, return (E_record_update { record; path; update }) record_type
  | E_constructor { constructor = Label label; _ }
    when String.(label = "M_right" || label = "M_left") -> raise.error (assert false)
  | E_constructor { constructor; element = arg } ->
    (* [tvars] are the parameters of the type *)
    let tvars, arg_type, sum_type =
      match Context.get_sum constructor ctx with
      | (tvar, tvars, arg_type, sum_type) :: ignored ->
        warn_ambiguous_constructor ~raise loc (tvar, arg_type) ignored;
        tvars, arg_type, sum_type
      | [] -> raise.error (unbound_constructor constructor loc)
    in
    let module TMap = O.Helpers.TMap in
    (* Instantiate [tvars] (assumption: kind is [Type]) *)
    let tvars : Exists_var.t TMap.t =
      tvars |> List.map ~f:(fun tvar -> tvar, Exists_var.fresh ()) |> TMap.of_list
    in
    let ctx =
      Context.(
        ctx
        |@ of_list
             (TMap.values tvars |> List.map ~f:(fun evar -> C_exists_var (evar, Type))))
    in
    let arg_type =
      TMap.fold
        (fun tvar evar arg_type -> t_subst_evar arg_type ~tvar ~evar)
        tvars
        arg_type
    in
    let sum_type =
      TMap.fold
        (fun tvar evar sum_type -> t_subst_evar sum_type ~tvar ~evar)
        tvars
        sum_type
    in
    (* Check argument *)
    let ctx, arg = check ~ctx arg arg_type in
    ctx, sum_type, return (E_constructor { constructor; element = arg }) sum_type
  | E_matching { matchee; cases } ->
    (* Infer type of matchee *)
    let ctx, matchee_type, matchee = infer ~ctx matchee in
    (* Add existential for return type *)
    let evar = Exists_var.fresh ~loc () in
    let ctx = Context.(ctx |:: C_exists_var (evar, Type)) in
    let ret_type = t_exists evar in
    (* Type check the match cases *)
    let ctx, cases = check_cases ~raise ~options ~ctx cases matchee_type ret_type in
    (* Elaborate (by compiling pattern) *)
    ( Context.drop_until ctx ~at:(C_exists_var (evar, Type))
    , ret_type
    , return (compile_match ~raise ~options ~loc ~ctx matchee cases matchee_type) ret_type
    )
  | E_mod_in { module_binder = mvar; rhs; let_result } ->
    let mctx, rhs = infer_module_expr ~raise ~options ~ctx rhs in
    let ctx, ret_type, let_result =
      Context.enter
        ~ctx
        ~at:(C_module (mvar, mctx))
        ~in_:(fun ctx -> infer ~ctx:Context.(ctx |:: C_module (mvar, mctx)) let_result)
    in
    ctx, ret_type, return (E_mod_in { module_binder = mvar; rhs; let_result }) ret_type
  | E_module_accessor { module_path; element } ->
    let mctx =
      List.fold module_path ~init:ctx ~f:(fun ctx mvar ->
        trace_option
          ~raise
          (unbound_module_variable mvar (I.ModuleVar.get_location mvar))
          (Context.get_module ctx mvar))
    in
    let elt_type =
      trace_option ~raise (unbound_variable element loc) @@ Context.get_value mctx element
    in
    ctx, elt_type, return (E_module_accessor { module_path; element }) elt_type
  | E_assign { binder = { var; _ } as binder; expression } ->
    let type_ =
      trace_option ~raise (unbound_variable binder.var (O.ValueVar.get_location var))
      @@ Context.get_value ctx binder.var
    in
    let binder = { binder with ascr = Some type_ } in
    let ctx, expression = check ~ctx expression type_ in
    let ret_type = O.t_unit () in
    ctx, ret_type, return (E_assign { binder; expression }) ret_type


and infer_constant ~(raise : raise) ~options ~ctx ~loc const args =
  let return args type_ =
    O.make_e ~location:loc (E_constant { cons_name = const; arguments = args }) type_
  in
  let ctx, args, ret_type =
    Constant_typers.infer_constant
      ~raise
      ~options
      ~infer:(infer_expression ~options)
      ~check:(check_expression ~options)
      ~ctx
      ~loc
      const
      args
  in
  ctx, ret_type, return args ret_type


and check_lambda
  ~raise
  ~options
  ~ctx
  ({ binder; output_type = ret_ascr; result } : _ I.lambda)
  arg_type
  ret_type
  : Context.t * O.lambda
  =
  let ({ var; ascr = arg_ascr; _ } : _ I.binder) = binder in
  let result =
    Option.value_map ret_ascr ~default:result ~f:(fun ret_ascr ->
      I.e_ascription result ret_ascr)
  in
  let ctx, f =
    match arg_ascr with
    | Some arg_ascr ->
      let arg_ascr = evaluate_type ~raise ~ctx arg_ascr in
      (* TODO: Kinding check for ascription *)
      let ctx, f = subtype ~raise ~ctx ~recieved:arg_type ~expected:arg_ascr in
      (* Generate let binding for ascription subtyping, will be inlined later on *)
      ( ctx
      , fun hole ->
          O.e_a_let_in
            { var; ascr = Some arg_ascr; attributes = { const_or_var = None } }
            (f (O.e_variable var arg_type))
            hole
            { inline = true
            ; no_mutation = true
            ; view = false
            ; public = false
            ; hidden = true
            } )
    | None -> ctx, fun x -> x
  in
  let arg_type = Context.apply ctx arg_type in
  let ret_type = Context.apply ctx ret_type in
  let ctx, result =
    check_expression
      ~raise
      ~options
      ~ctx:Context.(ctx |:: C_value (var, arg_type))
      result
      ret_type
  in
  ( Context.drop_until ctx ~at:(C_value (var, arg_type))
  , { binder = { binder with ascr = Some arg_type }; result = f result } )


and infer_lambda
  ~raise
  ~options
  ~ctx
  ({ binder; output_type = ret_ascr; result } : _ I.lambda)
  : Context.t * O.type_expression * O.lambda
  =
  let ({ var; ascr = arg_ascr; _ } : _ I.binder) = binder in
  (* Desugar return ascription to (result : ret_ascr) *)
  let result =
    Option.value_map ret_ascr ~default:result ~f:(fun ret_ascr ->
      I.e_ascription result ret_ascr)
  in
  Context.Generalization.enter ~ctx ~in_:(fun ctx ->
    let ctx, arg_type =
      match arg_ascr with
      | Some arg_ascr -> ctx, evaluate_type ~raise ~ctx arg_ascr
      | None ->
        let evar = Exists_var.fresh () in
        Context.(ctx |:: C_exists_var (evar, Type)), t_exists evar
    in
    let ctx, ret_type, result =
      infer_expression
        ~raise
        ~options
        ~ctx:Context.(ctx |:: C_value (var, arg_type))
        result
    in
    ( ctx
    , O.t_arrow arg_type ret_type ()
    , { binder = { binder with ascr = Some arg_type }; result } ))


and infer_application ~raise ~options ~ctx lamb_type args =
  let self = infer_application ~raise ~options in
  let check = check_expression ~raise ~options in
  let fail () = assert false in
  match lamb_type.type_content with
  | T_for_all { ty_binder = tvar; kind; type_ } ->
    let evar = Exists_var.fresh () in
    let lamb_type = t_subst type_ ~tvar ~type_:(t_exists evar) in
    let ctx, ret_type, f, args =
      self ~ctx:Context.(ctx |:: C_exists_var (evar, kind)) lamb_type args
    in
    ( ctx
    , ret_type
    , (fun hole -> f (O.e_type_inst { forall = hole; type_ = t_exists evar } lamb_type))
    , args )
  | T_arrow { type1 = arg_type; type2 = ret_type } ->
    let ctx, args = check ~ctx args arg_type in
    ctx, ret_type, (fun hole -> hole), args
  | T_variable tvar ->
    (match Exists_var.of_type_var tvar with
     | None -> fail ()
     | Some evar ->
       let kind =
         Context.get_exists_var ctx evar
         |> trace_option ~raise (unbound_exists_variable (Exists_var.loc evar) evar)
       in
       if not
            (match kind with
             | Type -> true
             | _ -> false)
       then raise.error (assert false);
       let evar1 = Exists_var.fresh () in
       let evar2 = Exists_var.fresh () in
       let arg_type = t_exists evar1 in
       let ret_type = t_exists evar2 in
       let hole =
         Context.of_list
           [ C_exists_var (evar1, Type)
           ; C_exists_var (evar2, Type)
           ; C_exists_eq (evar, Type, O.t_arrow arg_type ret_type ())
           ]
       in
       let ctx, args =
         check
           ~ctx:Context.(insert_at ctx ~at:(C_exists_var (evar, Type)) ~hole)
           args
           arg_type
       in
       ctx, ret_type, (fun hole -> hole), args)
  | _ -> fail ()


and check_pattern
  ~raise
  ~ctx
  (pat : I.type_expression I.pattern)
  (type_ : O.type_expression)
  : Context.t * O.type_expression O.pattern
  =
  let loc = pat.location in
  let err () = pattern_do_not_conform_type pat type_ in
  let fail () = raise.error (err ()) in
  let return content = Location.wrap ~loc content in
  let self ?(raise = raise) = check_pattern ~raise in
  match pat.wrap_content, type_.type_content with
  | I.P_unit, O.T_constant { injection = Stage_common.Constant.Unit; _ } ->
    ctx, return @@ O.P_unit
  | I.P_unit, _ -> fail ()
  | I.P_var ({ var; _ } as binder), _ ->
    ( Context.(ctx |:: C_value (var, type_))
    , return @@ O.P_var { binder with ascr = Some type_ } )
  | ( I.P_list (I.Cons (hd_pat, tl_pat))
    , O.T_constant
        { injection = Stage_common.Constant.List; parameters = [ elt_type ]; _ } ) ->
    let ctx, hd_pat = self ~ctx hd_pat elt_type in
    let ctx, tl_pat = self ~ctx tl_pat type_ in
    ctx, return @@ O.P_list (O.Cons (hd_pat, tl_pat))
  | ( I.P_list (I.List list_pat)
    , O.T_constant
        { injection = Stage_common.Constant.List; parameters = [ elt_type ]; _ } ) ->
    let ctx, list_pat =
      List.fold_right list_pat ~init:(ctx, []) ~f:(fun elt (ctx, list_pat) ->
        let ctx, elt = self ~ctx elt elt_type in
        ctx, elt :: list_pat)
    in
    ctx, return @@ O.P_list (O.List list_pat)
  | I.P_variant (label, arg_pat), O.T_sum row ->
    let label_row_elem =
      trace_option ~raise (err ()) @@ O.LMap.find_opt label row.content
    in
    let ctx, arg_pat = self ~ctx arg_pat label_row_elem.associated_type in
    ctx, return @@ O.P_variant (label, arg_pat)
  | I.P_tuple tuple_pat, O.T_record row ->
    if O.LMap.cardinal row.content <> List.length tuple_pat then raise.error (fail ());
    let ctx, tuple_pat =
      List.fold_mapi tuple_pat ~init:ctx ~f:(fun i ctx pat ->
        let pat_row_elem =
          trace_option ~raise (err ())
          @@ O.LMap.find_opt (Label (Int.to_string i)) row.content
        in
        self ~ctx pat pat_row_elem.associated_type)
    in
    ctx, return @@ O.P_tuple tuple_pat
  | I.P_record (labels, pats), O.T_record row ->
    if O.LMap.cardinal row.content <> List.length labels then raise.error (fail ());
    let record_pat =
      match List.zip labels pats with
      | Ok record_pat -> record_pat
      | Unequal_lengths ->
        raise.error (corner_case "Mismatch between labels and patterns")
    in
    let ctx, record_pat =
      List.fold_map record_pat ~init:ctx ~f:(fun ctx (label, pat) ->
        let label_row_elem =
          trace_option ~raise (err ()) @@ O.LMap.find_opt label row.content
        in
        let ctx, pat = self ~ctx pat label_row_elem.associated_type in
        ctx, (label, pat))
    in
    let labels, pats = List.unzip record_pat in
    ctx, return @@ O.P_record (labels, pats)
  | _ -> raise.error (fail ())


and check_cases
  ~raise
  ~options
  ~ctx
  (cases : (I.expression, I.type_expression) I.match_case list)
  matchee_type
  ret_type
  : Context.t * (O.type_expression O.pattern * O.expression) list
  =
  List.fold_map ~init:ctx cases ~f:(fun ctx { pattern; body } ->
    let marker : Context.item = C_marker (Exists_var.fresh ()) in
    let ctx, pattern =
      check_pattern ~raise ~ctx:Context.(ctx |:: marker) pattern matchee_type
    in
    let ctx, body = check_expression ~raise ~options ~ctx body ret_type in
    Context.drop_until ctx ~at:marker, (pattern, body))


and compile_match ~raise ~options ~loc ~ctx (matchee : O.expression) cases matchee_type =
  (* Check anomalies *)
  let eqs =
    (* Apply to [matchee_type] removes as many variables as possible *)
    let matchee_type = Context.apply ctx matchee_type in
    List.map cases ~f:(fun (pat, body) -> pat, matchee_type, body)
  in
  let () =
    Pattern_anomalies.check_anomalies
      ~raise
      ~syntax:options.syntax_for_errors
      ~loc
      eqs
      matchee_type
  in
  (* Elaborate (by compiling pattern) *)
  match matchee.expression_content with
  | E_variable var ->
    let match_expr = Pattern_matching.compile_matching ~raise ~err_loc:loc var eqs in
    match_expr.expression_content
  | _ ->
    let var = I.ValueVar.fresh () in
    let match_expr = Pattern_matching.compile_matching ~raise ~err_loc:loc var eqs in
    O.E_let_in
      { let_binder = { var; ascr = None; attributes = { const_or_var = Some `Var } }
      ; rhs = matchee
      ; let_result = { match_expr with location = loc }
      ; attr =
          { inline = false
          ; no_mutation = false
          ; public = true
          ; view = false
          ; hidden = false
          }
      }


and infer_module_expr ~raise ~options ~ctx (mod_expr : I.module_expr)
  : Context.t * O.module_expr
  =
  let loc = mod_expr.location in
  let return content =
    let mod_expr = Location.wrap ~loc content in
    Context.context_of_module_expr ~outer_context:ctx mod_expr, mod_expr
  in
  let access_module ctx mvar =
    trace_option
      ~raise
      (unbound_module_variable mvar (I.ModuleVar.get_location mvar))
      (Context.get_module ctx mvar)
  in
  match mod_expr.wrap_content with
  | I.M_struct decls ->
    let decls = infer_module ~raise ~options ~ctx decls in
    return (O.M_struct decls)
  | I.M_module_path path ->
    (* Check we can access every element in [path] *)
    let (_ : Context.t) = List.fold ~f:access_module ~init:ctx (List.Ne.to_list path) in
    return (O.M_module_path path)
  | I.M_variable mvar ->
    (* Check we can access [mvar] *)
    let (_ : Context.t) = access_module ctx mvar in
    return (O.M_variable mvar)


and infer_declaration ~(raise : raise) ~options ~ctx (decl : I.declaration)
  : Context.t * O.declaration
  =
  let loc = decl.location in
  let return (content : O.declaration_content) = Location.wrap ~loc content in
  match decl.wrap_content with
  | Declaration_type { type_binder; type_expr; type_attr = { public; hidden } } ->
    let type_expr = evaluate_type ~raise ~ctx type_expr in
    let type_expr = { type_expr with orig_var = Some type_binder } in
    let ctx = Context.add_type ctx type_binder type_expr in
    ( ctx
    , return
      @@ Declaration_type { type_binder; type_expr; type_attr = { public; hidden } } )
  | Declaration_constant { binder = { ascr; var; attributes }; attr; expr } ->
    let expr =
      Option.value_map ascr ~default:expr ~f:(fun ascr -> I.e_ascription expr ascr)
    in
    let ascr = Option.map ascr ~f:(evaluate_type ~raise ~ctx) in
    let ctx, expr_type, expr =
      trace ~raise (constant_declaration_tracer loc var expr ascr)
      @@ infer_expression ~options ~ctx expr
    in
    let ctx = Context.(ctx |:: C_value (var, expr_type)) in
    ( ctx
    , return
      @@ Declaration_constant
           { binder = { ascr = Some expr_type; var; attributes }; expr; attr } )
  | Declaration_module { module_binder; module_; module_attr = { public; hidden } } ->
    let module_ctx, module_ = infer_module_expr ~raise ~options ~ctx module_ in
    let ctx = Context.add_module ctx module_binder module_ctx in
    ( ctx
    , return
      @@ Declaration_module { module_binder; module_; module_attr = { public; hidden } } )


and infer_module ~raise ~options ~ctx (module_ : I.module_) : O.module_ =
  (* This context use all the declaration so you can use private declaration to type the module. 
    It should not be returned*)
  let _ctx, module_ =
    List.fold_map
      ~f:(fun ctx decl -> infer_declaration ~raise ~options ~ctx decl)
      ~init:ctx
      module_
  in
  module_


let type_program ~raise ~options ?env module_ =
  let ctx = Context.init ?env () in
  infer_module ~raise ~options ~ctx module_


let type_declaration ~raise ~options ?env decl =
  let ctx = Context.init ?env () in
  let _ctx, decl = infer_declaration ~raise ~options ~ctx decl in
  decl


let type_expression ~raise ~options ?env ?tv_opt expr =
  let ctx = Context.init ?env () in
  match tv_opt with
  | Some type_ ->
    let _ctx, expr = check_expression ~raise ~options ~ctx expr type_ in
    expr
  | None ->
    let _ctx, _type, expr = infer_expression ~raise ~options ~ctx expr in
    expr
