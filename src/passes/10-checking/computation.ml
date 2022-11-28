module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
open Trace
open Errors
module List = Simple_utils.List
open Ligo_prim

module State = struct
  type t = Context.t * Substitution.t
end

type ('a, 'err, 'wrn) t =
  raise:('err, 'wrn) raise
  -> options:Compiler_options.middle_end
  -> loc:Location.t
  -> State.t
  -> State.t * 'a

let rec encode (type_ : Ast_typed.type_expression) : Type.t =
  let return content : Type.t =
    { content
    ; meta = type_.type_meta
    ; orig_var = type_.orig_var
    ; location = type_.location
    }
  in
  match type_.type_content with
  | T_variable tvar -> return @@ T_variable tvar
  | T_arrow arr ->
    let arr = Arrow.map encode arr in
    return @@ T_arrow arr
  | T_singleton lit -> return @@ T_singleton lit
  | T_abstraction abs ->
    let abs = Abstraction.map encode abs in
    return @@ T_abstraction abs
  | T_for_all abs ->
    let abs = Abstraction.map encode abs in
    return @@ T_for_all abs
  | T_constant { language; injection; parameters } ->
    let parameters = List.map parameters ~f:encode in
    return @@ T_construct { language; constructor = injection; parameters }
  | T_sum row ->
    let row = encode_row row in
    return @@ T_sum row
  | T_record row ->
    let row = encode_row row in
    return @@ T_record row


and encode_row ({ fields; layout } : Ast_typed.rows) : Type.row =
  let fields = Record.map ~f:encode_row_elem fields in
  let layout = encode_layout layout in
  { Type.fields; layout }


and encode_row_elem (row_elem : Ast_typed.row_element) : Type.row_element =
  Rows.map_row_element_mini_c encode row_elem


and encode_layout (layout : Layout.t) : Type.layout =
  match layout with
  | L_tree -> L_tree
  | L_comb -> L_comb


let rec signature_of_module_expr
    : ctx:Context.t -> Ast_typed.module_expr -> Context.Signature.t
  =
 fun ~ctx mod_expr ->
  match mod_expr.wrap_content with
  | M_struct decls -> signature_of_module ~ctx decls
  | M_variable mvar ->
    (match Context.get_module ctx mvar with
    | Some sig_ -> sig_
    | None -> failwith "Unbounded module")
  | M_module_path path ->
    (match Context.get_signature ctx path with
    | Some sig_ -> sig_
    | None ->
      Format.kasprintf
        failwith
        "Unbounded signature path: %a"
        Module_expr.pp_module_path
        path)


and signature_of_module : ctx:Context.t -> Ast_typed.module_ -> Context.Signature.t =
 fun ~ctx module_ ->
  match module_ with
  | [] -> []
  | decl :: module_ ->
    let public, sig_decl = signature_item_of_decl ~ctx decl in
    let sig_ =
      signature_of_module ~ctx:(Context.add_signature_items ctx sig_decl) module_
    in
    if public then sig_decl @ sig_ else sig_


and signature_item_of_decl : ctx:Context.t -> Ast_typed.decl -> bool * Context.Signature.t
  =
 fun ~ctx decl ->
  match Location.unwrap decl with
  | D_value { binder; expr; attr = { public; _ } } ->
    public, [ S_value (Binder.get_var binder, encode expr.type_expression) ]
  | D_type { type_binder = tvar; type_expr = type_; type_attr = { public; _ } } ->
    public, [ S_type (tvar, encode type_) ]
  | D_module { module_binder = mvar; module_; module_attr = { public; _ } } ->
    let sig_' = signature_of_module_expr ~ctx module_ in
    public, [ S_module (mvar, sig_') ]
  | D_irrefutable_match { pattern; expr = _; attr = { public; _ } } ->
    let sigs =
      List.map (Ast_typed.Pattern.binders pattern) ~f:(fun b ->
          Context.Signature.S_value (Binder.get_var b, encode @@ Binder.get_ascr b))
    in
    public, sigs
  | D_open { module_ } ->
    let sig_' = signature_of_module_expr ~ctx module_ in
    false, [ S_open sig_' ]
  | D_include { module_ } ->
    let sig_' = signature_of_module_expr ~ctx module_ in
    true, [ S_include sig_' ]


(* Load context from the outside declarations *)
let ctx_init ?env () =
  match env with
  | None -> Context.empty
  | Some env ->
    Environment.fold env ~init:Context.empty ~f:(fun ctx decl ->
        match Location.unwrap decl with
        | D_irrefutable_match { pattern; expr = _; attr = _ } ->
          List.fold (Ast_typed.Pattern.binders pattern) ~init:ctx ~f:(fun ctx x ->
              Context.add_imm ctx (Binder.get_var x) (encode @@ Binder.get_ascr x))
        | D_value { binder; expr; attr = _ } ->
          Context.add_imm ctx (Binder.get_var binder) (encode expr.type_expression)
        | D_type { type_binder; type_expr; type_attr = _ } ->
          Context.add_type ctx type_binder (encode type_expr)
        | D_module { module_binder; module_; module_attr = _ } ->
          let sig_ = signature_of_module_expr ~ctx module_ in
          Context.add_module ctx module_binder sig_
        | D_open { module_ } ->
          let sig_ = signature_of_module_expr ~ctx module_ in
          Context.add_open ctx sig_
        | D_include { module_ } ->
          let sig_ = signature_of_module_expr ~ctx module_ in
          Context.add_include ctx sig_)


let run_elab t ~raise ~options ?env () =
  let ctx = ctx_init ?env () in
  (* Format.printf "@[Context:@.%a@]" Context.pp ctx; *)
  let ctx, pos = Context.mark ctx in
  let (ctx, subst), elab =
    t ~raise ~options ~loc:Location.generated (ctx, Substitution.empty)
  in
  (* Drop to get any remaining equations that relate to elaborated thing *)
  let _ctx, subst' = Context.drop_until ctx ~pos ~on_exit:Drop in
  Elaboration.run elab ~raise (Substitution.merge subst subst')


include Monad.Make3 (struct
  type nonrec ('a, 'err, 'wrn) t = ('a, 'err, 'wrn) t

  let return result ~raise:_ ~options:_ ~loc:_ state = state, result

  let bind t ~f ~raise ~options ~loc state =
    let state, result = t ~raise ~options ~loc state in
    f result ~raise ~options ~loc state


  let map = `Define_using_bind
end)

let all_lmap (lmap : ('a, 'err, 'wrn) t Record.LMap.t) : ('a Record.LMap.t, 'err, 'wrn) t =
 fun ~raise ~options ~loc state ->
  Record.LMap.fold_map lmap ~init:state ~f:(fun _label t state ->
      t ~raise ~options ~loc state)


let all_lmap_unit (lmap : (unit, 'err, 'wrn) t Record.LMap.t) : (unit, 'err, 'wrn) t =
 fun ~raise ~options ~loc state ->
  let state =
    Record.LMap.fold
      (fun _label t state ->
        let state, () = t ~raise ~options ~loc state in
        state)
      lmap
      state
  in
  state, ()


let context () : (Context.t, _, _) t =
 fun ~raise:_ ~options:_ ~loc:_ (ctx, subst) -> (ctx, subst), ctx


let options () : (Compiler_options.middle_end, _, _) t =
 fun ~raise:_ ~options ~loc:_ state -> state, options


let loc () : (Location.t, _, _) t = fun ~raise:_ ~options:_ ~loc state -> state, loc

let set_loc loc (in_ : ('a, 'err, 'wrn) t) : ('a, 'err, 'wrn) t =
 fun ~raise ~options ~loc:_ state -> in_ ~raise ~options ~loc state


let set_context ctx : (unit, _, _) t =
 fun ~raise:_ ~options:_ ~loc:_ (_ctx, subst) -> (ctx, subst), ()


let lift_raise f : _ t = fun ~raise ~options:_ ~loc:_ state -> state, f raise

let raise_result result ~error : _ t =
 fun ~raise ~options:_ ~loc state ->
  match result with
  | Ok result -> state, result
  | Error err -> raise.error (error err loc)


let raise_opt opt ~error : _ t =
 fun ~raise ~options:_ ~loc state -> state, trace_option ~raise (error loc) opt


let raise err : _ t = fun ~raise ~options:_ ~loc _state -> raise.error (err loc)
let raise_l ~loc err : _ t = fun ~raise ~options:_ ~loc:_ _state -> raise.error (err loc)

let warn wrn : _ t =
 fun ~raise ~options:_ ~loc state ->
  raise.warning (wrn loc);
  state, ()


let fresh_lexists () =
  let open Let_syntax in
  let%bind loc = loc () in
  let lvar = Layout_var.fresh ~loc () in
  return (lvar, Type.L_exists lvar)


let fresh_texists () =
  let open Let_syntax in
  let%bind loc = loc () in
  let tvar = Type_var.fresh ~loc () in
  return (tvar, Type.t_exists ~loc tvar ())


let fresh_type_var () =
  let open Let_syntax in
  let%bind loc = loc () in
  let tvar = Type_var.fresh ~loc () in
  return tvar


module Options = struct
  let test () =
    let open Let_syntax in
    let%map options = options () in
    options.test


  let syntax () =
    let open Let_syntax in
    let%map options = options () in
    options.syntax_for_errors


  let no_color () =
    let open Let_syntax in
    let%map options = options () in
    options.no_colour
end

type 'a exit =
  | Drop : 'a exit
  | Lift_type : (Type.t * 'a) exit
  | Lift_sig : (Context.Signature.t * 'a) exit

module Context_ = Context

module Context = struct
  module Signature = Context.Signature

  let lift_var ~get_vars ~add_var ~add_eq ~at ~fresh ~var' t =
    let open Let_syntax in
    let%bind ctx = context () in
    let ctx1, ctx2 = Context.split_at ctx ~at in
    if not @@ Set.mem (get_vars ctx1) var'
    then (
      let%bind var'', t' = fresh () in
      let%bind () =
        set_context @@ Context.(add_var ctx1 var'' |:: at |@ add_eq ctx2 var' t')
      in
      return t')
    else return t


  let lift_lvar ~at ~lvar' layout =
    lift_var
      ~get_vars:Context.get_lexists_vars
      ~add_var:Context.add_lexists_var
      ~add_eq:Context.add_lexists_eq
      ~at
      ~fresh:fresh_lexists
      ~var':lvar'
      layout


  let lift_tvar ~at ~tvar' ~kind type_ =
    lift_var
      ~get_vars:Context.get_texists_vars
      ~add_var:(fun ctx tvar' -> Context.add_texists_var ctx tvar' kind)
      ~add_eq:(fun ctx tvar' -> Context.add_texists_eq ctx tvar' kind)
      ~at
      ~fresh:fresh_texists
      ~var':tvar'
      type_


  let insert_at ~at ~hole : _ t =
    let open Let_syntax in
    let%bind ctx = context () in
    set_context @@ Context.insert_at ctx ~at ~hole:(Context.of_list hole)


  let lock (type a) ~(on_exit : a exit) ~(in_ : (a, _, _) t) : (a, _, _) t =
   fun ~raise ~options ~loc (ctx, subst) ->
    let ctx, lock = Context.lock ctx in
    let (ctx, subst), result = in_ ~raise ~options ~loc (ctx, subst) in
    let ctx, subst', (result : a) =
      match on_exit, result with
      | Drop, result ->
        let ctx, subst' = Context.unlock ctx ~on_exit:Drop ~lock in
        ctx, subst', result
      | Lift_type, (type_, result) ->
        let (ctx, type_), subst' =
          Context.unlock (ctx, type_) ~on_exit:(Lift Context.Apply.type_) ~lock
        in
        ctx, subst', (type_, result)
      | Lift_sig, (sig_, result) ->
        let (ctx, sig_), subst' =
          Context.unlock (ctx, sig_) ~on_exit:(Lift Context.Apply.sig_) ~lock
        in
        ctx, subst', (sig_, result)
    in
    let subst = Substitution.merge subst subst' in
    (ctx, subst), result


  let add (type a) items ~(on_exit : a exit) ~(in_ : (a, _, _) t) : (a, _, _) t =
   fun ~raise ~options ~loc (ctx, subst) ->
    let ctx, pos = Context.mark ctx in
    let ctx = List.fold_right items ~init:ctx ~f:(fun item ctx -> Context.add ctx item) in
    let (ctx, subst), result = in_ ~raise ~options ~loc (ctx, subst) in
    let ctx, subst', (result : a) =
      match on_exit, result with
      | Drop, result ->
        let ctx, subst' = Context.drop_until ctx ~on_exit:Drop ~pos in
        ctx, subst', result
      | Lift_type, (type_, result) ->
        let (ctx, type_), subst' =
          Context.drop_until (ctx, type_) ~on_exit:(Lift Context.Apply.type_) ~pos
        in
        ctx, subst', (type_, result)
      | Lift_sig, (sig_, result) ->
        let (ctx, sig_), subst' =
          Context.drop_until (ctx, sig_) ~on_exit:(Lift Context.Apply.sig_) ~pos
        in
        ctx, subst', (sig_, result)
    in
    let subst = Substitution.merge subst subst' in
    (ctx, subst), result


  let push items : _ t =
   fun ~raise:_ ~options:_ ~loc:_ (ctx, subst) ->
    (Context.(ctx |@ of_list items), subst), ()


  let lift_ctx f : _ t =
    let open Let_syntax in
    let%map ctx = context () in
    f ctx


  let get_value var : _ t = lift_ctx (fun ctx -> Context.get_value ctx var)
  let get_value_exn var ~error : _ t = get_value var >>= raise_result ~error
  let get_imm var : _ t = lift_ctx (fun ctx -> Context.get_imm ctx var)
  let get_imm_exn var ~error : _ t = get_imm var >>= raise_opt ~error
  let get_mut var : _ t = lift_ctx (fun ctx -> Context.get_mut ctx var)
  let get_mut_exn var ~error : _ t = get_mut var >>= raise_opt ~error
  let get_type_var tvar : _ t = lift_ctx (fun ctx -> Context.get_type_var ctx tvar)
  let get_type_var_exn tvar ~error = get_type_var tvar >>= raise_opt ~error
  let get_type tvar : _ t = lift_ctx (fun ctx -> Context.get_type ctx tvar)
  let get_type_exn tvar ~error = get_type tvar >>= raise_opt ~error

  let get_texists_var tvar ~error : _ t =
    lift_ctx (fun ctx -> Context.get_texists_var ctx tvar) >>= raise_opt ~error


  let get_signature path : _ t = lift_ctx (fun ctx -> Context.get_signature ctx path)
  let get_signature_exn path ~error : _ t = get_signature path >>= raise_opt ~error
  let get_module mvar : _ t = lift_ctx (fun ctx -> Context.get_module ctx mvar)
  let get_module_exn mvar ~error : _ t = get_module mvar >>= raise_opt ~error
  let get_sum constr : _ t = lift_ctx (fun ctx -> Context.get_sum ctx constr)
  let get_record fields : _ t = lift_ctx (fun ctx -> Context.get_record ctx fields)

  let add_texists_eq tvar kind type_ : _ t =
   fun ~raise:_ ~options:_ ~loc:_ (ctx, subst) ->
    (Context.add_texists_eq ctx tvar kind type_, subst), ()


  let add_lexists_eq lvar layout : _ t =
   fun ~raise:_ ~options:_ ~loc:_ (ctx, subst) ->
    (Context.add_lexists_eq ctx lvar layout, subst), ()


  module Apply = struct
    let type_ type' : _ t =
     fun ~raise:_ ~options:_ ~loc:_ (ctx, subst) ->
      (ctx, subst), Context.Apply.type_ ctx type'
  end

  module Well_formed = struct
    let type_ type_ =
      let open Let_syntax in
      let%map ctx = context () in
      Context.Well_formed.type_ ~ctx type_


    let context () =
      let open Let_syntax in
      let%map ctx = context () in
      Context.Well_formed.context ctx
  end

  let tapply = Apply.type_
end

let occurs_check ~tvar (type_ : Type.t) =
  let open Let_syntax in
  let%bind loc = loc () in
  lift_raise
  @@ fun raise ->
  let fail () = raise.error (occurs_check_failed tvar type_ loc) in
  let rec loop (type_ : Type.t) =
    match type_.content with
    | T_variable _tvar' -> ()
    | T_exists tvar' -> if Type_var.equal tvar tvar' then fail ()
    | T_arrow { type1; type2 } ->
      loop type1;
      loop type2
    | T_for_all { type_; _ } | T_abstraction { type_; _ } -> loop type_
    | T_construct { parameters; _ } -> List.iter parameters ~f:loop
    | T_record rows | T_sum rows ->
      Record.LMap.iter
        (fun _label ({ associated_type; _ } : _ Rows.row_element_mini_c) ->
          loop associated_type)
        rows.fields
    | T_singleton _ -> ()
  in
  loop type_


module Mode = struct
  type t =
    | Covariant (* + *)
    | Contravariant (* - *)
    | Invariant (* +- *)

  let invert t =
    match t with
    | Covariant -> Contravariant
    | Contravariant -> Covariant
    | Invariant -> Invariant
end

let lift_layout ~at (layout : Type.layout) : (Type.layout, _, _) t =
  let open Let_syntax in
  match layout with
  | L_tree | L_comb -> return layout
  | L_exists lvar' -> Context.lift_lvar ~at ~lvar' layout


let rec lift ~(mode : Mode.t) ~kind ~tvar (type_ : Type.t) : (Type.t, _, _) t =
  let open Let_syntax in
  let lift ~mode type_ = lift ~mode ~kind ~tvar type_ in
  let lift_row row = lift_row ~kind ~tvar row in
  let const content = return { type_ with content } in
  match type_.content with
  | T_variable tvar' -> const @@ T_variable tvar'
  | T_exists tvar' ->
    Context.lift_tvar ~at:(C_texists_var (tvar, kind)) ~tvar' ~kind type_
  | T_for_all { ty_binder = tvar'; kind = kind'; type_ } ->
    (match mode with
    | Contravariant ->
      let%bind tvar', type' = fresh_texists () in
      let%bind () =
        Context.insert_at
          ~at:(C_texists_var (tvar, kind))
          ~hole:[ C_texists_var (tvar', kind'); C_texists_var (tvar, kind) ]
      in
      lift ~mode:Contravariant (Type.subst ~tvar:tvar' ~type_:type' type_)
    | Covariant ->
      Context.add
        [ C_type_var (tvar', kind') ]
        ~on_exit:Drop
        ~in_:(lift ~mode:Covariant type_)
    | Invariant ->
      Context.add
        [ C_type_var (tvar', kind') ]
        ~on_exit:Drop
        ~in_:
          (let%bind type_ = lift ~mode:Invariant type_ in
           const @@ T_for_all { ty_binder = tvar'; kind = kind'; type_ }))
  | T_abstraction { ty_binder = tvar'; kind; type_ } ->
    let%bind tvar'' = fresh_type_var () in
    let type_ = Type.subst_var type_ ~tvar:tvar' ~tvar':tvar'' in
    let%bind type_ =
      Context.add [ C_type_var (tvar'', kind) ] ~on_exit:Drop ~in_:(lift ~mode type_)
    in
    const @@ T_abstraction { ty_binder = tvar'; kind; type_ }
  | T_arrow { type1; type2 } ->
    let%bind type1 = lift ~mode:(Mode.invert mode) type1 in
    let%bind type2 = Context.tapply type2 >>= lift ~mode in
    const @@ T_arrow { type1; type2 }
  | T_sum row ->
    let%bind row = lift_row row in
    const @@ T_sum row
  | T_record row ->
    let%bind row = lift_row row in
    const @@ T_record row
  | T_construct construct ->
    let%bind parameters =
      construct.parameters
      |> List.map ~f:(fun param ->
             let%bind param = Context.tapply param in
             lift ~mode:Invariant param)
      |> all
    in
    const @@ T_construct { construct with parameters }
  | T_singleton _ -> return type_


and lift_row ~kind ~tvar ({ fields; layout } : Type.row) : (Type.row, _, _) t =
  let open Let_syntax in
  let%bind layout = lift_layout ~at:(C_texists_var (tvar, kind)) layout in
  let%bind fields =
    fields
    |> Record.map ~f:(fun (row_elem : Type.row_element) ->
           let%map associated_type =
             Context.tapply row_elem.associated_type >>= lift ~mode:Invariant ~kind ~tvar
           in
           { row_elem with associated_type })
    |> all_lmap
  in
  return { Type.fields; layout }


let unify_texists tvar type_ =
  let open Let_syntax in
  let%bind () = occurs_check ~tvar type_ in
  let%bind kind = Context.get_texists_var tvar ~error:(unbound_texists_var tvar) in
  let%bind type_ = lift ~mode:Invariant ~tvar ~kind type_ in
  if%bind
    match%map Context.Well_formed.type_ type_ with
    | Some kind' -> Kind.equal kind kind'
    | _ -> false
  then Context.add_texists_eq tvar kind type_
  else raise_l ~loc:type_.location (ill_formed_type type_)


let unify_layout type1 type2 (layout1 : Type.layout) (layout2 : Type.layout) =
  let open Let_syntax in
  match layout1, layout2 with
  | L_comb, L_tree | L_tree, L_comb ->
    raise (cannot_unify_diff_layout type1 type2 layout1 layout2)
  | L_comb, L_comb | L_tree, L_tree -> return ()
  | L_exists lvar1, L_exists lvar2 when Layout_var.equal lvar1 lvar2 -> return ()
  | L_exists lvar, layout | layout, L_exists lvar ->
    let%bind layout = lift_layout ~at:(C_lexists_var lvar) layout in
    Context.add_lexists_eq lvar layout


let equal_domains lmap1 lmap2 =
  let open Record in
  (* One day this will be removed when we use [Core] maps *)
  LSet.(equal (of_list (LMap.keys lmap1)) (of_list (LMap.keys lmap2)))


type unify_error =
  [ `Typer_cannot_unify of bool * Type.t * Type.t * Location.t
  | `Typer_cannot_unify_diff_layout of
    Type.t * Type.t * Type.layout * Type.layout * Location.t
  | `Typer_ill_formed_type of Type.t * Location.t
  | `Typer_occurs_check_failed of Type_var.t * Type.t * Location.t
  | `Typer_unbound_texists_var of Type_var.t * Location.t
  ]

let rec unify (type1 : Type.t) (type2 : Type.t) =
  let open Let_syntax in
  let unify_ type1 type2 =
    let%bind type1 = Context.tapply type1 in
    let%bind type2 = Context.tapply type2 in
    unify type1 type2
  in
  let fail () =
    let%bind no_color = Options.no_color () in
    raise (cannot_unify no_color type1 type2)
  in
  match type1.content, type2.content with
  | T_singleton lit1, T_singleton lit2 when Literal_value.equal lit1 lit2 -> return ()
  | T_variable tvar1, T_variable tvar2 when Type_var.equal tvar1 tvar2 -> return ()
  | T_exists tvar1, T_exists tvar2 when Type_var.equal tvar1 tvar2 -> return ()
  | _, T_exists tvar2 -> unify_texists tvar2 type1
  | T_exists tvar1, _ -> unify_texists tvar1 type2
  | ( T_construct { language = lang1; constructor = constr1; parameters = params1 }
    , T_construct { language = lang2; constructor = constr2; parameters = params2 } )
    when String.(lang1 = lang2) && Literal_types.equal constr1 constr2 ->
    (match List.map2 params1 params2 ~f:unify_ with
    | Ok ts -> all_unit ts
    | Unequal_lengths -> raise (assert false))
  | T_arrow { type1 = type11; type2 = type12 }, T_arrow { type1 = type21; type2 = type22 }
    ->
    let%bind () = unify type11 type21 in
    unify_ type12 type22
  | ( T_for_all { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_for_all { ty_binder = tvar2; kind = kind2; type_ = type2 } )
  | ( T_abstraction { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_abstraction { ty_binder = tvar2; kind = kind2; type_ = type2 } )
    when Kind.equal kind1 kind2 ->
    let%bind tvar = fresh_type_var () in
    let type1 = Type.subst_var type1 ~tvar:tvar1 ~tvar':tvar in
    let type2 = Type.subst_var type2 ~tvar:tvar2 ~tvar':tvar in
    Context.add [ C_type_var (tvar, kind1) ] ~on_exit:Drop ~in_:(unify type1 type2)
  | ( T_sum { fields = fields1; layout = layout1 }
    , T_sum { fields = fields2; layout = layout2 } )
  | ( T_record { fields = fields1; layout = layout1 }
    , T_record { fields = fields2; layout = layout2 } )
    when equal_domains fields1 fields2 ->
    let%bind () = unify_layout type1 type2 layout1 layout2 in
    (* TODO: This should be replaced by [map2] or smth *)
    fields1
    |> Record.LMap.mapi (fun label (row_elem1 : Type.row_element) ->
           let row_elem2 = Record.LMap.find label fields2 in
           unify_ row_elem1.associated_type row_elem2.associated_type)
    |> all_lmap_unit
  | _ -> fail ()


type subtype_error = unify_error

module O = Ast_typed
module E = Elaboration

let rec subtype ~(received : Type.t) ~(expected : Type.t)
    : (O.expression -> O.expression Elaboration.t, _, _) t
  =
  let open Let_syntax in
  let subtype received expected = subtype ~received ~expected in
  let subtype_texists ~mode tvar type_ =
    let%bind () = occurs_check ~tvar type_ in
    let%bind kind = Context.get_texists_var tvar ~error:(unbound_texists_var tvar) in
    let%bind type_ = lift ~mode ~tvar ~kind type_ in
    let%bind () = Context.add_texists_eq tvar kind type_ in
    return E.return
  in
  let%bind loc = loc () in
  match received.content, expected.content with
  | T_arrow { type1 = type11; type2 = type12 }, T_arrow { type1 = type21; type2 = type22 }
    ->
    let%bind f1 = subtype type21 type11 in
    let%bind type12 = Context.tapply type12 in
    let%bind type22 = Context.tapply type22 in
    let%bind f2 = subtype type12 type22 in
    return
      E.(
        fun hole ->
          let%bind type11 = decode type11
          and type12 = decode type12
          and type21 = decode type21
          and type22 = decode type22 in
          if O.compare_type_expression type11 type21 = 0
             && O.compare_type_expression type12 type22 = 0
          then return hole
          else (
            let x = Value_var.fresh ~loc ~name:"_sub" () in
            let%bind arg = f1 (O.e_variable x type21) in
            let binder = Param.make x type21 in
            let%bind result = f2 (O.e_a_application hole arg type12) in
            return @@ O.e_a_lambda { binder; result; output_type = type22 } type21 type22))
  | T_for_all { ty_binder = tvar; kind; type_ }, _ ->
    let%bind tvar', texists = fresh_texists () in
    let type' = Type.subst type_ ~tvar ~type_:texists in
    let%bind f =
      Context.add
        [ C_texists_var (tvar', kind) ]
        ~on_exit:Drop
        ~in_:(subtype type' expected)
    in
    return
      E.(
        fun hole ->
          let%bind type' = decode type' in
          let%bind texists = decode texists in
          f (O.e_type_inst { forall = hole; type_ = texists } type'))
  | _, T_for_all { ty_binder = tvar; kind; type_ } ->
    let%bind tvar' = fresh_type_var () in
    let%bind f =
      Context.add
        [ C_type_var (tvar', kind) ]
        ~on_exit:Drop
        ~in_:(subtype received (Type.subst_var type_ ~tvar ~tvar'))
    in
    return
      E.(
        fun hole ->
          let%bind result = f hole in
          let%bind expected = decode expected in
          return @@ O.e_type_abstraction { type_binder = tvar'; result } expected)
  | (T_variable tvar1, T_variable tvar2 | T_exists tvar1, T_exists tvar2)
    when Type_var.equal tvar1 tvar2 -> return E.return
  | T_exists tvar1, _ -> subtype_texists ~mode:Contravariant tvar1 expected
  | _, T_exists tvar2 -> subtype_texists ~mode:Covariant tvar2 received
  | _, _ ->
    let%bind () = unify received expected in
    return E.return


let exists kind =
  let open Let_syntax in
  let%bind tvar, texists = fresh_texists () in
  let%bind () = Context.push [ C_texists_var (tvar, kind) ] in
  return texists


let for_all kind =
  let open Let_syntax in
  let%bind tvar = fresh_type_var () in
  let%bind () = Context.push [ C_type_var (tvar, kind) ] in
  return (Type.t_variable ~loc:(Type_var.get_location tvar) tvar ())


let lexists () =
  let open Let_syntax in
  let%bind lvar, layout = fresh_lexists () in
  let%bind () = Context.push [ C_lexists_var lvar ] in
  return layout


let def bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (var, mut_flag, type_) ->
         Context_.C_value (var, mut_flag, type_)))
    ~in_
    ~on_exit


let def_module bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (mvar, sig_) -> Context_.C_module (mvar, sig_)))
    ~on_exit
    ~in_


let def_type bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (tvar, type_) -> Context_.C_type (tvar, type_)))
    ~in_
    ~on_exit


let def_type_var bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (tvar, kind) -> Context_.C_type_var (tvar, kind)))
    ~in_
    ~on_exit


let def_sig_item sig_items ~on_exit ~in_ =
  Context.add
    (List.map sig_items ~f:Context_.items_of_signature_item |> List.join)
    ~in_
    ~on_exit


let assert_ cond ~error =
  let open Let_syntax in
  if cond then return () else raise error


let hash_context () =
  let open Let_syntax in
  let%bind ctx = context () in
  Context_.Hashes.set_context ctx;
  return ()


let generalize (t : (Type.t * 'a, _, _) t)
    : (Type.t * (Type_var.t * Kind.t) list * 'a, _, _) t
  =
 fun ~raise ~options ~loc (ctx, subst) ->
  let ctx, pos = Context_.mark ctx in
  let (ctx, subst), (type_, result) = t ~raise ~options ~loc (ctx, subst) in
  let ctx, type_, tvars, subst' = Context_.generalize ctx type_ ~pos ~loc in
  (ctx, Substitution.merge subst subst'), (type_, tvars, result)


let create_type ?meta (constr : Type.constr) =
  let open Let_syntax in
  let%bind loc = loc () in
  return (constr ?meta ~loc ())


let try_ (body : ('a, 'err, 'wrn) t) ~(with_ : 'err -> ('a, 'err, 'wrn) t)
    : ('a, 'err, 'wrn) t
  =
 fun ~raise ~options ~loc state ->
  Trace.try_with
    (fun ~raise ~catch:_ -> body ~raise ~options ~loc state)
    (fun ~catch:_ err -> with_ err ~raise ~options ~loc state)


let try_all (ts : ('a, 'err, 'wrn) t list) : ('a, 'err, 'wrn) t =
 fun ~raise ~options ~loc state ->
  Trace.bind_exists
    ~raise
    (List.Ne.of_list @@ List.map ts ~f:(fun t ~raise -> t ~raise ~options ~loc state))


module With_frag = struct
  type fragment = (Value_var.t * Param.mutable_flag * Type.t) list
  type nonrec ('a, 'err, 'wrn) t = (fragment * 'a, 'err, 'wrn) t

  let lift t = t >>| fun x -> [], x

  let lift_reader f t =
    let open Let_syntax in
    let%bind frag, x = t in
    let%bind y = f (return x) in
    return (frag, y)


  let extend frag = return (frag, ())
  let run t = t

  include Monad.Make3 (struct
    type nonrec ('a, 'err, 'wrn) t = ('a, 'err, 'wrn) t

    let return x = return ([], x)

    let bind t ~f =
      let open Let_syntax in
      let%bind frag1, x = t in
      let%bind frag2, y = f x in
      return (frag1 @ frag2, y)


    let map = `Define_using_bind
  end)

  let all_lmap (lmap : ('a, 'err, 'wrn) t Record.LMap.t)
      : ('a Record.LMap.t, 'err, 'wrn) t
    =
   fun ~raise ~options ~loc state ->
    let (state, frag), lmap =
      Record.LMap.fold_map lmap ~init:(state, []) ~f:(fun _label t (state, frag) ->
          let state, (frag', result) = t ~raise ~options ~loc state in
          (state, frag @ frag'), result)
    in
    state, (frag, lmap)


  let all_lmap_unit (lmap : (unit, 'err, 'wrn) t Record.LMap.t) : (unit, 'err, 'wrn) t =
   fun ~raise ~options ~loc state ->
    let state, frag =
      Record.LMap.fold
        (fun _label t (state, frag) ->
          let state, (frag', ()) = t ~raise ~options ~loc state in
          state, frag @ frag')
        lmap
        (state, [])
    in
    state, (frag, ())


  let loc () = lift (loc ())
  let set_loc loc = lift_reader (set_loc loc)
  let raise_result result ~error = lift (raise_result result ~error)
  let raise_opt opt ~error = lift (raise_opt opt ~error)
  let raise_l ~loc error = lift (raise_l ~loc error)
  let raise error = lift (raise error)
  let warn warning = lift (warn warning)
  let assert_ cond ~error = lift (assert_ ~error cond)
  let create_type ?meta type_ = lift (create_type ?meta type_)

  module Context = struct
    let get_sum label = lift (Context.get_sum label)
    let get_record row = lift (Context.get_record row)
    let tapply type_ = lift (Context.tapply type_)
  end

  let exists kind = lift (exists kind)
  let lexists () = lift (lexists ())
  let unify type1 type2 = lift (unify type1 type2)
  let subtype ~received ~expected = lift (subtype ~received ~expected)
end
