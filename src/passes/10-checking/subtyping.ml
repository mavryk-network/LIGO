module Well_formed = Context.Well_formed
module Exists_var = Context.Exists_var
module Trace = Simple_utils.Trace
open Trace
module Errors = Errors
open Errors
open Ast_typed

let occurs_check ~raise ~loc ~evar type_ =
  let fail () = raise.error (occurs_check_failed loc evar type_) in
  let rec loop type_ =
    match type_.type_content with
    | T_variable tvar' ->
      (match Exists_var.of_type_var tvar' with
       | Some evar' -> if Exists_var.equal evar evar' then fail ()
       | None -> ())
    | T_arrow { type1; type2 } ->
      loop type1;
      loop type2
    | T_for_all { type_; _ } | T_abstraction { type_; _ } -> loop type_
    | T_constant { parameters; _ } -> List.iter parameters ~f:loop
    | T_record rows | T_sum rows ->
      LMap.iter (fun _label { associated_type; _ } -> loop associated_type) rows.content
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

let t_subst t ~tvar ~type_ = Ast_typed.Helpers.subst_no_capture_type tvar type_ t
let t_subst_var ~loc t ~tvar ~tvar' = t_subst t ~tvar ~type_:(t_variable ~loc tvar' ())
let t_exists ~loc (evar : Exists_var.t) = t_variable ~loc (evar :> type_variable) ()

let rec lift ~raise ~loc ~ctx ~(mode : Mode.t) ~kind ~evar (type_ : type_expression)
  : Context.t * type_expression
  =
  let self ?(ctx = ctx) ~mode = lift ~raise ~loc ~ctx ~mode ~kind ~evar in
  let return content = { type_ with type_content = content } in
  match type_.type_content with
  | T_variable tvar' ->
    (match Exists_var.of_type_var tvar' with
     | Some evar' ->
       let ctx1, ctx2 = Context.split_at ctx ~at:(C_exists_var (evar, kind)) in
       if List.mem ~equal:Exists_var.equal (Context.get_exists_vars ctx1) evar'
       then ctx, type_
       else (
         let evar'' = Exists_var.fresh ~loc () in
         let type_ = t_exists ~loc evar'' in
         ( Context.(
             ctx1
             |:: C_exists_var (evar'', kind)
             |:: C_exists_var (evar, kind)
             |@ add_exists_eq ctx2 evar' kind type_)
         , type_ ))
     | None -> ctx, type_)
  | T_for_all { ty_binder = tvar'; kind = kind'; type_ } ->
    (match mode with
     | Contravariant ->
       let ctx1, ctx2 = Context.split_at ctx ~at:(C_exists_var (evar, kind)) in
       let evar' = Exists_var.fresh ~loc () in
       self
         ~ctx:
           Context.(
             ctx1 |:: C_exists_var (evar', kind') |:: C_exists_var (evar, kind) |@ ctx2)
         ~mode:Contravariant
         (t_subst ~tvar:tvar' ~type_:(t_exists ~loc evar') type_)
     | Covariant ->
       let ctx, pos = Context.mark ctx in
       let ctx, type_ =
         self ~ctx:Context.(ctx |:: C_type_var (tvar', kind')) ~mode:Covariant type_
       in
       Context.drop_until ctx ~pos, type_
     | Invariant ->
       let ctx, pos = Context.mark ctx in
       let ctx, type_ =
         self ~ctx:Context.(ctx |:: C_type_var (tvar', kind')) ~mode:Invariant type_
       in
       ( Context.drop_until ctx ~pos
       , return @@ T_for_all { ty_binder = tvar'; kind = kind'; type_ } ))
  | T_abstraction { ty_binder = tvar'; kind; type_ } ->
    let tvar'' = TypeVar.fresh ~loc () in
    let type_ = t_subst_var ~loc type_ ~tvar:tvar' ~tvar':tvar'' in
    let ctx, pos = Context.mark ctx in
    let ctx, type_ = self ~ctx:Context.(ctx |:: C_type_var (tvar'', kind)) ~mode type_ in
    Context.drop_until ctx ~pos, type_
  | T_arrow { type1; type2 } ->
    let ctx, type1 = self ~ctx ~mode:(Mode.invert mode) type1 in
    let ctx, type2 = self ~ctx ~mode (Context.apply ctx type2) in
    ctx, return @@ T_arrow { type1; type2 }
  | T_sum { content; layout } ->
    let ctx, content =
      LMap.fold_map content ~init:ctx ~f:(fun _label row_elem ctx ->
        (* TODO: Formalize addition of rows to calculus (including treatment of variance) *)
        let ctx, associated_type =
          self ~ctx ~mode:Invariant (Context.apply ctx row_elem.associated_type)
        in
        ctx, { row_elem with associated_type })
    in
    ctx, return @@ T_sum { content; layout }
  | T_record { content; layout } ->
    let ctx, content =
      LMap.fold_map content ~init:ctx ~f:(fun _label row_elem ctx ->
        (* TODO: Formalize addition of rows to calculus (including treatment of variance) *)
        let ctx, associated_type =
          self ~ctx ~mode:Invariant (Context.apply ctx row_elem.associated_type)
        in
        ctx, { row_elem with associated_type })
    in
    ctx, return @@ T_record { content; layout }
  | T_constant inj ->
    let ctx, parameters =
      List.fold_map inj.parameters ~init:ctx ~f:(fun ctx param ->
        self ~ctx ~mode:Invariant (Context.apply ctx param))
    in
    ctx, return @@ T_constant { inj with parameters }
  | T_singleton _ -> ctx, type_


let equal_literal lit1 lit2 =
  let open Stage_common in
  Enums.literal_to_enum lit1 = Enums.literal_to_enum lit2


let consistent_injections
  { language = lang1; injection = inj1; _ }
  { language = lang2; injection = inj2; _ }
  =
  String.(lang1 = lang2) && Stage_common.Constant.equal inj1 inj2


let equal_domains lmap1 lmap2 =
  LSet.(equal (of_list (LMap.keys lmap1)) (of_list (LMap.keys lmap2)))


module External_types = struct
  module Type = struct
    type t = (type_expression list * type_expression) List.Ne.t

    module Syntax = struct
      let create xs = List.Ne.of_list xs
      let ( ^~> ) arg_type ret_type = [ arg_type ], ret_type
      let ( ^-> ) arg_type (arg_types, ret_type) = arg_type :: arg_types, ret_type
    end
  end

  type ('err, 'wrn) unify =
    raise:('err, 'wrn) raise
    -> ctx:Context.t
    -> type_expression
    -> type_expression
    -> Context.t

  type ('err, 'wrn) t =
    raise:('err, 'wrn) raise
    -> unify:('err, 'wrn) unify
    -> ctx:Context.t
    -> type_expression list
    -> Context.t * type_expression

  let of_type (types : Type.t) : _ t =
   fun ~raise ~unify ~ctx recieved_arg_types ->
    Trace.bind_exists ~raise
    @@ List.Ne.map
         (fun (expected_arg_types, ret_type) ~raise ->
           let arg_types =
             match List.zip recieved_arg_types expected_arg_types with
             | Ok result -> result
             | Unequal_lengths ->
               raise.error
                 (corner_case
                    "Unequal lengths between mode annotation and argument types")
           in
           (* Unify args types *)
           let ctx =
             List.fold arg_types ~init:ctx ~f:(fun ctx (recieved, expected) ->
               unify ~raise ~ctx (Context.apply ctx recieved) (Context.apply ctx expected))
           in
           ctx, ret_type)
         types


  let int_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type.Syntax in
    of_type (create [ t_nat () ^~> t_int (); t_bls12_381_fr () ^~> t_int () ])


  let ediv_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type.Syntax in
    of_type
      (create
         [ t_nat () ^-> t_nat () ^~> t_option (t_pair (t_nat ()) (t_nat ()))
         ; t_int () ^-> t_int () ^~> t_option (t_pair (t_int ()) (t_nat ()))
         ; t_nat () ^-> t_int () ^~> t_option (t_pair (t_int ()) (t_nat ()))
         ; t_int () ^-> t_nat () ^~> t_option (t_pair (t_int ()) (t_nat ()))
         ; t_mutez () ^-> t_mutez () ^~> t_option (t_pair (t_nat ()) (t_mutez ()))
         ; t_mutez () ^-> t_nat () ^~> t_option (t_pair (t_mutez ()) (t_mutez ()))
         ])
end

let rec unify
  ~raise
  ~loc
  ~(ctx : Context.t)
  (type1 : type_expression)
  (type2 : type_expression)
  : Context.t
  =
  let unify = unify ~loc in
  let self ?(ctx = ctx) type1 type2 = unify ~raise ~ctx type1 type2 in
  let fail () = raise.error (cannot_unify loc type1 type2) in
  let unify_evar evar type_ =
    occurs_check ~raise ~loc ~evar type_;
    let kind =
      Context.get_exists_var ctx evar
      |> trace_option ~raise (unbound_exists_variable (Exists_var.loc evar) evar)
    in
    let ctx, type_ = lift ~raise ~loc ~ctx ~mode:Invariant ~evar ~kind type_ in
    if not
         (match Well_formed.type_expr ~ctx type_ with
          | Some kind' -> equal_kind kind kind'
          | _ -> false)
    then raise.error (ill_formed_type type_.location type_);
    Context.add_exists_eq ctx evar kind type_
  in
  match type1.type_content, type2.type_content with
  | T_singleton lit1, T_singleton lit2 when equal_literal lit1 lit2 -> ctx
  | T_constant inj1, T_constant inj2 when consistent_injections inj1 inj2 ->
    (match
       List.fold2 inj1.parameters inj2.parameters ~init:ctx ~f:(fun ctx param1 param2 ->
         self ~ctx (Context.apply ctx param1) (Context.apply ctx param2))
     with
     | Ok ctx -> ctx
     | Unequal_lengths ->
       failwith "Cannot occur since injections are consistent and fully applied")
  | T_constant { injection = External "int"; parameters = [ int_type ]; _ }, _ ->
    let ctx, type2' = External_types.int_types ~raise ~unify ~ctx [ int_type ] in
    self ~ctx type2' type2
  | _, T_constant { injection = External "int"; parameters; _ } ->
    let ctx, type1' = External_types.int_types ~raise ~unify ~ctx parameters in
    self ~ctx type1' type1
  | T_constant { injection = External ("ediv" | "u_ediv"); parameters; _ }, _ ->
    let ctx, type2' = External_types.ediv_types ~raise ~unify ~ctx parameters in
    self ~ctx type2' type2
  | _, T_constant { injection = External ("ediv" | "u_ediv"); parameters; _ } ->
    let ctx, type1' = External_types.ediv_types ~raise ~unify ~ctx parameters in
    self ~ctx type1' type1
  | T_variable tvar1, T_variable tvar2 when TypeVar.equal tvar1 tvar2 -> ctx
  | T_variable tvar1, _ when TypeVar.is_exists tvar1 ->
    unify_evar (Exists_var.of_type_var_exn tvar1) type2
  | _, T_variable tvar2 when TypeVar.is_exists tvar2 ->
    unify_evar (Exists_var.of_type_var_exn tvar2) type1
  | T_arrow { type1 = type11; type2 = type12 }, T_arrow { type1 = type21; type2 = type22 }
    ->
    let ctx = self ~ctx type11 type21 in
    self ~ctx (Context.apply ctx type12) (Context.apply ctx type22)
  | ( T_for_all { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_for_all { ty_binder = tvar2; kind = kind2; type_ = type2 } )
  | ( T_abstraction { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_abstraction { ty_binder = tvar2; kind = kind2; type_ = type2 } )
    when equal_kind kind1 kind2 ->
    let tvar = TypeVar.fresh_like ~loc tvar1 in
    let type1 = t_subst_var ~loc ~tvar:tvar1 ~tvar':tvar type1 in
    let type2 = t_subst_var ~loc ~tvar:tvar2 ~tvar':tvar type2 in
    let ctx, pos = Context.mark ctx in
    self ~ctx:Context.(ctx |:: C_type_var (tvar, kind1)) type1 type2
    |> Context.drop_until ~pos
  | ( T_sum { content = content1; layout = layout1 }
    , T_sum { content = content2; layout = layout2 } )
  | ( T_record { content = content1; layout = layout1 }
    , T_record { content = content2; layout = layout2 } )
    when layout_eq layout1 layout2 && equal_domains content1 content2 ->
    (* Naive unification. Layout and content must be consistent *)
    LMap.fold
      (fun label { associated_type = type1; _ } ctx ->
        let { associated_type = type2; _ } = LMap.find label content2 in
        self ~ctx (Context.apply ctx type1) (Context.apply ctx type2))
      content1
      ctx
  | _ -> fail ()


let rec subtype ~raise ~loc ~ctx ~(recieved : type_expression) ~(expected : type_expression)
  : Context.t * (expression -> expression)
  =
  (* Format.printf "Subtype: %a, %a\n" PP.type_expression recieved PP.type_expression expected; *)
  let self ?(ctx = ctx) recieved expected = subtype ~raise ~loc ~ctx ~recieved ~expected in
  let subtype_evar ~mode evar type_ =
    let kind =
      Context.get_exists_var ctx evar
      |> trace_option ~raise (unbound_exists_variable loc evar)
    in
    let ctx, type_ = lift ~raise ~loc ~ctx ~mode ~evar ~kind type_ in
    occurs_check ~raise ~loc ~evar type_;
    Context.add_exists_eq ctx evar kind type_, fun x -> x
  in
  match recieved.type_content, expected.type_content with
  | T_arrow { type1 = type11; type2 = type12 }, T_arrow { type1 = type21; type2 = type22 }
    ->
    if type_expression_eq (type11, type21) && type_expression_eq (type12, type22)
    then ctx, fun hole -> hole
    else (
      let ctx, f1 = self ~ctx type21 type11 in
      let ctx, f2 = self ~ctx (Context.apply ctx type12) (Context.apply ctx type22) in
      ( ctx
      , fun hole ->
          let x = ValueVar.fresh ~name:"_sub" () in
          let args = f1 (e_variable x type21) in
          let binder =
            { var = x; ascr = Some type21; attributes = { const_or_var = Some `Const } }
          in
          let result = f2 (e_application { lamb = hole; args } type12) in
          e_a_lambda { binder; result } type21 type22 ))
  | T_for_all { ty_binder = tvar; kind; type_ }, _ ->
    let evar = Exists_var.fresh ~loc () in
    let type' = t_subst type_ ~tvar ~type_:(t_exists ~loc evar) in
    let ctx, pos = Context.mark ctx in
    let ctx, f =
      self
        ~ctx:Context.(ctx |:: C_exists_var (evar, kind))
        type'
        expected
    in
    ( Context.drop_until ctx ~pos
    , fun hole -> f (e_type_inst { forall = hole; type_ = t_exists ~loc evar } type') )
  | _, T_for_all { ty_binder = tvar; kind; type_ } ->
    let tvar' = TypeVar.fresh_like ~loc tvar in
    let ctx, pos = Context.mark ctx in
    let ctx, f =
      self
        ~ctx:Context.(ctx |:: C_type_var (tvar', kind))
        recieved
        (t_subst_var ~loc type_ ~tvar ~tvar')
    in
    ( Context.drop_until ctx ~pos
    , fun hole -> e_type_abstraction { type_binder = tvar'; result = f hole } expected )
  | T_variable tvar1, T_variable tvar2 when TypeVar.equal tvar1 tvar2 -> ctx, fun x -> x
  | T_variable tvar1, _ when TypeVar.is_exists tvar1 ->
    subtype_evar ~mode:Contravariant (Exists_var.of_type_var_exn tvar1) expected
  | _, T_variable tvar2 when TypeVar.is_exists tvar2 ->
    subtype_evar ~mode:Covariant (Exists_var.of_type_var_exn tvar2) recieved
  | _, _ -> unify ~raise ~loc ~ctx recieved expected, fun x -> x