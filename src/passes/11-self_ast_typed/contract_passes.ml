open Ligo_prim
open Ast_typed.Types
open Simple_utils.Trace
module Ligo_string = Simple_utils.Ligo_string

type contract_pass_data =
  { contract_type : Helpers.contract_type
  ; main_name : Value_var.t
  }

module VVarSet = Caml.Set.Make (Value_var)
module MVarMap = Simple_utils.Map.Make (Module_var)

type scope = { t : (scope * Value_var.t list) MVarMap.t }

let pp_scope ppf scope =
  Format.fprintf
    ppf
    "{%a}"
    (Simple_utils.PP_helpers.list_sep_d (fun ppf (k, (_s, _l)) ->
         Format.fprintf ppf "%a => (_,_)" Module_var.pp k))
    (MVarMap.to_kv_list scope.t)


module FreeVar = struct
  type t =
    { module_ : t MVarMap.t
    ; free : VVarSet.t
    }

  let rec pp ppf fv =
    Format.fprintf
      ppf
      "{env: %a;used_var: %a}"
      (Simple_utils.PP_helpers.list_sep_d (fun ppf (k, v) ->
           Format.fprintf ppf "(%a,%a)" Module_var.pp k pp v))
      (MVarMap.to_kv_list fv.module_)
      (Simple_utils.PP_helpers.list_sep_d Value_var.pp)
      (VVarSet.elements fv.free)


  let empty = { module_ = MVarMap.empty; free = VVarSet.empty }
  let singleton v = { empty with free = VVarSet.singleton v }
  let add_module t var t' = { t with module_ = MVarMap.add var t' t.module_ }
  let get_module_opt t var = MVarMap.find_opt var t.module_
  let push fv binder = add_module empty binder fv

  let push_at_path fv path =
    Simple_utils.List.Ne.fold_right ~f:(Fun.flip push) ~init:fv path


  let rec merge fv1 fv2 =
    let module_ =
      MVarMap.merge
        (fun _ v1 v2 ->
          match v1, v2 with
          | None, None -> None
          | Some v, None | None, Some v -> Some v
          | Some v1, Some v2 -> Some (merge v1 v2))
        fv1.module_
        fv2.module_
    in
    let free = VVarSet.union fv1.free fv2.free in
    { module_; free }


  let unions l = List.fold l ~init:empty ~f:merge
  let is_empty fv = VVarSet.is_empty fv.free && MVarMap.is_empty fv.module_

  let handle_alias fv path binder =
    match MVarMap.find_opt binder fv.module_ with
    | Some binded_vars ->
      if is_empty binded_vars
      then None
      else (
        let fv = { fv with module_ = MVarMap.remove binder fv.module_ } in
        let pushed_binders = push_at_path binded_vars path in
        Some (merge fv pushed_binders))
    | None -> None


  let lift fv (ms : scope) op =
    let _, opened_binder =
      Simple_utils.List.Ne.fold_left
        ~f:(fun (ms, _) var ->
          Option.value_or_thunk (MVarMap.find_opt var ms.t) ~default:(fun () ->
              raise
              @@ Failure
                   (Format.asprintf
                      "Unbound module %a in scope %a"
                      Module_var.pp
                      var
                      pp_scope
                      ms)))
        ~init:(ms, [])
        op
    in
    let used_var = List.filter ~f:(fun a -> VVarSet.mem a fv.free) opened_binder in
    match used_var with
    | [] -> None
    | _ ->
      let fv =
        List.fold
          ~f:(fun fv v -> { fv with free = VVarSet.remove v fv.free })
          ~init:fv
          used_var
      in
      let module_fv = { empty with free = VVarSet.of_list used_var } in
      let module_fv = push_at_path module_fv op in
      Some (merge fv module_fv)
end

let rec get_fv expr =
  let self = get_fv in
  let return env expression_content = env, { expr with expression_content } in
  let get_fv_lambda Lambda.{ binder; output_type; result } =
    let fv, result = self result in
    ( (match Param.get_mut_flag binder with
      | Immutable ->
        FreeVar.{ fv with free = VVarSet.remove (Param.get_var binder) @@ fv.free }
      | Mutable ->
        fv
        (*  Not sure if this is usefull
        { env with
          used_mut_var = VVarSet.remove (Param.get_var binder) @@ env.used_mut_var
        } *))
    , Lambda.{ binder; output_type; result } )
  in
  match expr.expression_content with
  | E_variable v -> return (FreeVar.singleton v) @@ E_variable v
  | (E_literal _ | E_raw_code _) as ec -> return FreeVar.empty @@ ec
  | E_constant { cons_name; arguments } ->
    let env_lst, arguments = List.unzip @@ List.map ~f:self arguments in
    return (FreeVar.unions @@ env_lst) @@ E_constant { cons_name; arguments }
  | E_application { lamb; args } ->
    let env_l, lamb = self lamb in
    let env_a, args = self args in
    return (FreeVar.merge env_l env_a) @@ E_application { lamb; args }
  | E_type_inst { forall; type_ } ->
    let env, forall = self forall in
    return env @@ E_type_inst { forall; type_ }
  | E_lambda lambda ->
    let env, lambda = get_fv_lambda lambda in
    return env @@ E_lambda lambda
  | E_type_abstraction { type_binder; result } ->
    let env, result = self result in
    return env @@ E_type_abstraction { type_binder; result }
  | E_recursive { fun_name; lambda; fun_type } ->
    let env, lambda = get_fv_lambda lambda in
    return { env with free = VVarSet.remove fun_name env.free }
    @@ E_recursive { fun_name; lambda; fun_type }
  | E_constructor { constructor; element } ->
    let env, element = self element in
    return env @@ E_constructor { constructor; element }
  | E_matching { matchee; cases } ->
    let env, matchee = self matchee in
    let env_c, cases = get_fv_cases cases in
    return (FreeVar.merge env env_c) @@ E_matching { matchee; cases }
  | E_record m ->
    let res = Record.map ~f:self m in
    let keys, env_exp = List.unzip @@ Record.LMap.to_kv_list res in
    let env, exp = List.unzip env_exp in
    let m = Record.of_list @@ List.zip_exn keys exp in
    return (FreeVar.unions env) @@ E_record m
  | E_update { struct_; path; update } ->
    let env_r, struct_ = self struct_ in
    let env_u, update = self update in
    return (FreeVar.merge env_r env_u) @@ E_update { struct_; path; update }
  | E_accessor { struct_; path } ->
    let env, struct_ = self struct_ in
    return env @@ E_accessor { struct_; path }
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let env, let_result = self let_result in
    let free =
      List.fold (Pattern.binders let_binder) ~init:env.free ~f:(fun used_var b ->
          VVarSet.remove (Binder.get_var b) used_var)
    in
    let env = { env with free } in
    let env', rhs = self rhs in
    return (FreeVar.merge env env')
    @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_mod_in { module_binder; rhs; let_result } ->
    let env, let_result = self let_result in
    (match MVarMap.find_opt module_binder env.module_ with
    | Some env' ->
      let env = { env with module_ = MVarMap.remove module_binder env.module_ } in
      let env', rhs = get_fv_module_expr env' rhs in
      return (FreeVar.merge env env') @@ E_mod_in { module_binder; rhs; let_result }
    | None -> env, let_result)
  | E_module_accessor { module_path; element } ->
    let init = FreeVar.singleton element in
    let env = List.fold_right module_path ~f:(Fun.flip FreeVar.push) ~init in
    return env @@ E_module_accessor { module_path; element }
  | E_assign { binder; expression } ->
    let env, expression = self expression in
    return env @@ E_assign { binder; expression }
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let env, let_result = self let_result in
    (* Unclear if this is usefull

    let binders = Pattern.binders let_binder in
    let used_mut_var =
      List.fold binders ~init:env.used_mut_var ~f:(fun used_var b ->
          VVarSet.remove (Binder.get_var b) used_var)
    in
    let env = { env with used_mut_var } in
    *)
    let env', rhs = self rhs in
    return (FreeVar.merge env env')
    @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_deref var ->
    return FreeVar.empty
    (* { empty_env with used_mut_var = VVarSet.singleton var } *) @@ E_deref var
  | E_while { cond; body } ->
    let env1, cond = self cond
    and env2, body = self body in
    return (FreeVar.merge env1 env2) @@ E_while { cond; body }
  | E_for { binder; start; final; incr; f_body } ->
    let env1, start = self start
    and env2, final = self final
    and env3, incr = self incr in
    let env' = FreeVar.unions [ env1; env2; env3 ] in
    let env, f_body = self f_body in
    let env = { env with free = VVarSet.remove binder env.free } in
    return (FreeVar.merge env' env) @@ E_for { binder; start; final; incr; f_body }
  | E_for_each { fe_binder; collection; collection_type; fe_body } ->
    let coll_env, collection = self collection in
    let fe_binder_set =
      let binder1, binder2 = fe_binder in
      binder1 :: Option.to_list binder2 |> VVarSet.of_list
    in
    let body_env, fe_body = self fe_body in
    let body_env = { body_env with free = VVarSet.diff body_env.free fe_binder_set } in
    return (FreeVar.merge coll_env body_env)
    @@ E_for_each { fe_binder; collection; collection_type; fe_body }


and get_fv_cases
    : _ Match_expr.match_case list -> FreeVar.t * _ Match_expr.match_case list
  =
 fun ms ->
  let envs =
    List.map ms ~f:(fun { pattern; body } ->
        let env, body = get_fv body in
        let binders = Pattern.binders pattern |> List.map ~f:Binder.get_var in
        let free = List.fold_right binders ~init:env.free ~f:VVarSet.remove in
        let env = { env with free } in
        env, Match_expr.{ pattern; body })
  in
  let envs, ms = List.unzip envs in
  FreeVar.unions envs, ms


and get_fv_module (fv : FreeVar.t) acc = function
  | [] -> fv, acc
  | ({ Location.wrap_content = D_value { binder; expr; attr }; _ } as hd) :: tl ->
    let binder' = binder in
    if VVarSet.mem (Binder.get_var binder') fv.free
    then (
      let env = { fv with free = VVarSet.remove (Binder.get_var binder') fv.free } in
      let env', expr = get_fv expr in
      let env = FreeVar.merge env @@ env' in
      get_fv_module
        env
        ({ hd with wrap_content = D_value { binder; expr; attr } } :: acc)
        tl)
    else get_fv_module fv acc tl
  | ({ Location.wrap_content = D_module { module_binder; module_; module_attr }; _ } as
    hd)
    :: tl ->
    (match MVarMap.find_opt module_binder fv.module_ with
    | Some env' ->
      let new_env, module_ = get_fv_module_expr env' module_ in
      let env = { fv with module_ = MVarMap.remove module_binder fv.module_ } in
      let env = FreeVar.merge env new_env in
      get_fv_module
        env
        ({ hd with wrap_content = D_module { module_binder; module_; module_attr } }
        :: acc)
        tl
    | None -> get_fv_module fv acc tl)
  | ({ Location.wrap_content = D_irrefutable_match { pattern; expr; attr }; _ } as hd)
    :: tl ->
    let binders =
      List.filter (Pattern.binders pattern) ~f:(fun binder' ->
          VVarSet.mem (Binder.get_var binder') fv.free)
    in
    if List.is_empty binders
    then get_fv_module fv acc tl
    else (
      let env =
        List.fold binders ~init:fv ~f:(fun env binder' ->
            { env with free = VVarSet.remove (Binder.get_var binder') env.free })
      in
      let env', expr = get_fv expr in
      let env = FreeVar.merge env @@ env' in
      get_fv_module
        env
        ({ hd with wrap_content = D_irrefutable_match { pattern; expr; attr } } :: acc)
        tl)
  | hd :: tl -> get_fv_module fv (hd :: acc) tl


and get_fv_module_path fv path =
  let new_fv = FreeVar.push_at_path fv path in
  new_fv, path


and get_fv_module_expr fv x =
  match x.wrap_content with
  | M_struct prg ->
    (* TODO: user [get_fv_program] & removed [get_fv_module] *)
    let new_env, prg = get_fv_module fv [] @@ List.rev prg in
    new_env, { x with wrap_content = M_struct prg }
  | M_module_path path ->
    let new_env, path = get_fv_module_path fv path in
    new_env, { x with wrap_content = M_module_path path }
  | M_variable v ->
    let new_env = FreeVar.push fv v in
    new_env, x


and get_fv_program (fv : FreeVar.t) acc : program -> _ * program = function
  | [] -> fv, acc
  | ({ Location.wrap_content = D_irrefutable_match { pattern; expr; attr }; _ } as hd)
    :: tl ->
    let binders =
      List.filter (Pattern.binders pattern) ~f:(fun binder' ->
          VVarSet.mem (Binder.get_var binder') fv.free)
    in
    if List.is_empty binders
    then get_fv_program fv acc tl
    else (
      let env =
        List.fold binders ~init:fv ~f:(fun fv binder' ->
            FreeVar.{ fv with free = VVarSet.remove (Binder.get_var binder') fv.free })
      in
      let env', expr = get_fv expr in
      let env = FreeVar.merge env @@ env' in
      get_fv_program
        env
        ({ hd with wrap_content = D_irrefutable_match { pattern; expr; attr } } :: acc)
        tl)
  | ({ Location.wrap_content = D_value { binder; expr; attr }; _ } as hd) :: tl ->
    let binder' = binder in
    if VVarSet.mem (Binder.get_var binder') fv.free
    then (
      let env = { fv with free = VVarSet.remove (Binder.get_var binder') fv.free } in
      let env', expr = get_fv expr in
      let env = FreeVar.merge env @@ env' in
      get_fv_program
        env
        ({ hd with wrap_content = D_value { binder; expr; attr } } :: acc)
        tl)
    else get_fv_program fv acc tl
  | ({ Location.wrap_content = D_module { module_binder; module_; module_attr }; _ } as
    hd)
    :: tl ->
    (match MVarMap.find_opt module_binder fv.module_ with
    | Some env' ->
      let new_env, module_ = get_fv_module_expr env' module_ in
      let env = { fv with module_ = MVarMap.remove module_binder fv.module_ } in
      let env = FreeVar.merge env new_env in
      get_fv_program
        env
        ({ hd with wrap_content = D_module { module_binder; module_; module_attr } }
        :: acc)
        tl
    | None -> get_fv_program fv acc tl)
  | ({ Location.wrap_content = D_open path; _ } as hd) :: tl ->
    let new_env, path = get_fv_module_path fv path in
    let env = FreeVar.merge fv new_env in
    get_fv_program env ({ hd with wrap_content = D_open path } :: acc) tl
  | ({ Location.wrap_content = D_include path; _ } as hd) :: tl ->
    let new_env, path = get_fv_module_path fv path in
    let env = FreeVar.merge fv new_env in
    get_fv_program env ({ hd with wrap_content = D_include path } :: acc) tl
  | hd :: tl -> get_fv_program fv (hd :: acc) tl


let collect_binder_module_path (ms : scope) path =
  Simple_utils.List.Ne.fold_left
    ~init:(ms, [])
    ~f:(fun (ms, _) b ->
      Option.value_or_thunk
        ~default:(fun () ->
          failwith
          @@ Format.asprintf
               "collect binder_module_path failed for var : %a in scope %a"
               Module_var.pp
               b
               pp_scope
               ms)
        (MVarMap.find_opt b ms.t))
    path


let rec collect_binder_module_expr (ms : scope) (mexpr : module_expr) =
  match mexpr.wrap_content with
  | M_struct prg -> collect_binder_declarations ms prg
  | M_variable var ->
    let ms' =
      Option.value_or_thunk
        ~default:(fun () -> failwith "collect variable failed")
        (MVarMap.find_opt var ms.t)
    in
    ms'
  | M_module_path path -> collect_binder_module_path ms path


and collect_binder_declarations ms prg =
  List.fold_left
    ~f:(fun (ms, binders) d ->
      match Location.unwrap d with
      | D_value { binder = { var; _ }; _ } -> ms, var :: binders
      | D_irrefutable_match { pattern = { wrap_content = P_var { var; _ }; _ }; _ } ->
        ms, var :: binders
      | D_irrefutable_match _ | D_type _ -> ms, binders
      | D_module { module_binder; module_; _ } ->
        let ms' = collect_binder_module_expr ms module_ in
        { t = MVarMap.add module_binder ms' ms.t }, binders
      | D_open path | D_include path ->
        let ms', bs = collect_binder_module_path ms path in
        { t = MVarMap.union (fun _ _ a -> Some a) ms.t ms'.t }, bs @ binders)
    ~init:(ms, [])
    prg


let rec remove_unused_in_module_expr
    (ms : scope)
    binder
    (fv : FreeVar.t)
    (mexpr : module_expr)
    : FreeVar.t * module_expr option
  =
  let return fv wrap_content : _ * module_expr option =
    fv, Some { mexpr with wrap_content }
  in
  match mexpr.wrap_content with
  | M_struct prg ->
    (match FreeVar.get_module_opt fv binder with
    | Some fv' ->
      let fv', prg =
        remove_unused_in_declaration_list (fst @@ MVarMap.find binder ms.t) ~fv:fv' prg
      in
      let fv = FreeVar.merge fv fv' in
      return fv @@ M_struct prg
    | None -> fv, None)
  | M_variable var ->
    (match FreeVar.handle_alias fv (var, []) binder with
    | Some fv' ->
      let fv = FreeVar.merge fv fv' in
      return fv @@ M_variable var
    | None -> fv, None)
  | M_module_path path ->
    (match FreeVar.handle_alias fv path binder with
    | Some fv' ->
      let fv = FreeVar.merge fv fv' in
      return fv @@ M_module_path path
    | None -> fv, None)


and remove_unused_in_declaration_list (ms : scope) ~fv (prg : module_)
    : FreeVar.t * module_
  =
  let self ms ~fv prg = remove_unused_in_declaration_list ms ~fv prg in
  match prg with
  | [] -> fv, []
  | ({ wrap_content = D_value { binder = { var; _ }; expr; _ }; location = _ } as d)
    :: prg ->
    (* check if this is the entrypoint *)
    let fv', prg = self ms ~fv prg in
    let fv, _expr = get_fv expr in
    (* check that it is usefull*)
    (* check that it is usefull*)
    if VVarSet.mem var fv'.free
    then (
      let fv' = { fv' with free = VVarSet.remove var fv'.free } in
      let fv = FreeVar.merge fv fv' in
      fv, d :: prg
      (* drop because useless *))
    else fv', prg
  | ({ wrap_content = D_irrefutable_match { pattern = irr; expr; _ }; location = _ } as d)
    :: prg ->
    (* check if this is the entrypoint *)
    (* check that it is usefull*)
    let fv', prg = self ms ~fv prg in
    let fv, _expr = get_fv expr in
    let binders =
      List.filter (Pattern.binders irr) ~f:(fun binder' ->
          VVarSet.mem (Binder.get_var binder') fv'.free)
    in
    if not @@ List.is_empty binders
    then (
      let fv' =
        List.fold binders ~init:fv' ~f:(fun env binder' ->
            { env with free = VVarSet.remove (Binder.get_var binder') env.free })
      in
      let fv = FreeVar.merge fv' @@ fv in
      fv, d :: prg
      (* drop because useless *))
    else fv', prg
  | ({ wrap_content = D_type _; location = _ } as d) :: prg ->
    (* always keep ?*)
    let fv, prg = self ms ~fv prg in
    fv, d :: prg
  | ({ wrap_content = D_module { module_binder; module_; module_attr }; location = _ } as
    d)
    :: prg ->
    let ms' = collect_binder_module_expr ms module_ in
    let ms = { t = MVarMap.add module_binder ms' ms.t } in
    let fv', prg = self ms ~fv prg in
    let fv, mod_ = remove_unused_in_module_expr ms module_binder fv' module_ in
    (match mod_ with
    | Some module_ ->
      ( FreeVar.merge fv fv'
      , { d with wrap_content = D_module { module_binder; module_; module_attr } } :: prg
      )
    | None -> fv', prg)
  | ({ wrap_content = D_open op; location = _ } as d) :: prg ->
    let fv, prg = self ms ~fv prg in
    (match FreeVar.lift fv ms op with
    | Some fv -> fv, d :: prg
    | None -> fv, prg)
  | ({ wrap_content = D_include inc; location = _ } as d) :: prg ->
    let fv, prg = self ms ~fv prg in
    (match FreeVar.lift fv ms inc with
    | Some fv -> fv, d :: prg
    | None -> fv, prg)


(* Bottom up and top-down pass in order the process the open and includes correctly
    The algorithm collect the content of module on the way down and report usage of variable on the way
    up. The recursion stop way locating the entrypoint or if it reach the end of the program, then
    it throw an error

    For open/include, it compare the content collected in the module with the free_variable on the way up
    so it can lift them *)
let remove_unused ~raise : Value_var.t -> program -> program =
 fun entrypoint prg ->
  let rec aux ms (prg : program) =
    let self ms prg = aux ms prg in
    match prg with
    | [] ->
      (* no entrypoint *)
      raise.error
        (Errors.corner_case
        @@ Format.asprintf "Entrypoint '%a' not found" Value_var.pp entrypoint)
    | ({ wrap_content = D_value { binder = { var; _ }; expr; _ }; location = _ } as d)
      :: prg ->
      (* get free vars in expr 1*)
      let fv, _expr = get_fv expr in
      (* check if this is the entrypoint *)
      if Value_var.equal var entrypoint
      then fv, [ d ]
      else (
        (* get free variable from the rest of the program *)
        let fv', prg = self ms prg in
        (* check that it is usefull*)
        if VVarSet.mem var fv'.free
        then (
          let fv' = { fv' with free = VVarSet.remove var fv'.free } in
          let fv = FreeVar.merge fv fv' in
          fv, d :: prg
          (* drop because useless *))
        else fv', prg)
    | ({ wrap_content = D_irrefutable_match { pattern = irr; expr; _ }; location = _ } as
      d)
      :: prg ->
      (* get free vars in expr 1*)
      let fv, _expr = get_fv expr in
      (* check if this is the entrypoint *)
      (match Location.unwrap irr with
      | P_var { var; _ } when Value_var.equal var entrypoint -> fv, [ d ]
      | _ ->
        (* check that it is usefull*)
        let fv', prg = self ms prg in
        let binders =
          List.filter (Pattern.binders irr) ~f:(fun binder' ->
              VVarSet.mem (Binder.get_var binder') fv'.free)
        in
        if not @@ List.is_empty binders
        then (
          let fv' =
            List.fold binders ~init:fv' ~f:(fun env binder' ->
                { env with free = VVarSet.remove (Binder.get_var binder') env.free })
          in
          let fv = FreeVar.merge fv' fv in
          fv, d :: prg
          (* drop because useless *))
        else fv', prg)
    | ({ wrap_content = D_type _; location = _ } as d) :: prg ->
      (* always keep ?*)
      let fv, prg = self ms prg in
      fv, d :: prg
    | ({ wrap_content = D_module { module_binder; module_; module_attr }; location = _ }
      as d)
      :: prg ->
      let ms' = collect_binder_module_expr ms module_ in
      let ms = { t = MVarMap.add module_binder ms' ms.t } in
      let fv', prg = self ms prg in
      let fv, mod_ = remove_unused_in_module_expr ms module_binder fv' module_ in
      (match mod_ with
      | Some module_ ->
        ( FreeVar.merge fv fv'
        , { d with wrap_content = D_module { module_binder; module_; module_attr } }
          :: prg )
      | None -> fv', prg)
    | ({ wrap_content = D_open op; location = _ } as d) :: prg ->
      let fv, prg = self ms prg in
      (match FreeVar.lift fv ms op with
      | Some fv -> fv, d :: prg
      | None -> fv, prg)
    | ({ wrap_content = D_include inc; location = _ } as d) :: prg ->
      let fv, prg = self ms prg in
      (match FreeVar.lift fv ms inc with
      | Some fv -> fv, d :: prg
      | None -> fv, prg)
  in
  snd @@ aux { t = MVarMap.empty } prg


let remove_unused_for_views : program -> program =
 fun prg ->
  (* Process declaration in reverse order *)
  let prg_decls = List.rev prg in
  (* Format.printf "prg_decls:%a\n" (Ast_typed.PP.program ~use_hidden:false) prg ; *)
  let envs =
    List.filter_map prg_decls ~f:(fun decl ->
        match decl.wrap_content with
        | D_value dc when dc.attr.view ->
          let rhs_env, _ = get_fv dc.expr in
          let lhs_env =
            FreeVar.{ empty with free = VVarSet.of_list [ Binder.get_var dc.binder ] }
          in
          Some (lhs_env, rhs_env)
        | D_irrefutable_match dc when dc.attr.view ->
          let rhs_env, _ = get_fv dc.expr in
          let lhs_env =
            FreeVar.
              { empty with
                free =
                  VVarSet.of_list
                    (List.map ~f:Binder.get_var (Pattern.binders dc.pattern))
              }
          in
          Some (lhs_env, rhs_env)
        | D_value _
        | D_irrefutable_match _
        | D_type _
        | D_module _
        | D_open _
        | D_include _ -> None)
  in
  (* lhs_envs = variables bound by declaration ; rhs_envs = free variables in declaration rhs *)
  let lhs_envs, rhs_envs = List.unzip envs in
  let env = FreeVar.(merge (unions lhs_envs) (unions rhs_envs)) in
  let _, module_ = get_fv_program env [] prg_decls in
  module_


let remove_unused_expression : expression -> program -> expression * program =
 fun expr prg ->
  (* Process declaration in reverse order *)
  let prg_decls = List.rev prg in
  let env, main_expr = get_fv expr in
  let _, module_ = get_fv_program env [] prg_decls in
  main_expr, module_
