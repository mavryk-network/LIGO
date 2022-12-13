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

type env =
  { env : env MVarMap.t
  ; used_var : VVarSet.t
  ; used_mut_var : VVarSet.t
  }

let empty_env =
  { env = MVarMap.empty; used_var = VVarSet.empty; used_mut_var = VVarSet.empty }


let rec pp_env ppf env =
  Format.fprintf
    ppf
    "{env: %a;used_var: %a}"
    (Simple_utils.PP_helpers.list_sep_d (fun ppf (k, v) ->
         Format.fprintf ppf "(%a,%a)" Module_var.pp k pp_env v))
    (MVarMap.to_kv_list env.env)
    (Simple_utils.PP_helpers.list_sep_d Value_var.pp)
    (VVarSet.elements env.used_var)


(* Detect and remove unesed declaration *)
let rec merge_env
    { env = x1; used_var = y1; used_mut_var = z1 }
    { env = x2; used_var = y2; used_mut_var = z2 }
  =
  let aux _ a b = Some (merge_env a b) in
  { env = MVarMap.union aux x1 x2
  ; used_var = VVarSet.union y1 y2
  ; used_mut_var = VVarSet.union z1 z2
  }


and unions l = List.fold l ~init:empty_env ~f:merge_env

and get_fv expr =
  let self = get_fv in
  let return env expression_content = env, { expr with expression_content } in
  let get_fv_lambda Lambda.{ binder; output_type; result } =
    let env, result = self result in
    ( (match Param.get_mut_flag binder with
      | Immutable ->
        { env with used_var = VVarSet.remove (Param.get_var binder) @@ env.used_var }
      | Mutable ->
        { env with
          used_mut_var = VVarSet.remove (Param.get_var binder) @@ env.used_mut_var
        })
    , Lambda.{ binder; output_type; result } )
  in
  match expr.expression_content with
  | E_variable v ->
    return { empty_env with used_var = VVarSet.singleton v } @@ E_variable v
  | (E_literal _ | E_raw_code _) as ec ->
    return { empty_env with used_var = VVarSet.empty } @@ ec
  | E_constant { cons_name; arguments } ->
    let env_lst, arguments = List.unzip @@ List.map ~f:self arguments in
    return (unions @@ env_lst) @@ E_constant { cons_name; arguments }
  | E_application { lamb; args } ->
    let env_l, lamb = self lamb in
    let env_a, args = self args in
    return (merge_env env_l env_a) @@ E_application { lamb; args }
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
    return { env with used_var = VVarSet.remove fun_name env.used_var }
    @@ E_recursive { fun_name; lambda; fun_type }
  | E_constructor { constructor; element } ->
    let env, element = self element in
    return env @@ E_constructor { constructor; element }
  | E_matching { matchee; cases } ->
    let env, matchee = self matchee in
    let env_c, cases = get_fv_cases cases in
    return (merge_env env env_c) @@ E_matching { matchee; cases }
  | E_record m ->
    let res = Record.map ~f:self m in
    let keys, env_exp = List.unzip @@ Record.LMap.to_kv_list res in
    let env, exp = List.unzip env_exp in
    let m = Record.of_list @@ List.zip_exn keys exp in
    return (unions env) @@ E_record m
  | E_update { struct_; path; update } ->
    let env_r, struct_ = self struct_ in
    let env_u, update = self update in
    return (merge_env env_r env_u) @@ E_update { struct_; path; update }
  | E_accessor { struct_; path } ->
    let env, struct_ = self struct_ in
    return env @@ E_accessor { struct_; path }
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let env, let_result = self let_result in
    let used_var =
      List.fold (Pattern.binders let_binder) ~init:env.used_var ~f:(fun used_var b ->
          VVarSet.remove (Binder.get_var b) used_var)
    in
    let env = { env with used_var } in
    let env', rhs = self rhs in
    return (merge_env env env') @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_mod_in { module_binder; rhs; let_result } ->
    let env, let_result = self let_result in
    (match MVarMap.find_opt module_binder env.env with
    | Some env' ->
      let env = { env with env = MVarMap.remove module_binder env.env } in
      let env', rhs = get_fv_module_expr env' rhs in
      return (merge_env env env') @@ E_mod_in { module_binder; rhs; let_result }
    | None -> env, let_result)
  | E_module_accessor { module_path; element } ->
    let init = { empty_env with used_var = VVarSet.singleton element } in
    let env =
      List.fold_right
        module_path
        ~f:(fun module_name env ->
          { empty_env with env = MVarMap.singleton module_name env })
        ~init
    in
    return env @@ E_module_accessor { module_path; element }
  | E_assign { binder; expression } ->
    let env, expression = self expression in
    return env @@ E_assign { binder; expression }
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let env, let_result = self let_result in
    let binders = Pattern.binders let_binder in
    let used_mut_var =
      List.fold binders ~init:env.used_mut_var ~f:(fun used_var b ->
          VVarSet.remove (Binder.get_var b) used_var)
    in
    let env = { env with used_mut_var } in
    let env', rhs = self rhs in
    return (merge_env env env')
    @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_deref var ->
    return { empty_env with used_mut_var = VVarSet.singleton var } @@ E_deref var
  | E_while { cond; body } ->
    let env1, cond = self cond
    and env2, body = self body in
    return (merge_env env1 env2) @@ E_while { cond; body }
  | E_for { binder; start; final; incr; f_body } ->
    let env1, start = self start
    and env2, final = self final
    and env3, incr = self incr in
    let env' = unions [ env1; env2; env3 ] in
    let env, f_body = self f_body in
    let env = { env with used_var = VVarSet.remove binder env.used_var } in
    return (merge_env env' env) @@ E_for { binder; start; final; incr; f_body }
  | E_for_each { fe_binder; collection; collection_type; fe_body } ->
    let coll_env, collection = self collection in
    let fe_binder_set =
      let binder1, binder2 = fe_binder in
      binder1 :: Option.to_list binder2 |> VVarSet.of_list
    in
    let body_env, fe_body = self fe_body in
    let body_env =
      { body_env with used_var = VVarSet.diff body_env.used_var fe_binder_set }
    in
    return (merge_env coll_env body_env)
    @@ E_for_each { fe_binder; collection; collection_type; fe_body }


and get_fv_cases : _ Match_expr.match_case list -> env * _ Match_expr.match_case list =
 fun ms ->
  let envs =
    List.map ms ~f:(fun { pattern; body } ->
        let env, body = get_fv body in
        let binders = Pattern.binders pattern |> List.map ~f:Binder.get_var in
        let used_var = List.fold_right binders ~init:env.used_var ~f:VVarSet.remove in
        let env = { env with used_var } in
        env, Match_expr.{ pattern; body })
  in
  let envs, ms = List.unzip envs in
  unions envs, ms


and get_fv_module (env : env) acc = function
  | [] -> env, acc
  | ({ Location.wrap_content = D_value { binder; expr; attr }; _ } as hd) :: tl ->
    let binder' = binder in
    if VVarSet.mem (Binder.get_var binder') env.used_var
    then (
      let env =
        { env with used_var = VVarSet.remove (Binder.get_var binder') env.used_var }
      in
      let env', expr = get_fv expr in
      let env = merge_env env @@ env' in
      get_fv_module
        env
        ({ hd with wrap_content = D_value { binder; expr; attr } } :: acc)
        tl)
    else get_fv_module env acc tl
  | ({ Location.wrap_content = D_module { module_binder; module_; module_attr }; _ } as
    hd)
    :: tl ->
    (match MVarMap.find_opt module_binder env.env with
    | Some env' ->
      let new_env, module_ = get_fv_module_expr env' module_ in
      let env = { env with env = MVarMap.remove module_binder env.env } in
      let env = merge_env env new_env in
      get_fv_module
        env
        ({ hd with wrap_content = D_module { module_binder; module_; module_attr } }
        :: acc)
        tl
    | None -> get_fv_module env acc tl)
  | ({ Location.wrap_content = D_irrefutable_match { pattern; expr; attr }; _ } as hd)
    :: tl ->
    let binders =
      List.filter (Pattern.binders pattern) ~f:(fun binder' ->
          VVarSet.mem (Binder.get_var binder') env.used_var)
    in
    if List.is_empty binders
    then get_fv_module env acc tl
    else (
      let env =
        List.fold binders ~init:env ~f:(fun env binder' ->
            { env with used_var = VVarSet.remove (Binder.get_var binder') env.used_var })
      in
      let env', expr = get_fv expr in
      let env = merge_env env @@ env' in
      get_fv_module
        env
        ({ hd with wrap_content = D_irrefutable_match { pattern; expr; attr } } :: acc)
        tl)
  | hd :: tl -> get_fv_module env (hd :: acc) tl


and get_fv_module_path env path =
  let rec push_env (name, name_lst) toto =
    match name_lst with
    | [] -> { empty_env with env = MVarMap.singleton name toto }
    | hd :: tl ->
      { empty_env with env = MVarMap.singleton name @@ push_env (hd, tl) toto }
  in
  let new_env = push_env path env in
  new_env, path


and get_fv_module_expr env x =
  match x.wrap_content with
  | M_struct prg ->
    (* TODO: user [get_fv_program] & removed [get_fv_module] *)
    let new_env, prg = get_fv_module env [] @@ List.rev prg in
    new_env, { x with wrap_content = M_struct prg }
  | M_module_path path ->
    let new_env, path = get_fv_module_path env path in
    new_env, { x with wrap_content = M_module_path path }
  | M_variable v ->
    let new_env = { empty_env with env = MVarMap.singleton v env } in
    new_env, x


and get_fv_program (env : env) acc : program -> _ * program = function
  | [] -> env, acc
  | ({ Location.wrap_content = D_irrefutable_match { pattern; expr; attr }; _ } as hd)
    :: tl ->
    let binders =
      List.filter (Pattern.binders pattern) ~f:(fun binder' ->
          VVarSet.mem (Binder.get_var binder') env.used_var)
    in
    if List.is_empty binders
    then get_fv_program env acc tl
    else (
      let env =
        List.fold binders ~init:env ~f:(fun env binder' ->
            { env with used_var = VVarSet.remove (Binder.get_var binder') env.used_var })
      in
      let env', expr = get_fv expr in
      let env = merge_env env @@ env' in
      get_fv_program
        env
        ({ hd with wrap_content = D_irrefutable_match { pattern; expr; attr } } :: acc)
        tl)
  | ({ Location.wrap_content = D_value { binder; expr; attr }; _ } as hd) :: tl ->
    let binder' = binder in
    if VVarSet.mem (Binder.get_var binder') env.used_var
    then (
      let env =
        { env with used_var = VVarSet.remove (Binder.get_var binder') env.used_var }
      in
      let env', expr = get_fv expr in
      let env = merge_env env @@ env' in
      get_fv_program
        env
        ({ hd with wrap_content = D_value { binder; expr; attr } } :: acc)
        tl)
    else get_fv_program env acc tl
  | ({ Location.wrap_content = D_module { module_binder; module_; module_attr }; _ } as
    hd)
    :: tl ->
    (match MVarMap.find_opt module_binder env.env with
    | Some env' ->
      let new_env, module_ = get_fv_module_expr env' module_ in
      let env = { env with env = MVarMap.remove module_binder env.env } in
      let env = merge_env env new_env in
      get_fv_program
        env
        ({ hd with wrap_content = D_module { module_binder; module_; module_attr } }
        :: acc)
        tl
    | None -> get_fv_program env acc tl)
  | ({ Location.wrap_content = D_open path; _ } as hd) :: tl ->
    let new_env, path = get_fv_module_path env path in
    let env = merge_env env new_env in
    get_fv_program env ({ hd with wrap_content = D_open path } :: acc) tl
  | ({ Location.wrap_content = D_include path; _ } as hd) :: tl ->
    let new_env, path = get_fv_module_path env path in
    let env = merge_env env new_env in
    get_fv_program env ({ hd with wrap_content = D_include path } :: acc) tl
  | hd :: tl -> get_fv_program env (hd :: acc) tl


module MMap = Simple_utils.Map.Make (Module_var)

type scope = { t : (scope * declaration option) MMap.t }

(*
let remove_unused ~raise : contract_pass_data -> program -> program =
 fun contract_pass_data prg ->
  (* Process declaration in reverse order *)
  let rec scan (scope : scope) (decl : declaration) =
    match decl.wrap_content with
    | D_value { binder = { var; _ }; _ }
    | D_irrefutable_match { pattern = { wrap_content = P_var { var; _ }; _ }; _ } ->
      scope, (decl, not (Value_var.equal var contract_pass_data.main_name))
    | D_irrefutable_match _ | D_type _ -> scope, (decl, true)
    | D_module { module_binder; module_; _ } ->
      scan_module_expr scope decl module_binder module_
    | D_open path | D_include path -> scan_module_path scope decl path
  and scan_module_expr scope decl module_binder (mexpr : module_expr) =
    match mexpr.wrap_content with
    | M_struct prg ->
      let scanned, list = List.fold_map ~f:scan ~init:scope prg in
      let list = List.drop_while ~f:(fun (_, b) -> Bool.equal true b) @@ List.rev list in
      let list, _ = List.unzip list in
      { t = MMap.add module_binder (scanned, List.hd list) scope.t }, (decl, true)
    | M_variable var ->
      let scanned, res = scan_module_path scope decl (var, []) in
      { t = MMap.union (fun _ _ a -> Some a) scope.t scanned.t }, res
    | M_module_path path ->
      let scanned, res = scan_module_path scope decl path in
      { t = MMap.union (fun _ _ a -> Some a) scope.t scanned.t }, res
  and scan_module_path scope decl path =
    let mod_scope, res =
      Simple_utils.List.Ne.fold_left
        ~f:(fun (s, _b) b -> MMap.find b s.t)
        ~init:(scope, None)
        path
    in
    ( { t = MMap.union (fun _ _ a -> Some a) scope.t mod_scope.t }
    , (decl, Option.is_none res) )
  in
  let scope, prg_decls = List.fold_map ~f:scan ~init:{ t = MMap.empty } prg in
  (* Remove the definition after the main entry_point (can't be relevant), mostly remove the test *)
  let prg_decls = List.rev prg_decls |> List.drop_while ~f:snd |> List.map ~f:fst in
  let main_decl, prg_decls =
    trace_option ~raise (Errors.corner_case "Entrypoint not found")
    @@ Simple_utils.List.uncons prg_decls
  in
  let env =
    trace_option ~raise (Errors.corner_case "Entrypoint not found")
    @@
    let rec aux decl =
      match Location.unwrap decl with
      | D_value dc ->
        let env, _ = get_fv dc.expr in
        Some env
      | D_irrefutable_match dc ->
        let env, _ = get_fv dc.expr in
        Some env
      | D_open path | D_include path ->
        let _scope, decl =
          Simple_utils.List.Ne.fold_left
            ~f:(fun (s, _d) p -> MMap.find p s.t)
            ~init:(scope, None)
            path
        in
        Option.map ~f:aux decl |> Option.join
      | D_type _ | D_module _ -> None
    in
    aux main_decl
  in
  let _, module_ = get_fv_program env [ main_decl ] prg_decls in
  module_
*)

type scope = { t : (scope * Value_var.t list) MMap.t }

let collect_binder_module_path (ms : scope) path =
  Simple_utils.List.Ne.fold_left
    ~init:(ms, [])
    ~f:(fun (ms, _) b -> MMap.find b ms.t)
    path


let rec collect_binder_module_expr (ms : scope) (mexpr : module_expr) =
  match mexpr.wrap_content with
  | M_struct prg -> collect_binder_declarations ms prg
  | M_variable var ->
    let ms' = MMap.find var ms.t in
    ms'
  | M_module_path path -> collect_binder_module_path ms path


and collect_binder_declarations (ms : scope) prg =
  List.fold_left
    ~f:(fun (ms, binders) d ->
      match Location.unwrap d with
      | D_value { binder = { var; _ }; _ } -> ms, var :: binders
      | D_irrefutable_match { pattern = { wrap_content = P_var { var; _ }; _ }; _ } ->
        ms, var :: binders
      | D_irrefutable_match _ | D_type _ -> ms, binders
      | D_module { module_binder; module_; _ } ->
        let ms' = collect_binder_module_expr ms module_ in
        { t = MMap.add module_binder ms' ms.t }, binders
      | D_open path | D_include path ->
        let ms', bs = collect_binder_module_path ms path in
        { t = MMap.union (fun _ _ a -> Some a) ms.t ms'.t }, bs @ binders)
    ~init:(ms, [])
    prg


let remove_unused ~raise : Value_var.t -> program -> program =
 fun entrypoint prg ->
  let rec aux ms (prg : program) top_level =
    let self ms prg = aux ms prg false in
    match prg with
    | [] ->
      raise.error
        (Errors.corner_case
        @@ Format.asprintf "Entrypoint '%a' not found" Value_var.pp entrypoint)
    | ({ wrap_content = D_value { binder = { var; _ }; expr; _ }; location = _ } as d)
      :: prg ->
      let fv, _expr = get_fv expr in
      (* check if this is the entrypoint *)
      if top_level && Value_var.equal var entrypoint
      then fv, [ d ]
      else (
        let fv', prg = self ms prg in
        (* check that it is usefull*)
        if VVarSet.mem var fv.used_var
        then (
          let fv' = { fv' with used_var = VVarSet.remove var fv'.used_var } in
          let fv = merge_env fv fv' in
          fv, d :: prg
          (* drop because useless *))
        else fv', prg)
    | ({ wrap_content = D_irrefutable_match { pattern = irr; expr; _ }; location = _ } as
      d)
      :: prg ->
      let fv, _expr = get_fv expr in
      (* check if this is the entrypoint *)
      (match Location.unwrap irr with
      | P_var { var; _ } when top_level && Value_var.equal var entrypoint -> fv, [ d ]
      | _ ->
        (* check that it is usefull*)
        let fv', prg = self ms prg in
        let binders =
          List.filter (Pattern.binders irr) ~f:(fun binder' ->
              VVarSet.mem (Binder.get_var binder') fv'.used_var)
        in
        if List.is_empty binders
        then (
          let fv' =
            List.fold binders ~init:fv ~f:(fun env binder' ->
                { env with
                  used_var = VVarSet.remove (Binder.get_var binder') env.used_var
                })
          in
          let fv = merge_env fv' @@ fv in
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
      let ms, _binders = collect_binder_module_expr ms module_ in
      let fv, prg = self ms prg in
      let fv, mod_ = remove_unused_in_module_expr fv module_ in
      (match mod_ with
      | Some module_ ->
        ( fv
        , { d with wrap_content = D_module { module_binder; module_; module_attr } }
          :: prg )
      | None -> fv, prg)
    | ({ wrap_content = D_open op; location = _ } as d) :: prg ->
      let fv, prg = self ms prg in
      fv, d :: prg
    | ({ wrap_content = D_include inc; location = _ } as d) :: prg ->
      let fv, prg = self ms prg in
      fv, d :: prg
  in
  snd @@ aux ms prg true


let remove_unused_for_views : program -> program =
 fun prg ->
  (* Process declaration in reverse order *)
  let prg_decls = List.rev prg in
  (* Format.eprintf "prg_decls:%a\n" (Ast_typed.PP.program ~use_hidden:false) prg ; *)
  let envs =
    List.filter_map prg_decls ~f:(fun decl ->
        match decl.wrap_content with
        | D_value dc when dc.attr.view ->
          let rhs_env, _ = get_fv dc.expr in
          let lhs_env =
            { empty_env with used_var = VVarSet.of_list [ Binder.get_var dc.binder ] }
          in
          Some (lhs_env, rhs_env)
        | D_irrefutable_match dc when dc.attr.view ->
          let rhs_env, _ = get_fv dc.expr in
          let lhs_env =
            { empty_env with
              used_var =
                VVarSet.of_list (List.map ~f:Binder.get_var (Pattern.binders dc.pattern))
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
  let env = merge_env (unions lhs_envs) (unions rhs_envs) in
  let _, module_ = get_fv_program env [] prg_decls in
  module_


let remove_unused_expression : expression -> program -> expression * program =
 fun expr prg ->
  (* Process declaration in reverse order *)
  let prg_decls = List.rev prg in
  let env, main_expr = get_fv expr in
  let _, module_ = get_fv_program env [] prg_decls in
  main_expr, module_
