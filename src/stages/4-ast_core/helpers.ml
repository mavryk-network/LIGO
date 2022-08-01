open Types

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map ~f:(fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all ~f:(fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let tuple_of_record (m: _ LMap.t) =
  let aux i =
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind ~f: (fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux

(* This function transforms an application expression `l e1 ... en` into the pair `([ e1 ; ... ; en ] , l)` *)
let destruct_applications (e : expression) =
  let rec destruct_applications acc (lamb : expression) =
    match lamb.term_content with
    | T_application {lamb;args} ->
       destruct_applications (args :: acc) lamb
    | _ ->
       (lamb, acc) in
  destruct_applications [] e

(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) = match t.term_content with
    | T_pi { binder = { var; ascr = Some { term_content = T_type; _ }; _} ; result } ->
       destruct_for_alls (var :: type_vars) result
    | _ -> (List.rev type_vars, t)
  in destruct_for_alls [] t

module Free_variables = struct

  module VarSet = Caml.Set.Make(TermVar)
  module ModVarSet = Caml.Set.Make(ModuleVar)
  module ModVarMap = Caml.Map.Make(ModuleVar)

  type module_env = { mod_var_set : ModVarSet.t; module_env : module_env_map; var_set : VarSet.t }
  and module_env_map = module_env ModVarMap.t

  let empty = 
    { mod_var_set = ModVarSet.empty; module_env = ModVarMap.empty; var_set = VarSet.empty }

  let rec merge 
    { mod_var_set = mvar_set1; module_env = mod_env1; var_set = var_set1 } 
    { mod_var_set = mvar_set2; module_env = mod_env2; var_set = var_set2 }
  =
    { mod_var_set = ModVarSet.union mvar_set1 mvar_set2 ;
      module_env = ModVarMap.union (fun _ me1 me2 -> Some (merge me1 me2)) mod_env1 mod_env2 ;
      var_set = VarSet.union var_set1 var_set2
    }

  let remove_var var mod_env = 
    { mod_env with var_set = VarSet.remove var mod_env.var_set }

  let remove_mod_var mod_var mod_env = 
    { mod_env with mod_var_set = ModVarSet.remove mod_var mod_env.mod_var_set }

  let unions : module_env list -> module_env =
    fun l -> List.fold l ~init:empty ~f:merge

  let rec fv_term : term -> module_env = fun term -> 
    let self = fv_term in
    match term.term_content with
    | T_variable v -> 
      { empty with var_set = VarSet.singleton v }
    | T_literal _ -> empty
    | T_constant { arguments; _ } ->
      unions (List.map ~f:self arguments)
    | T_application { lamb; args } ->
      unions [ self lamb; self args ]
    | T_lambda lambda ->
      fv_lambda lambda
    | T_recursive { fun_name; fun_type; lambda } ->
      let env_lambda = fv_lambda lambda in
      unions [ self fun_type; remove_var fun_name env_lambda ]
    | T_let_in { let_binder; rhs; let_result; _ } ->
      let var, env_ascr = fv_binder let_binder in
      unions [ self rhs; env_ascr; remove_var var (self let_result) ]
    | T_mod_in { module_binder; rhs; let_result } ->
      unions [ fv_module_expr rhs; remove_mod_var module_binder (self let_result) ]
    | T_module_accessor { module_path; _ } -> 
      (* TODO: Check this *)
      { empty with mod_var_set = ModVarSet.of_list module_path }
    | T_raw_code { code; _ } -> self code
    | T_constructor { element; _ } -> self element
    | T_matching { matchee; cases } ->
      merge (self matchee) (fv_cases cases)
    | T_record fields ->
      let fields = 
        fields
        |> LMap.map self 
        |> LMap.to_list 
      in
      unions fields
    | T_record_update { record; update; _ } ->
      unions [ self record; self update ]
    | T_record_accessor { record; _ } ->
      self record
    | T_ascription { anno_expr; type_annotation } ->
      unions [ self anno_expr; self type_annotation ]
    | T_assign { binder = _; expression } ->
      (* TODO: Check ignoring [binder] *)
      self expression
    | T_sum { fields ; _ } ->
      let fields = LMap.to_list fields |> List.map ~f:(fun ({ associated_type ; _ } : _ row_element_mini_c) -> fv_term associated_type) in
      unions fields
    | T_prod { fields ; _ } ->
      let fields = LMap.to_list fields |> List.map ~f:(fun ({ associated_type ; _ } : _ row_element_mini_c) -> fv_term associated_type) in
      unions fields
    | T_arrow { type1 ; type2 } ->
      unions [ fv_term type1; fv_term type2 ]
    | T_type -> empty
    | T_pi { binder; result } ->
      let var, env_ascr = fv_binder binder in
      unions [ env_ascr; remove_var var (self result) ]

  and fv_option t ~fv =
    Option.(value ~default:empty (t >>| fv))

  and fv_binder { var; ascr; _ } = 
    var, fv_option ascr ~fv:fv_term

  and fv_lambda { binder; output_type; result } =
    let var, env_ascr = fv_binder binder in
    unions [ fv_option output_type ~fv:fv_term; env_ascr; remove_var var (fv_term result) ] 

  and fv_pattern (pat : term pattern) = 
    let merge (var_set1, env1) (var_set2, env2) = 
      VarSet.union var_set1 var_set2, merge env1 env2
    in
    let fv_patterns pats = 
      pats
      |> List.map ~f:fv_pattern
      |> List.fold ~init:(VarSet.empty, empty) ~f:merge
    in
    match pat.wrap_content with
    | P_unit -> VarSet.empty, empty
    | P_var binder ->
      let var, env = fv_binder binder in
      VarSet.singleton var, env
    | P_tuple pats -> fv_patterns pats
    | P_list list_pat ->
      (match list_pat with
      | Cons (hd_pat, tl_pat) -> merge (fv_pattern hd_pat) (fv_pattern tl_pat)
      | List pats -> fv_patterns pats)
    | P_record (_, pats) -> fv_patterns pats
    | P_variant (_, pat) -> fv_pattern pat

  and fv_case { pattern; body } = 
    let bound, pat_env = fv_pattern pattern in
    unions [ pat_env; VarSet.fold remove_var bound (fv_term body) ]

  and fv_cases cases = 
    unions (List.map ~f:fv_case cases)

  and fv_module_expr =
    fun mod_expr ->
      match mod_expr.wrap_content with
      | M_struct prg -> fv_module prg
      | M_variable _ | M_module_path _ -> empty

  and fv_module = 
    fun mod_ ->
      mod_
      |> List.map ~f:(fun (decl : declaration) ->
          match decl.wrap_content with
          | Declaration_constant {expr; _} ->
            fv_term expr
          | Declaration_module {module_; _} ->
            fv_module_expr module_
          | Declaration_type { type_expr; _ } -> fv_term type_expr)
      |> unions 
end
