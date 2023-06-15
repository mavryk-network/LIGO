open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils.Function
open Errors
module Location = Simple_utils.Location

(* 

fetch types in module:  storage ; type with [@dyn_entry] ?
fetch declaration in module: all [@entry] ; all [@view]
internally define dyn_storage as (storage, dyn_param -> storage -> operation list * storage) dyn_storage

for each ctor in [@dyn_entry] type :
  - fetch 'condition_on_set_<ctor>' delcaration
  - if not present, condition_on_set_<ctor> = fun _ -> true`


for each (ctor, assoc_ty) in [@dyn_entry] type : 
  - define set_<ctor> : (<assoc_ty> -> storage -> operation list * storage) -> <dyn_storage> -> operation list * <dyn_storage>
    = .. ez ..

for each [@entry] declarations:
  - fetch parameter type in the rhs
  - define [@entry] <entry> = : int -> dyn_storage -> operation list * dyn_storage

for each [@view] declarations:
  - fetch view parameter type <p_ty> in the rhs
  - fetch view result type <r_ty> in the rhs
  - define [@view] : p_ty -> dyn_storage -> r_ty
*)
include Flag.No_arg ()

let tv_storage ~loc = t_var ~loc @@ Ty_variable.of_input_var ~loc "storage"
let v_dyn_storage ~loc = Ty_variable.of_input_var ~loc "dyn_storage"
let tv_dyn_storage ~loc = t_var ~loc (v_dyn_storage ~loc)
let name = __MODULE__

type ez_row = (Label.t * ty_expr) list

let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_attr (attr, _); _ } when String.equal attr.key "dyn_storage" ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile ~raise:_ = Nothing

let rec compile ~raise =
  let program : _ program_ -> program =
   fun p ->
    match fetch_dyn_entry_type p with
    | Some dyn_entry_t ->
      let loc = Location.generated in
      let storage_t = fetch_static_storage_type ~raise p in
      let entry_ts = fetch_static_entry_parameter_types p in
      let view_ts = fetch_view_types p in
      let dynamic_storage_type =
        let loc = get_t_loc storage_t in
        t_module_app
          ~loc
          { constr =
              { module_path =
                  List.Ne.singleton
                    (Mod_variable.of_input_var ~loc "Dynamic_entrypoints_helpers")
              ; field = Ty_variable.of_input_var ~loc "dyn_storage"
              ; field_as_open = false
              }
          ; type_args = storage_t, [ make_ps_func ~loc dyn_entry_t storage_t ]
          }
      in
      let dyn_entry_rows = get_ez_row ~raise dyn_entry_t in
      let dyn_storage_type_decl =
        top_level_decl
          ~loc
          (d_type ~loc { name = v_dyn_storage ~loc; type_expr = dynamic_storage_type })
      in
      let condition_decls = on_set_conditions p dyn_entry_rows in
      let setter_decls = dyn_setters dynamic_storage_type dyn_entry_rows in
      let dyn_caller_decls = dyn_caller dynamic_storage_type dyn_entry_rows in
      let entry_fwd_decls = entry_forwarders dynamic_storage_type entry_ts in
      let view_fwd_decls = view_forwarders dynamic_storage_type view_ts in
      make_prg
        (strip_views_and_entry_attributes p
        @ [ dyn_storage_type_decl ]
        @ List.join
            [ condition_decls
            ; setter_decls
            ; dyn_caller_decls
            ; entry_fwd_decls
            ; view_fwd_decls
            ])
    | None -> make_prg p
  in
  Fold { idle_fold with program }


and get_ez_row ~raise : ty_expr -> ez_row =
 fun ty ->
  ty
  |> get_t_sum_raw
  |> trace_option ~raise (dynamic_entry_not_a_sum_type ty)
  |> List.map ~f:(fun (label, el) ->
         ( label
         , Option.value
             ~default:(tv_unit ~loc:Location.generated ())
             Non_linear_rows.(el.associated_type) ))


and find_decl : program_entry list -> f:(declaration -> 'a option) -> 'a option =
 fun p ~f ->
  p
  |> List.rev
  |> List.map ~f:get_pe
  |> List.find_map ~f:(function
         | PE_declaration d -> f d
         | _ -> None)


and filter_decl : type b. program_entry list -> f:(declaration -> b option) -> b list =
 fun p ~f ->
  p
  |> List.rev
  |> List.map ~f:get_pe
  |> List.filter_map ~f:(function
         | PE_declaration d -> f d
         | _ -> None)


and fetch_static_storage_type ~raise : program_entry list -> ty_expr =
 fun p ->
  p
  |> find_decl ~f:(fun d ->
         let open Simple_utils.Option in
         let* { name; type_expr } = get_d_type d in
         if Ligo_prim.Type_var.is_name name "storage" then Some type_expr else None)
  |> trace_option ~raise (no_dynamic_entry p)


and fetch_dyn_entry_type : program_entry list -> ty_expr option =
  find_decl ~f:(fun d ->
      let open Simple_utils.Option in
      let* { key; value }, ty_decl = get_d_attr d in
      if String.equal key "dyn_entry" && Option.is_none value
      then
        let* { name = _; type_expr } = get_d_type ty_decl in
        Some type_expr
      else None)


and get_first_param_t rhs_type pattern =
  let open Simple_utils.Option in
  let from_annot =
    let* rhs_type in
    Some (get_t_fun_lst rhs_type)
  in
  match from_annot with
  | Some (first :: _) -> Some first
  | _ ->
    let* ty, _ = get_p_var_typed pattern in
    List.hd (get_t_fun_lst ty)


and get_third_param_t rhs_type pattern =
  let open Simple_utils.Option in
  let from_annot =
    let* rhs_type in
    Some (get_t_fun_lst rhs_type)
  in
  match from_annot with
  | Some (_ :: _ :: third :: _) -> Some third
  | _ ->
    let* ty, _ = get_p_var_typed pattern in
    let* _, _, three = List.to_triple (get_t_fun_lst ty) in
    Some three


and fetch_static_entry_parameter_types : program_entry list -> (Variable.t * ty_expr) list
  =
 fun p ->
  p
  |> filter_decl ~f:(fun d ->
         let open Simple_utils.Option in
         let* { key; value }, d = get_d_attr d in
         if String.equal key "entry" && Option.is_none value
         then
           let* pattern, rhs_type = get_decl_and_ty d in
           let* name = List.to_singleton (get_pattern_binders pattern) in
           let* entry_param_ty = get_first_param_t rhs_type pattern in
           Some (name, entry_param_ty)
         else None)


and fetch_view_types : program_entry list -> (Variable.t * ty_expr * ty_expr) list =
 fun p ->
  p
  |> filter_decl ~f:(fun d ->
         let open Simple_utils.Option in
         let* { key; value }, d = get_d_attr d in
         if String.equal key "view" && Option.is_none value
         then
           let* pattern, rhs_type = get_decl_and_ty d in
           let* name = List.to_singleton (get_pattern_binders pattern) in
           let* view_param_ty = get_first_param_t rhs_type pattern in
           let* view_result_ty = get_third_param_t rhs_type pattern in
           Some (name, view_param_ty, view_result_ty)
         else None)


and make_ps_func ~loc : ty_expr -> ty_expr -> ty_expr =
 fun p s ->
  t_fun_of_list ~loc [ p; s; t_pair_raw ~loc [ t_list ~loc (tv_operation ~loc ()); s ] ]


and on_set_conditions : program_entry list -> ez_row -> program_entry list =
 fun p dyn_param_rows ->
  let loc = Location.generated in
  let labels_str = List.map ~f:(Label.to_string <@ fst) dyn_param_rows in
  let present_conditions =
    (* compute condition_on_set_ bindings that are present *)
    filter_decl p ~f:(fun d ->
        let open Simple_utils.Option in
        let* pattern, _ = get_decl_and_ty d in
        let* name = List.to_singleton (get_pattern_binders pattern) in
        List.find labels_str ~f:(fun label ->
            Variable.is_name name ("condition_on_set_" ^ String.uncapitalize label)))
  in
  let missing_conditions =
    List.filter
      ~f:(fun l -> not @@ List.exists ~f:(String.equal l) present_conditions)
      labels_str
  in
  let fun_const_true ~loc =
    (* this is (fun _ -> true) *)
    e_lambda
      ~loc
      { binder = Ligo_prim.Param.make (Variable.fresh ~loc ()) None
      ; output_type = None
      ; result = e_true ~loc
      }
  in
  List.map missing_conditions ~f:(fun str ->
      make_pe
        (PE_declaration
           (d_const
              ~loc
              Simple_decl.
                { pattern =
                    p_var
                      ~loc
                      (Variable.of_input_var
                         ~loc
                         ("condition_on_set_" ^ String.uncapitalize str))
                ; type_params = None
                ; rhs_type = Some (t_fun ~loc (tv_storage ~loc, tv_bool ~loc ()))
                ; let_rhs = fun_const_true ~loc
                })))


and dyn_setters : ty_expr -> ez_row -> program_entry list =
 fun dyn_storage_t dyn_param_rows ->
  let loc = Location.generated in
  List.map dyn_param_rows ~f:(fun (label, dyn_param_ty) ->
      let label = String.uncapitalize (Label.to_string label) in
      let pattern = p_var ~loc (Variable.of_input_var ~loc ("set_" ^ label)) in
      let rhs_type =
        Some
          (make_ps_func
             ~loc
             (make_ps_func ~loc dyn_param_ty (tv_storage ~loc))
             dyn_storage_t)
      in
      let args_prefixes = [ "condition_on_set_"; "enum_"; "get_" ] in
      make_decl_of_call_to_std_lib
        ~loc
        ~suffix:label
        ~std_lib_fun:"setter"
        ~args_prefixes
        pattern
        rhs_type)


and dyn_caller : ty_expr -> ez_row -> program_entry list =
 fun dyn_storage_t dyn_param_rows ->
  let loc = Location.generated in
  List.map dyn_param_rows ~f:(fun (label, dyn_param_ty) ->
      let label = String.uncapitalize (Label.to_string label) in
      let pattern = p_var ~loc (Variable.of_input_var ~loc label) in
      let rhs_type = Some (make_ps_func ~loc dyn_param_ty dyn_storage_t) in
      let args_prefixes = [ "enum_"; "make_" ] in
      make_decl_of_call_to_std_lib
        ~loc
        ~suffix:label
        ~std_lib_fun:"caller"
        ~args_prefixes
        pattern
        rhs_type)


and entry_forwarders : ty_expr -> (Variable.t * ty_expr) list -> program_entry list =
 fun dyn_storage_t entry_params ->
  let loc = Location.generated in
  List.map entry_params ~f:(fun (entry_name, entry_param_ty) ->
      let rhs_type = Some (make_ps_func ~loc entry_param_ty dyn_storage_t) in
      let let_rhs =
        e_application
          ~loc
          { lamb = mod_access_to_stdlib ~loc "forward_entry"
          ; args = e_variable ~loc entry_name
          }
      in
      top_level_decl
        ~loc
        ~attr:"entry"
        (d_const
           ~loc
           { pattern = p_var ~loc entry_name; type_params = None; rhs_type; let_rhs }))


and view_forwarders
    : ty_expr -> (Variable.t * ty_expr * ty_expr) list -> program_entry list
  =
 fun dyn_storage_t view_params ->
  let loc = Location.generated in
  List.map view_params ~f:(fun (view_name, view_param_ty, view_res_ty) ->
      let rhs_type =
        Some (t_fun_of_list ~loc [ view_param_ty; dyn_storage_t; view_res_ty ])
      in
      let let_rhs =
        e_application
          ~loc
          { lamb = mod_access_to_stdlib ~loc "forward_view"
          ; args = e_variable ~loc view_name
          }
      in
      top_level_decl
        ~loc
        ~attr:"view"
        (d_const
           ~loc
           { pattern = p_var ~loc view_name; type_params = None; rhs_type; let_rhs }))


and strip_views_and_entry_attributes : program_entry list -> program_entry list =
  List.map ~f:(fun p ->
      let opt =
        let open Simple_utils.Option in
        let* d = get_pe_declaration p in
        let* { key; value }, d' = get_d_attr d in
        let k_eq = String.equal key in
        if (k_eq "entry" || k_eq "view" || k_eq "dyn_entry") && Option.is_none value
        then Some (make_pe (PE_declaration d'))
        else None
      in
      Option.value ~default:p opt)


and make_decl_of_call_to_std_lib ~loc ~suffix ~std_lib_fun ~args_prefixes pattern rhs_type
  =
  (* generate the following top-level declaration
    `[@entry] let <pattern> : <rhs_type> = Dynamic_entrypoints_helpers.<std_lib_fun> <suffix>_<args_prefixes>[0] .. <suffix>_<args_prefixes>[N]`
  *)
  let let_rhs =
    let lamb = mod_access_to_stdlib ~loc std_lib_fun in
    let args =
      args_prefixes
      |> List.map ~f:(fun prefix -> prefix ^ suffix)
      |> List.map ~f:(Variable.of_input_var ~loc)
      |> List.map ~f:(e_variable ~loc)
    in
    e_application_lst ~loc lamb args
  in
  top_level_decl
    ~loc
    ~attr:"entry"
    (d_const ~loc { pattern; type_params = None; rhs_type; let_rhs })


and mod_access_to_stdlib ~loc fun_ =
  e_module_access
    ~loc
    Mod_access.
      { module_path =
          List.Ne.singleton (Mod_variable.of_input_var ~loc "Dynamic_entrypoints_helpers")
      ; field = Variable.of_input_var ~loc fun_
      ; field_as_open = false
      }


and top_level_decl ~loc ?attr d =
  let d =
    Option.value_map attr ~default:d ~f:(fun attr ->
        d_attr ~loc ({ key = attr; value = None }, d))
  in
  make_pe (PE_declaration d)


and get_decl_and_ty d =
  let open Simple_utils.Option in
  bind_eager_or
    (let* { pattern; rhs_type; _ } = get_d_const d in
     return (pattern, rhs_type))
    (let* { pattern; _ } = get_d_irrefutable_match d in
     return (pattern, None))
