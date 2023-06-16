open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils.Function
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let tv_parameter ~loc = Ty_variable.of_input_var ~loc "parameter"
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
    let entry_ts = fetch_static_entry_parameter_types p in
    if List.is_empty entry_ts
    then make_prg p
    else (
      let loc = Location.generated in
      make_prg
        (p @ [ make_parameter_type ~loc entry_ts; make_main_function ~loc entry_ts ]))
  in
  Fold { idle_fold with program }


and filter_decl : type b. program_entry list -> f:(declaration -> b option) -> b list =
 fun p ~f ->
  p
  |> List.rev
  |> List.map ~f:get_pe
  |> List.filter_map ~f:(function
         | PE_declaration d -> f d
         | _ -> None)


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


and var_to_label v =
  if Variable.is_generated v
  then assert false
  else Label.Label (String.capitalize (Variable.to_name_exn v))


and make_parameter_type ~loc : (Variable.t * ty_expr) list -> program_entry =
 fun entries ->
  let rows =
    List.mapi
      ~f:(fun i (v, ty) ->
        ( var_to_label v
        , Non_linear_rows.{ associated_type = Some ty; attributes = []; decl_pos = i } ))
      entries
  in
  let type_expr = t_sum_raw ~loc rows in
  top_level_decl ~loc (d_type ~loc { name = tv_parameter ~loc; type_expr })


and make_main_function ~loc : (Variable.t * ty_expr) list -> program_entry =
 fun entries ->
  let var_p, var_s =
    ("p", "s") |> Simple_utils.Pair.map ~f:(Variable.of_input_var ~loc)
  in
  let result =
    let cases =
      List.Ne.of_list
      @@ List.map entries ~f:(fun (v, _ty) ->
             let loc = Variable.get_location v in
             let args = List.map [ var_p; var_s ] ~f:(e_variable ~loc) in
             Case.
               { pattern = p_variant ~loc (var_to_label v) (Some (p_var ~loc var_p))
               ; rhs = e_application_lst ~loc (e_variable ~loc v) args
               })
    in
    e_match ~loc { expr = e_variable ~loc var_p; cases }
  in
  top_level_decl
    ~loc
    (d_const
       ~loc
       { pattern = p_var ~loc (Variable.of_input_var ~loc "main")
       ; type_params = None
       ; rhs_type = None
       ; let_rhs =
           e_lambda
             ~loc
             { binder = Ligo_prim.Param.make var_p None
             ; output_type = None
             ; result =
                 e_lambda
                   ~loc
                   { binder = Ligo_prim.Param.make var_s None
                   ; output_type = None
                   ; result
                   }
             }
       })


and top_level_decl ~loc d = make_pe (PE_declaration d)

and get_decl_and_ty d =
  let open Simple_utils.Option in
  bind_eager_or
    (let* { pattern; rhs_type; _ } = get_d_const d in
     return (pattern, rhs_type))
    (let* { pattern; expr; _ } = get_d_irrefutable_match d in
     return (pattern, None))
