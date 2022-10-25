open Simple_utils.Trace
open Errors
open Ligo_prim

let default_entrypoint = "main"
let default_entrypoint_var = Value_var.of_input_var default_entrypoint

let get_entries_of_module (prg : Ast_typed.module_) =
  let f d =
    match Location.unwrap d with
    | Ast_typed.D_value { binder; attr; _ }
      when attr.entry && Option.is_some (Binder.get_ascr binder) ->
      Some (Binder.get_var binder, Option.value_exn @@ Binder.get_ascr binder)
    | _ -> None
  in
  prg |> List.filter_map ~f


let create_entrypoint_function_expr entrypoints parameter_type storage_type =
  let open Ast_typed in
  let p_var = Value_var.of_input_var "p" in
  let s_var = Value_var.of_input_var "s" in
  let p = e_a_variable p_var parameter_type in
  let s = e_a_variable s_var storage_type in
  let params = Value_var.fresh ~name:"param" () in
  let fields =
    Record.LMap.of_list
      [ ( Label "0"
        , Location.wrap @@ Pattern.(P_var (Binder.make p_var parameter_type)) )
      ; ( Label "1"
        , Location.wrap @@ Pattern.(P_var (Binder.make s_var storage_type)) )
      ]
  in
  let param_storage = e_a_pair p s in
  let fun_type = Misc.build_entry_type parameter_type storage_type in
  let oplst_storage = t_pair (t_list @@ t_operation ()) storage_type in
  let cases =
    List.map entrypoints ~f:(fun entrypoint ->
        let constructor = Label.of_string (Value_var.to_name_exn entrypoint) in
        let pattern = Value_var.fresh ~name:"pattern" () in
        let body =
          e_a_application
            (e_a_variable entrypoint fun_type)
            (e_a_pair (e_variable pattern (t_unit ())) s)
            oplst_storage
        in
        let pattern =
          Location.wrap
          @@ Pattern.(
               P_variant
                 ( constructor
                 , Location.wrap
                   @@ P_var (Binder.make pattern param_storage.type_expression)
                 ))
        in
        ({ pattern; body } : _ Match_expr.match_case))
  in
  let body = e_a_matching p cases oplst_storage in
  let pattern = Location.wrap @@ Pattern.(P_record fields) in
  let result =
    e_a_matching
      (e_a_variable params param_storage.type_expression)
      [ { pattern; body } ]
      oplst_storage
  in
  e_lambda
    { binder = Param.(make params param_storage.type_expression)
    ; result
    ; output_type = oplst_storage
    }
    fun_type


let program ~raise
    :  Ast_typed.module_
    -> (Ast_typed.declaration * Ast_typed.declaration) option
  =
 fun module_ ->
  let open Simple_utils.Option in
  let* entries =
    Simple_utils.List.Ne.of_list_opt @@ get_entries_of_module module_
  in
  let parameter_type, storage_type =
    match Ast_typed.Misc.parameter_from_entrypoints entries with
    | Error (`Not_entry_point_form ep_type) ->
      raise.error
        (corner_case
        @@ Format.asprintf
             "Not an entrypoint form: %a"
             Ast_typed.PP.type_expression
             ep_type)
    | Error (`Storage_does_not_match (ep_1, storage_1, ep_2, storage_2)) ->
      raise.error
        (corner_case
        @@ Format.asprintf
             "@[<hv>Storage types do not match for different entrypoints:@.%a : %a@.%a : %a@]"
             Value_var.pp ep_1
             Ast_typed.PP.type_expression storage_1
             Value_var.pp ep_2
             Ast_typed.PP.type_expression storage_2)
    | Ok (p, s) -> p, s
  in
  let type_binder = Type_var.fresh ~name:"parameter" () in
  let entrypoint_type_decl =
    Location.wrap
    @@ Ast_typed.D_type
         { type_binder
         ; type_expr = parameter_type
         ; type_attr = { public = true; hidden = false }
         }
  in
  let entrypoint_function_decl =
    let expr =
      create_entrypoint_function_expr
        Simple_utils.List.Ne.(to_list @@ map fst entries)
        parameter_type
        storage_type
    in
    let binder =
      Binder.make default_entrypoint_var (Some expr.type_expression)
    in
    Location.wrap
    @@ Ast_typed.D_value
         { binder
         ; expr
         ; attr =
             { inline = false
             ; no_mutation = false
             ; entry = false
             ; view = false
             ; public = true
             ; thunk = false
             ; hidden = false
             }
         }
  in
  return (entrypoint_type_decl, entrypoint_function_decl)


let make_main_module_expr ~raise (module_ : Ast_typed.module_expr) =
  match Location.unwrap module_ with
  | M_struct ds ->
    (match program ~raise ds with
    | None -> module_
    | Some (type_decl, main_decl) ->
      Location.wrap ~loc:(Location.get_location module_)
      @@ Module_expr.M_struct (ds @ [ type_decl; main_decl ]))
  | _ -> module_


let make_main_module ~raise (program : Ast_typed.program) =
  let f d =
    match Location.unwrap d with
    | Ast_typed.D_module { module_binder; module_attr; module_ } ->
      let module_ = make_main_module_expr ~raise module_ in
      Location.wrap ~loc:(Location.get_location d)
      @@ Ast_typed.D_module { module_binder; module_attr; module_ }
    | _ -> d
  in
  Helpers.Declaration_mapper.map_module f program


let make_main_entrypoint ~raise
    :  Ast_typed.expression_variable Simple_utils.List.Ne.t -> Ast_typed.program
    -> Ast_typed.expression_variable * Ast_typed.program
  =
 fun entrypoints prg ->
  let prg = make_main_module ~raise prg in
  match entrypoints with
  | entrypoint, [] -> entrypoint, prg
  | entrypoint, rest ->
    let entries =
      let f ep =
        ( ep
        , fst @@ Helpers.fetch_entry_type ~raise (Value_var.to_name_exn ep) prg
        )
      in
      Simple_utils.List.Ne.map f (entrypoint, rest)
    in
    let parameter_type, storage_type =
      match Ast_typed.Misc.parameter_from_entrypoints entries with
      | Error (`Not_entry_point_form ep_type) ->
        raise.error
          (corner_case
          @@ Format.asprintf
               "Not an entrypoint form: %a"
               Ast_typed.PP.type_expression
               ep_type)
      | Error (`Storage_does_not_match (ep_1, storage_1, ep_2, storage_2)) ->
        raise.error
          (corner_case
           @@ Format.asprintf
             "@[<hv>Storage types do not match for different entrypoints:@.%a : %a@.%a : %a@]"
             Value_var.pp ep_1
             Ast_typed.PP.type_expression storage_1
             Value_var.pp ep_2
             Ast_typed.PP.type_expression storage_2)
      | Ok (p, s) -> p, s
    in
    let type_binder = Type_var.fresh ~name:"parameter" () in
    let entrypoint_type_decl =
      Location.wrap
      @@ Ast_typed.D_type
           { type_binder
           ; type_expr = parameter_type
           ; type_attr = { public = true; hidden = false }
           }
    in
    let entrypoint_function_decl =
      let expr =
        create_entrypoint_function_expr
          (entrypoint :: rest)
          parameter_type
          storage_type
      in
      let binder =
        Binder.make default_entrypoint_var (Some expr.type_expression)
      in
      Location.wrap
      @@ Ast_typed.D_value
           { binder
           ; expr
           ; attr =
               { inline = false
               ; no_mutation = false
               ; entry = false
               ; view = false
               ; public = true
               ; thunk = false
               ; hidden = false
               }
           }
    in
    let prg = prg @ [ entrypoint_type_decl; entrypoint_function_decl ] in
    default_entrypoint_var, prg


let program ~raise
    :  Ast_typed.expression_variable list -> Ast_typed.program
    -> Ast_typed.expression_variable * Ast_typed.program
  =
 fun entry_points prg ->
  let annoted_entry_points = get_entries_of_module prg |> List.map ~f:fst in
  match entry_points, annoted_entry_points with
  (* First make the entrypoint from the provided list *)
  | hd :: tl, _ ->
    let () =
      List.iter
        ~f:(fun var ->
          if Option.is_none
               (List.find ~f:(fun s -> Value_var.equal var s) entry_points)
          then raise.warning (`Main_entry_ignored (Value_var.get_location var)))
        annoted_entry_points
    in
    make_main_entrypoint ~raise (hd, tl) prg
  (* Second from annotations *)
  | [], hd :: tl -> make_main_entrypoint ~raise (hd, tl) prg
  | [], [] -> default_entrypoint_var, prg


let get_final_entrypoint_name ~raise
    :  Ast_typed.expression_variable list -> Ast_typed.program
    -> Ast_typed.expression_variable
  =
 fun entry_points prg ->
  let annoted_entry_points = get_entries_of_module prg |> List.map ~f:fst in
  match entry_points, annoted_entry_points with
  | [ hd ], lst ->
    let () =
      List.iter
        ~f:(fun var ->
          if not (Value_var.equal var hd)
          then raise.warning (`Main_entry_ignored (Value_var.get_location var)))
        lst
    in
    hd
  | [], [ hd ] -> hd
  | [], _ -> default_entrypoint_var
  | lst, _ ->
    let () =
      List.iter
        ~f:(fun var ->
          if Option.is_none (List.find ~f:(fun s -> Value_var.equal var s) lst)
          then raise.warning (`Main_entry_ignored (Value_var.get_location var)))
        annoted_entry_points
    in
    default_entrypoint_var
