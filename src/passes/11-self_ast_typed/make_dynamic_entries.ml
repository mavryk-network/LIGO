open Simple_utils.Trace
open Errors
open Ligo_prim

let default_dynmod = "Dynamic_entries"
let default_contract_var = Module_var.of_input_var ~loc:Location.generated default_dynmod

let get_dyn_entries_decl ~raise (mod_e : Ast_typed.module_expr) =
  match mod_e.module_content with
  | M_struct prg ->
    let f d =
      match Location.unwrap d with
      | Ast_typed.D_value { binder; attr; _ } when attr.dyn_entry ->
        Some (Binder.get_var binder, Binder.get_ascr binder)
      | Ast_typed.D_irrefutable_match { pattern; attr; _ } when attr.dyn_entry ->
        (match Location.unwrap pattern with
        | P_var binder -> Some (Binder.get_var binder, Binder.get_ascr binder)
        | _ -> None)
      | _ -> None
    in
    prg |> List.filter_map ~f
  | _ ->
    raise.error
      (corner_case
      @@ Format.asprintf
           "@[<hv>%aDynamic_entries module must be @]"
           (Snippet.pp ~no_colour:false)
           mod_e.module_location)


let get_dyn_entries ~raise (prg : Ast_typed.module_)
    : (Value_var.t * Ast_typed.Types.type_expression) list
  =
  let f d =
    match Location.unwrap d with
    | Ast_typed.D_module { module_binder; module_; _ }
      when Module_var.equal module_binder default_contract_var ->
      Some (d.location, module_)
    | _ -> None
  in
  match List.filter_map ~f prg with
  | [ (_, one) ] -> get_dyn_entries_decl ~raise one
  | [] -> []
  | (loc, _) :: _ ->
    raise.error
      (corner_case
      @@ Format.asprintf
           "@[<hv>%aMultiple definition of Dynamic_entries module@]"
           (Snippet.pp ~no_colour:false)
           loc)


(* let ps_func ~loc p s =
  Ast_typed.(
    t_arrow
      ~loc
      p
      (t_arrow ~loc s (t_pair ~loc (t_list ~loc (t_operation ~loc ())) s) ())
      ()) *)

(* let big_map_update =
  let loc = Location.generated in
  Module_access.
    { module_path = [ Module_var.of_input_var ~loc "Big_map" ]
    ; element = Value_var.of_input_var ~loc "update"
    }*)

let e_pack ~loc v ty =
  let p =
    Ast_typed.(
      make_e
        ~loc
        (E_raw_code
           { language = "michelson"
           ; code = e_a_string ~loc @@ Ligo_string.Standard "{ PACK }"
           })
        ty)
  in
  Ast_typed.e_a_applications ~loc p [ v ]


let generated_helpers
    : (Value_var.t * Ast_typed.Types.type_expression) list -> Ast_typed.declaration list
  =
 fun lst ->
  let open Ast_typed in
  let loc = Location.generated in
  let nat_big_map = t_big_map ~loc (t_nat ~loc ()) (t_bytes ~loc ()) in
  let t_big_map_update =
    let open Ast_typed in
    let loc = Location.generated in
    t_arrow
      ~loc
      (t_nat ~loc ())
      (t_arrow ~loc (t_option ~loc (t_bytes ~loc ())) nat_big_map ())
      ()
  in
  let t_pack ty = t_arrow ~loc ty (t_bytes ~loc ()) () in
  List.join
  @@ List.mapi lst ~f:(fun i (v, dyn_entry_ty) ->
         let e_key = e_a_nat ~loc (Z.of_int i) in
         let key =
           d_value
             ~loc
             { binder = Binder.make (Value_var.add_prefix "key_" v) (t_nat ~loc ())
             ; expr = e_key
             ; attr = ValueAttr.default_attributes
             }
         in
         let set =
           let v_in = Value_var.fresh ~loc () in
           let t_in = t_pair ~loc dyn_entry_ty nat_big_map in
           let e_in_n n =
             e_accessor
               ~loc
               Accessor.{ struct_ = e_variable ~loc v_in t_in; path = Label.of_string n }
               dyn_entry_ty
           in
           d_value
             ~loc
             { binder =
                 Binder.make
                   (Value_var.add_prefix "set_" v)
                   (t_arrow ~loc t_in nat_big_map ())
             ; expr =
                 e_a_lambda
                   ~loc
                   { binder = Param.make v_in t_in
                   ; output_type = nat_big_map
                   ; result =
                       make_e
                         ~loc
                         (e_map_update
                            e_key
                            (make_e
                               ~loc
                               (e_some (e_pack ~loc (e_in_n "0") (t_pack dyn_entry_ty)))
                               (t_option ~loc (t_bytes ~loc ())))
                            (e_in_n "1"))
                         t_big_map_update
                   }
                   t_in
                   nat_big_map
             ; attr = ValueAttr.default_attributes
             }
         in
         [ key; set ])


let program ~raise : Ast_typed.module_ -> Ast_typed.declaration list =
 fun module_ ->
  let extra_decls = generated_helpers (get_dyn_entries ~raise module_) in
  List.map module_ ~f:(function
      | Ast_typed.(
          { wrap_content =
              D_module
                ({ module_binder
                 ; module_ = { module_content = M_struct struct_; _ } as module_
                 ; _
                 } as mod_)
          ; _
          } as decl)
        when Module_var.equal module_binder default_contract_var ->
        let module_content = Module_expr.M_struct (struct_ @ extra_decls) in
        { decl with
          wrap_content =
            Ast_typed.D_module { mod_ with module_ = { module_ with module_content } }
        }
      | x -> x)


let make_dyn_module_expr ~raise (module_content : Ast_typed.module_content) =
  match module_content with
  | M_struct ds -> Module_expr.M_struct (program ~raise ds)
  | _ -> module_content


let make ~raise prg =
  let f d =
    match Location.unwrap d with
    | Ast_typed.D_module
        { module_binder
        ; module_attr
        ; module_ = { module_content; module_location; signature }
        ; annotation
        } ->
      let module_content = make_dyn_module_expr ~raise module_content in
      Location.wrap ~loc:(Location.get_location d)
      @@ Ast_typed.D_module
           { module_binder
           ; module_attr
           ; module_ = { module_content; module_location; signature }
           ; annotation
           }
    | _ -> d
  in
  let prg = Helpers.Declaration_mapper.map_module f prg in
  prg @ program ~raise prg
