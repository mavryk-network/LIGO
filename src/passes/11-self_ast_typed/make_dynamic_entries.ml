open Simple_utils.Trace
open Errors
open Ligo_prim

let default_dynmod = "Dynamic_entries"
let default_contract_var = Module_var.of_input_var ~loc:Location.generated default_dynmod

let e_a_pack ~loc v ty =
  let open Ast_typed in
  let ins = make_e ~loc (e_string @@ Ligo_string.Verbatim "{ PACK }") ty in
  make_e
    ~loc
    (E_raw_code { language = "michelson"; code = e_a_applications ~loc ins [ v ] })
    ty


let e_a_unpack ~loc v ty =
  let open Ast_typed in
  let ins = make_e ~loc (e_string @@ Ligo_string.Verbatim "{ UNPACK (typeopt $0) }") ty in
  make_e
    ~loc
    (E_raw_code { language = "michelson"; code = e_a_applications ~loc ins [ v ] })
    ty


let e_a_map_update ~loc k vopt m ty_op =
  let open Ast_typed in
  make_e ~loc (e_map_update k vopt m) ty_op


let e_a_map_find_opt ~loc k m ty_op = Ast_typed.(make_e ~loc (e_map_find_opt k m) ty_op)

let get_dyn_entries (prg : Ast_typed.signature)
    : (Value_var.t * Ast_typed.Types.type_expression) list
  =
  let f = function
    | Ast_typed.S_value (v, ty, attr) when attr.dyn_entry -> Some (v, ty)
    | _ -> None
  in
  List.filter_map ~f prg


let generated_helpers
    : (Value_var.t * Ast_typed.Types.type_expression) list -> Ast_typed.declaration list
  =
 fun lst ->
  let open Ast_typed in
  let loc = Location.generated in
  let nat_big_map = t_big_map ~loc (t_nat ~loc ()) (t_bytes ~loc ()) in
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
               Accessor.{ struct_ = e_variable ~loc v_in t_in; path = Label.of_int n }
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
                       e_a_map_update
                         ~loc
                         e_key
                         (make_e
                            ~loc
                            (e_some (e_a_pack ~loc (e_in_n 0) (t_pack dyn_entry_ty)))
                            (t_option ~loc (t_bytes ~loc ())))
                         (e_in_n 1)
                         nat_big_map
                   }
                   t_in
                   nat_big_map
             ; attr = ValueAttr.default_attributes
             }
         in
         let get =
           let v_in = Value_var.fresh ~loc () in
           let t_in = nat_big_map in
           let t_out = t_option ~loc dyn_entry_ty in
           let proj = Value_var.fresh ~loc () in
           let t_proj = t_bytes ~loc () in
           d_value
             ~loc
             { binder =
                 Binder.make (Value_var.add_prefix "get_" v) (t_arrow ~loc t_in t_out ())
             ; expr =
                 e_a_lambda
                   ~loc
                   { binder = Param.make v_in t_in
                   ; output_type = t_out
                   ; result =
                       e_a_matching
                         ~loc
                         (e_a_map_find_opt
                            ~loc
                            e_key
                            (e_variable ~loc v_in t_in)
                            (t_option ~loc (t_bytes ~loc ())))
                         [ { pattern =
                               Pattern.variant_pattern
                                 ~loc
                                 (Label.of_string "None", Pattern.unit ~loc)
                           ; body = make_e ~loc (e_none ()) t_out
                           }
                         ; { pattern =
                               Pattern.variant_pattern
                                 ~loc
                                 ( Label.of_string "Some"
                                 , Pattern.var ~loc (Binder.make proj t_proj) )
                           ; body =
                               e_a_unpack ~loc (e_variable ~loc proj t_proj) dyn_entry_ty
                           }
                         ]
                         t_out
                   }
                   t_in
                   t_out
             ; attr = ValueAttr.default_attributes
             }
         in
         [ key; set; get ])


let make ~raise prg =
  let f d =
    match Location.unwrap d with
    | Ast_typed.D_module
        { module_binder
        ; module_attr
        ; module_ = { module_content = M_struct struct_; module_location; signature }
        ; annotation
        }
      when Module_var.equal module_binder default_contract_var ->
      let dyn_entries = get_dyn_entries signature in
      let extra = generated_helpers dyn_entries in
      let module_content = Module_expr.M_struct (struct_ @ extra) in
      Location.wrap ~loc:(Location.get_location d)
      @@ Ast_typed.D_module
           { module_binder
           ; module_attr
           ; module_ = { module_content; module_location; signature }
           ; annotation
           }
    | _ -> d
  in
  Helpers.Declaration_mapper.map_module f prg
