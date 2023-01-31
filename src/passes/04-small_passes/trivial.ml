module O = Ast_core
module I = Ast_unified
module Location = Simple_utils.Location
open Simple_utils

let invariant () = failwith "impossible: have been reduced"
let ignored_attribute s = failwith ("ignored attribute "^s)

type statement = unit
type block = unit
type instruction = unit

let dummy_top_level () =
  (* directive are translated as let _ = () *)
  let loc = Location.generated in
  O.D_value
    { binder = Ligo_prim.(Binder.make (Value_var.fresh ~loc ()) None)
    ; expr = O.e_unit ~loc ()
    ; attr = O.ValueAttr.default_attributes
    }


type cata_pass =
  ( O.expression
  , O.type_expression
  , O.type_expression option O.Pattern.t
  , statement
  , block
  , O.module_expr
  , instruction
  , O.declaration
  , O.declaration )
  Ast_unified.Catamorphism.fold

let conv_vdecl_attr : O.ValueAttr.t -> I.Attribute.t -> O.ValueAttr.t =
 fun o_attr i_attr ->
  match i_attr with
  | { key = "inline"; value = None } -> { o_attr with inline = true }
  | { key = "no_mutation"; value = None } -> { o_attr with no_mutation = true }
  | { key = "view"; value = None } -> { o_attr with view = true }
  | { key = "private"; value = None } -> { o_attr with public = false }
  | { key = "hidden"; value = None } -> { o_attr with hidden = false }
  | { key = "thunk"; value = None } -> { o_attr with thunk = false }
  | _ -> ignored_attribute "vdecl"


let conv_exp_attr : O.ValueAttr.t -> I.Attribute.t -> O.ValueAttr.t =
 fun o_attr i_attr ->
  match i_attr with
  | { key = "inline"; value = None } -> { o_attr with inline = true }
  | { key = "no_mutation"; value = None } -> { o_attr with no_mutation = true }
  | { key = "thunk"; value = None } -> { o_attr with thunk = false }
  | _ -> ignored_attribute "exp"


let conv_modtydecl_attr : O.TypeOrModuleAttr.t -> I.Attribute.t -> O.TypeOrModuleAttr.t =
 fun o_attr i_attr ->
  match i_attr with
  | { key = "private"; value = None } -> { o_attr with public = false }
  | { key = "hidden"; value = None } -> { o_attr with hidden = false }
  | _ -> ignored_attribute "modty"

let conv_layout_attr : Ligo_prim.Layout.t option -> I.Attribute.t -> Ligo_prim.Layout.t option =
  fun o_attr i_attr ->
    match (o_attr , i_attr) with
    | Some _ , _ignored_attr -> ignored_attribute "layout"
    | None , { key = "layout"; value = Some "comb" } -> Some L_tree
    | None , { key = "layout"; value = Some "tree" } -> Some L_comb
    | _ -> ignored_attribute "layout"

let conv_row_attr : I.Attribute.t list -> string option = function
  | [] -> None
  | [ { key = "annot"; value = Some annot } ] -> Some annot
  | _ -> ignored_attribute "row"


let declaration
    :  ( O.declaration
       , O.expression
       , O.type_expression
       , O.type_expression option O.Pattern.t
       , O.module_expr )
       I.declaration_
    -> O.declaration
  =
 fun d ->
  let location = Location.get_location d in
  let ret wrap_content : O.declaration = { wrap_content; location } in
  match Location.unwrap d with
  | D_Attr (attr, O.{ wrap_content = D_value x; _ }) ->
    ret @@ D_value { x with attr = conv_vdecl_attr x.attr attr }
  | D_Attr (attr, O.{ wrap_content = D_irrefutable_match x; _ }) ->
    ret @@ D_irrefutable_match { x with attr = conv_vdecl_attr x.attr attr }
  | D_Attr (attr, O.{ wrap_content = D_type x; _ }) ->
    ret @@ D_type { x with type_attr = conv_modtydecl_attr x.type_attr attr }
  | D_Attr (attr, O.{ wrap_content = D_module x; _ }) ->
    ret @@ D_module { x with module_attr = conv_modtydecl_attr x.module_attr attr }
  | D_Const { type_params = None; pattern; rhs_type = _; let_rhs } ->
    ret
    @@ D_irrefutable_match
         { pattern; expr = let_rhs; attr = O.ValueAttr.default_attributes }
  | D_Directive _ -> ret (dummy_top_level ())
  | D_Module { name; mod_expr } ->
    ret
    @@ D_module
         { module_binder = name
         ; module_ = mod_expr
         ; module_attr = O.TypeOrModuleAttr.default_attributes
         }
  | D_Type { name; type_expr } ->
    ret
    @@ D_type
         { type_binder = name
         ; type_expr
         ; type_attr = O.TypeOrModuleAttr.default_attributes
         }
  | D_irrefutable_match { pattern; expr } ->
    ret @@ D_irrefutable_match { pattern; expr; attr = O.ValueAttr.default_attributes }
  | D_Let _ | D_Import _ | D_Export _ | D_Var _ | D_Multi_const _ | D_Multi_var _
  | D_Const { type_params = Some _; _ }
  | D_Fun _ | D_Type_abstraction _ -> invariant ()


let expr
    :  ( O.expression
       , O.type_expression
       , O.type_expression option O.Pattern.t
       , block
       , O.module_expr )
       I.expression_
    -> O.expression
  =
 fun e ->
  let location = Location.get_location e in
  let ret ?(location = location) expression_content : O.expression =
    O.{ expression_content; sugar = None; location }
  in
  match Location.unwrap e with
  | E_Attr (attr, { expression_content = E_let_in x; location; _ }) ->
    ret ~location @@ E_let_in { x with attributes = conv_exp_attr x.attributes attr }
  | E_Literal x -> ret @@ E_literal x
  | E_variable x -> ret @@ E_variable x
  | E_Record_pun fields ->
    let x =
      List.map
        ~f:(function
          | Complete (l, r) -> l, r
          | Punned _ -> invariant ())
        fields
    in
    ret @@ E_record (Ligo_prim.Record.of_list x)
  | E_ModA { module_path; field } ->
    let todo_restrict_pass =
      match O.get_e_variable field with
      | Some x -> x
      | None -> invariant ()
    in
    ret
    @@ E_module_accessor
         { module_path = List.Ne.to_list module_path; element = todo_restrict_pass }
  | E_Match { expr; cases } ->
    ret
    @@ E_matching
         { matchee = expr
         ; cases =
             List.map (List.Ne.to_list cases) ~f:(function I.Case.{ pattern; rhs } ->
                 O.Match_expr.{ pattern; body = rhs })
         }
  | E_Annot (anno_expr, type_annotation) ->
    ret @@ E_ascription { anno_expr; type_annotation }
  | E_TypeIn { type_binder; rhs; body } ->
    ret @@ E_type_in { type_binder; rhs; let_result = body }
  | E_ModIn { module_name; rhs; body } ->
    ret @@ E_mod_in { module_binder = module_name; rhs; let_result = body }
  | E_RawCode x -> ret @@ E_raw_code x
  | E_Let_mut_in
      { is_rec = false; type_params = None; lhs = lhs, []; rhs_type = _; rhs; body } ->
    (* TODO: remove once we have E_simple_let_mut_in *)
    ret
    @@ E_let_mut_in
         { let_binder = lhs
         ; rhs
         ; let_result = body
         ; attributes = O.ValueAttr.default_attributes
         }
  | E_Assign_unitary x -> ret @@ E_assign x
  | E_While { cond; block } -> ret @@ E_while { cond; body = block }
  | E_For { index; init; bound; step = Some incr; block } ->
    ret @@ E_for { binder = index; start = init; final = bound; incr; f_body = block }
  | E_For_in (ForMap { binding = k, v; collection; block }) ->
    ret
    @@ E_for_each
         { fe_binder = k, Some v; collection; collection_type = Map; fe_body = block }
  | E_For_in (ForSetOrList { var; for_kind; collection; block }) ->
    let collection_type : Ligo_prim.For_each_loop.collect_type =
      match for_kind with
      | `Set -> Set
      | `List -> List
    in
    ret
    @@ E_for_each { fe_binder = var, None; collection; collection_type; fe_body = block }
  | E_For_in (ForAny { pattern = { wrap_content = P_var var; _ }; collection; block }) ->
    ret
    @@ E_for_each
         { fe_binder = Ligo_prim.Binder.get_var var, None
         ; collection
         ; collection_type = Any
         ; fe_body = block
         }
  | E_constant x -> ret @@ E_constant x
  | E_Constructor x -> ret @@ E_constructor x
  | E_Simple_let_in { binder; rhs; let_result } ->
    ret
    @@ E_let_in
         { let_binder = binder
         ; rhs
         ; let_result
         ; attributes = O.ValueAttr.default_attributes
         }
  | E_Recursive { fun_name; fun_type; lambda } ->
    ret @@ E_recursive { fun_name; fun_type; lambda }
  | E_Lambda x -> ret @@ E_lambda x
  | E_Application x -> ret @@ E_application x
  | E_Type_abstraction { type_binder; result } ->
    ret @@ E_type_abstraction { type_binder; result }
  | E_record_update { struct_ ; label ; update} ->
    ret @@ E_update { struct_ ; path = label ; update }
  | E_record_access { struct_ ; label } ->
    ret @@ E_accessor { struct_ ; path = label }
  | E_Poly_fun _
  | E_Let_in _
  | E_Block_fun _
  | E_Binary_op _
  | E_Array _
  | E_Object _
  | E_Constr _
  | E_Ctor_App _
  | E_Call (_, _)
  | E_Attr (_, _)
  | E_Let_mut_in _
  | E_Unary_op _
  | E_Block_with _
  | E_Poly_recursive _
  | E_Assign_chainable _
  | E_Proj _
  | E_Update _
  | E_RevApp _ -> invariant ()
  | E_MapLookup _
  | E_Cond _
  | E_Map _
  | E_BigMap _
  | E_Sequence _
  | E_List _
  | E_Set _
  | E_For _
  | E_For_in _
  | E_Tuple _ -> failwith "TODO: pass"


let ty_expr : O.type_expression I.ty_expr_ -> O.type_expression =
 fun t ->
  let location = Location.get_location t in
  let ret type_content : O.type_expression = O.{ type_content; sugar = None; location } in
  match Location.unwrap t with
  | T_Attr (attr, O.{ type_content = T_sum x; _ }) ->
    ret @@ T_sum { x with layout = conv_layout_attr x.layout attr}
  | T_Attr (attr, O.{ type_content = T_record x; _ }) ->
    ret @@ T_record { x with layout = conv_layout_attr x.layout attr}
  | T_Var v -> ret @@ T_variable v
  | T_App { constr; type_args } ->
    (match constr with
    | { type_content = T_variable type_operator; _ } ->
      ret @@ T_app { type_operator; arguments = List.Ne.to_list type_args }
    | _ -> invariant ())
  | T_Fun (type1, type2) -> ret @@ T_arrow { type1; type2 }
  | T_String str -> ret @@ T_singleton (Literal_string (Ligo_string.standard str))
  | T_Int (_, x) -> ret @@ T_singleton (Literal_int x)
  | T_ModA { module_path; field = { type_content = T_variable element; _ } } ->
    ret @@ T_module_accessor { module_path = [ module_path ]; element }
  | T_Sum_raw lst ->
    let fields : O.row_element Ligo_prim.Record.t =
      Ligo_prim.Record.of_list
        (List.map
           lst
           ~f:(fun (l, I.Non_linear_rows.{ associated_type = t_opt; attributes; decl_pos })
              ->
             let associated_type =
               Option.value_or_thunk t_opt ~default:(fun () -> O.t_unit ~loc:location ())
             in
             let row_element =
               Ligo_prim.Rows.
                 { associated_type
                 ; michelson_annotation = conv_row_attr attributes
                 ; decl_pos
                 }
             in
             l, row_element))
    in
    ret @@ T_sum { fields; layout = None }
  | T_Record_raw lst ->
    let fields : O.row_element Ligo_prim.Record.t =
      Ligo_prim.Record.of_list
        (List.map
           lst
           ~f:(fun (l, I.Non_linear_rows.{ associated_type = t_opt; attributes; decl_pos })
              ->
             let associated_type =
               Option.value_or_thunk t_opt ~default:(fun () -> O.t_unit ~loc:location ())
             in
             let row_element =
               Ligo_prim.Rows.
                 { associated_type
                 ; michelson_annotation = conv_row_attr attributes
                 ; decl_pos
                 }
             in
             l, row_element))
    in
    ret @@ T_record { fields; layout = None }
  | T_Abstraction abs -> ret @@ T_abstraction abs
  | T_Disc_union _ 
  | T_Arg _ 
  | T_Prod _ 
  | T_Named_fun _ 
  | T_Attr _
  | T_ModA _ -> invariant ()


let pattern
    :  (O.type_expression option O.Pattern.t, O.type_expression) I.pattern_
    -> O.type_expression option O.Pattern.t
  =
 fun p ->
  let location = Location.get_location p in
  let ret wrap_content : O.type_expression option O.Pattern.t =
    { wrap_content; location }
  in
  match Location.unwrap p with
  | P_attr (_, p) ->
    (* should warn about ignored attribute ? *)
    p
  | P_unit -> ret @@ P_unit
  | P_typed (ty, { wrap_content = P_var x; _ }) ->
    ret @@ P_var (Ligo_prim.Binder.set_ascr x (Some ty))
  | P_var x -> ret @@ P_var (Ligo_prim.Binder.make x None)
  | P_list (List lst) -> ret @@ P_list (List lst)
  | P_list (Cons (l, r)) -> ret @@ P_list (Cons (l, r))
  | P_variant (l, Some p) -> ret @@ P_variant (l, p)
  | P_variant (l, None) ->
    ret @@ P_variant (l, Location.wrap ~loc:location O.Pattern.P_unit)
  | P_tuple lst -> ret @@ P_tuple lst
  | P_pun_record lst
    when List.for_all lst ~f:(function
             | Punned _ -> false
             | Complete _ -> true) ->
    let lst =
      List.map
        ~f:(function
          | Complete x -> x
          | Punned _ -> invariant ())
        lst
    in
    ret @@ P_record (Ligo_prim.Record.of_list lst)
  | P_mod_access _ | P_rest _ | P_pun_record _ | P_typed _ | P_literal _ -> invariant ()


let statement : _ I.statement_ -> statement = fun _ -> invariant ()
let block : _ I.block_ -> statement = fun _ -> invariant ()
let instruction : _ I.instruction_ -> instruction = fun _ -> invariant ()

let program : (O.declaration, O.declaration, unit) I.program_entry_ -> O.declaration =
  let ret location wrap_content : O.declaration = { wrap_content; location } in
  function
  | PE_Attr (attr, O.{ wrap_content = D_value x; location }) ->
    ret location @@ D_value { x with attr = conv_vdecl_attr x.attr attr }
  | PE_Attr (attr, O.{ wrap_content = D_irrefutable_match x; location }) ->
    ret location @@ D_irrefutable_match { x with attr = conv_vdecl_attr x.attr attr }
  | PE_Attr (attr, O.{ wrap_content = D_type x; location }) ->
    ret location @@ D_type { x with type_attr = conv_modtydecl_attr x.type_attr attr }
  | PE_Attr (attr, O.{ wrap_content = D_module x; location }) ->
    ret location
    @@ D_module { x with module_attr = conv_modtydecl_attr x.module_attr attr }
  | PE_Declaration d -> d
  | PE_Top_level_instruction _ -> invariant ()
  | PE_Preproc_directive _ -> Location.wrap ~loc:Location.generated (dummy_top_level ())


let mod_expr : (O.module_expr, O.declaration) I.mod_expr_ -> O.module_expr =
 fun m ->
  let location = Location.get_location m in
  let ret wrap_content : O.module_expr = { wrap_content; location } in
  match Location.unwrap m with
  | I.M_Body x -> ret @@ M_struct (List.Ne.to_list x)
  | I.M_Path x -> ret @@ M_module_path x
  | I.M_Var x -> ret @@ M_variable x


let conv_expr =
  I.Catamorphism.cata_expr
    ~f:
      { expr
      ; ty_expr
      ; pattern
      ; statement
      ; block
      ; mod_expr
      ; instruction
      ; declaration
      ; program
      }


let conv_program =
  I.Catamorphism.cata_program
    ~f:
      { expr
      ; ty_expr
      ; pattern
      ; statement
      ; block
      ; mod_expr
      ; instruction
      ; declaration
      ; program
      }