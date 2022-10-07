module Ligo_string = Simple_utils.Ligo_string
module Location = Simple_utils.Location
module I = Ast_core
module O = Ast_typed
open Ligo_prim

let untype_value_attr : O.Value_attr.t -> I.Value_attr.t =
 fun { inline; no_mutation; view; public; hidden; thunk } ->
  { inline; no_mutation; view; public; hidden; thunk }


let rec untype_type_expression (t : O.type_expression) : I.type_expression =
  let self = untype_type_expression in
  let return t = I.make_t t in
  match t.type_content with
  | O.T_sum row ->
    let row = untype_row row in
    return @@ I.T_sum row
  | O.T_record row ->
    let row = untype_row row in
    return @@ I.T_record row
  | O.T_variable name -> return @@ I.T_variable name
  | O.T_arrow arr ->
    let arr = Arrow.map self arr in
    return @@ I.T_arrow arr
  | O.T_constant { language = _; injection; parameters } ->
    let arguments = List.map ~f:self parameters in
    let type_operator =
      Type_var.fresh ~name:(Literal_types.to_string injection) ()
    in
    return @@ I.T_app { type_operator; arguments }
  | O.T_singleton l -> return @@ I.T_singleton l
  | O.T_abstraction x ->
    let x = Abstraction.map self x in
    return @@ T_abstraction x
  | O.T_for_all x ->
    let x = Abstraction.map self x in
    return @@ T_for_all x
  | O.T_tuple tup ->
    let tup = Tuple.map self tup in
    return @@ T_tuple tup


and untype_row (row : _ O.Rows.t) : _ I.Rows.t =
  let fields =
    Map.map
      row.fields
      ~f:(fun
           ({ content = { associated_type; decl_pos }; michelson_annotation } :
             _ O.Rows.Elem.t)
         ->
        let associated_type = untype_type_expression associated_type in
        ({ content = { associated_type; decl_pos }; michelson_annotation }
          : _ I.Rows.Elem.t))
  in
  { fields; attributes = { layout = Some row.attributes.layout } }


let untype_type_expression_option x = Option.return @@ untype_type_expression x

let rec untype_expression (e : O.expression) : I.expression =
  untype_expression_content e.expression_content


and untype_expression_content (ec : O.expression_content) : I.expression =
  let open I in
  let self = untype_expression in
  let self_type = untype_type_expression in
  let self_type_opt = untype_type_expression_option in
  let return content = I.make_e content in
  match ec with
  | E_literal l -> return @@ E_literal l
  | E_constant const ->
    let const = Constant.map self const in
    return @@ E_constant const
  | E_variable var -> return @@ E_variable var
  | E_application app ->
    let app = Application.map self app in
    return @@ E_application app
  | E_lambda lambda ->
    let lambda = Lambda.map self self_type_opt lambda in
    return @@ E_lambda lambda
  | E_type_abstraction abs ->
    let abs = Type_abs.map self abs in
    return @@ E_type_abstraction abs
  | E_constructor constr ->
    let constr = Constructor.map self constr in
    return @@ E_constructor constr
  | E_record record ->
    let record = Record.map self record in
    return @@ E_record record
  | E_accessor { struct_; path } ->
    let struct_ = self struct_ in
    return @@ E_accessor { struct_; path }
  | E_update { struct_; path; update } ->
    let struct_ = self struct_ in
    let update = self update in
    return @@ E_update { struct_; path; update }
  | E_matching { matchee; cases } ->
    let matchee = self matchee in
    (match cases with
    | Match_variant { cases; tv } ->
      (*
        If one day this code is actually executed, and if the list type is still not a tuple type.
        A special case for lists might be required here
      *)
      let aux : _ Ast_typed.matching_content_case -> _ Match_expr.match_case =
       fun { constructor; pattern; body } ->
        let pattern =
          match tv with
          | _ ->
            let proj =
              Location.wrap @@ Pattern.P_var (Binder.make pattern None)
            in
            Location.wrap @@ Pattern.P_variant (constructor, proj)
        in
        let body = self body in
        ({ pattern; body }
          : ( Ast_core.expression
            , Ast_core.type_expression option )
            Match_expr.match_case)
      in
      let cases = List.map ~f:aux cases in
      return @@ E_matching { matchee; cases }
    | Match_record { fields; body; tv = _ } ->
      let aux
          :  Label.t * Ast_typed.type_expression Binder.t
          -> Label.t * Ast_core.type_expression option Pattern.t
        =
       fun (Label label, binder) ->
        let proj =
          Location.wrap
          @@ Pattern.P_var
               (Binder.map (Fn.compose Option.return self_type) binder)
        in
        Label label, proj
      in
      let labels, patterns =
        List.unzip @@ List.map ~f:aux (Map.to_alist fields)
      in
      let body = self body in
      let case =
        let pattern = Location.wrap (Pattern.P_record (labels, patterns)) in
        ({ pattern; body } : _ Match_expr.match_case)
      in
      return @@ E_matching { matchee; cases = [ case ] }
    | Match_tuple { binders; body; tv = _ } ->
      let body = self body in
      let patterns =
        List.map binders ~f:(fun binder ->
            Location.wrap
            @@ Pattern.P_var
                 (Binder.map (Fn.compose Option.some self_type) binder))
      in
      let case =
        let pattern = Location.wrap (Pattern.P_tuple patterns) in
        ({ pattern; body } : _ Match_expr.match_case)
      in
      return @@ E_matching { matchee; cases = [ case ] })
  | E_let_in let_in ->
    let rhs_type = self_type let_in.rhs.type_expression in
    let O.Let_in.{ let_binder; rhs; let_result; attributes } =
      O.Let_in.map self self_type let_in
    in
    let let_binder = Binder.set_ascr let_binder (Some rhs_type) in
    let attributes : I.Value_attr.t = untype_value_attr attributes in
    return @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_mod_in mod_in ->
    let mod_in = Mod_in.map self untype_module_expr mod_in in
    return @@ E_mod_in mod_in
  | E_raw_code raw_code ->
    let raw_code = Raw_code.map self raw_code in
    return @@ E_raw_code raw_code
  | E_recursive recursive ->
    let recursive = Recursive.map self self_type recursive in
    return @@ E_recursive recursive
  | E_module_accessor ma -> return @@ E_module_accessor ma
  | E_let_mut_in let_in ->
    let rhs_type = self_type let_in.rhs.type_expression in
    let O.Let_in.{ let_binder; rhs; let_result; attributes } =
      O.Let_in.map self self_type let_in
    in
    let let_binder = Binder.set_ascr let_binder (Some rhs_type) in
    let attributes : I.Value_attr.t = untype_value_attr attributes in
    return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_assign a ->
    let assign = Assign.map self self_type_opt a in
    return @@ E_assign assign
  | E_for for_loop ->
    let for_loop = For_loop.map self for_loop in
    return @@ E_for for_loop
  | E_for_each for_each_loop ->
    let for_each_loop = For_each_loop.map self for_each_loop in
    return @@ E_for_each for_each_loop
  | E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ E_while while_loop
  | E_deref var -> return @@ E_variable var
  | E_type_inst { forall; type_ = type_inst } ->
    (match forall.type_expression.type_content with
    | T_for_all { ty_binder; type_; kind = _ } ->
      let type_ = Ast_typed.Helpers.subst_type ty_binder type_inst type_ in
      let forall = { forall with type_expression = type_ } in
      self forall
    | T_arrow _ ->
      (* This case is used for external typers *)
      self forall
    | _ ->
      failwith
        "Impossible case: cannot untype a type instance of a non polymorphic \
         type")
  | E_skip -> return E_skip
  | E_cond cond ->
    let cond = Conditional.map self cond in
    return @@ E_cond cond
  | E_sequence seq ->
    let seq = Sequence.map self seq in
    return @@ E_sequence seq
  | E_tuple tup ->
    let tup = Tuple.map self tup in
    return @@ E_tuple tup
  | E_set set_expr ->
    let set_expr = Set_expr.map self set_expr in
    return @@ E_set set_expr
  | E_list list_expr ->
    let list_expr = List_expr.map self list_expr in
    return @@ E_list list_expr
  | E_map map_expr ->
    let map_expr = Map_expr.map self map_expr in
    return @@ E_map map_expr
  | E_big_map map_expr ->
    let map_expr = Map_expr.map self map_expr in
    return @@ E_big_map map_expr


and untype_module_expr : O.module_expr -> I.module_expr =
 fun module_expr ->
  let return wrap_content : I.module_expr = { module_expr with wrap_content } in
  match module_expr.wrap_content with
  | M_struct prg ->
    let prg = untype_module prg in
    return (M_struct prg)
  | M_module_path path -> return (M_module_path path)
  | M_variable v -> return (M_variable v)


and untype_declaration_constant
    : (O.expression -> I.expression) -> _ O.Value_decl.t -> _ I.Value_decl.t
  =
 fun untype_expression { binder; expr; attr } ->
  let ty = untype_type_expression expr.O.type_expression in
  let binder = Binder.map (Fn.const @@ Some ty) binder in
  let expr = untype_expression expr in
  let expr = I.e_ascription expr ty in
  let attr = untype_value_attr attr in
  { binder; attr; expr }


and untype_declaration_type : _ O.Type_decl.t -> _ I.Type_decl.t =
 fun { type_binder; type_expr; type_attr = { public; hidden } } ->
  let type_expr = untype_type_expression type_expr in
  let type_attr = ({ public; hidden } : I.Type_or_module_attr.t) in
  { type_binder; type_expr; type_attr }


and untype_declaration_module : _ O.Module_decl.t -> _ I.Module_decl.t =
 fun { module_binder; module_; module_attr = { public; hidden } } ->
  let module_ = untype_module_expr module_ in
  let module_attr = ({ public; hidden } : I.Type_or_module_attr.t) in
  { module_binder; module_; module_attr }


and untype_declaration =
  let return (d : I.declaration_content) = d in
  fun (d : O.declaration_content) ->
    match d with
    | D_value dc ->
      let dc = untype_declaration_constant untype_expression dc in
      return @@ D_value dc
    | D_type dt ->
      let dt = untype_declaration_type dt in
      return @@ D_type dt
    | D_module dm ->
      let dm = untype_declaration_module dm in
      return @@ D_module dm


and untype_decl : O.decl -> I.decl = fun d -> Location.map untype_declaration d
and untype_module : O.module_ -> I.module_ = fun p -> List.map ~f:untype_decl p

and untype_program : O.program -> I.program =
 fun p -> List.map ~f:(Location.map untype_declaration) p
