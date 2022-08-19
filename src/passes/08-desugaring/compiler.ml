module I = Ast_sugar
module O = Ast_core
module C = Stage_common.Types

module Location = Simple_utils.Location
module Var      = Simple_utils.Var
module Pair     = Simple_utils.Pair

open Stage_common.Maps

let is_michelson_annotation = String.chop_prefix ~prefix:"annot:"

let is_layout = String.chop_prefix ~prefix:"layout:"

let get_michelson_annotation : (string list) -> string option = fun attributes ->
  let rec aux lst = match lst with
    | hd::tl -> ( match is_michelson_annotation hd with
      | Some ann -> Some ann
      | None -> aux tl
    )
    | [] -> None
  in
  aux attributes

let get_layout : (string list) -> O.layout option = fun attributes ->
  let rec aux lst = match lst with
    | hd::tl -> ( match is_layout hd with
      | Some "tree" -> Some O.L_tree
      | Some "comb" -> Some O.L_comb
      (*deal with wrong layout*)
      | None | Some _ -> aux tl
    )
    | [] -> None
  in
  aux attributes

let compile_exp_attributes : I.attributes -> O.known_attributes = fun attributes ->
  let is_inline attr = String.equal "inline" attr in
  let is_no_mutation attr = String.equal "no_mutation" attr in
  let is_view attr = String.equal "view" attr in
  let is_hidden attr = String.equal "hidden" attr in
  let is_ast_inline attr = String.equal "ast_inline" attr in
  let get_inline : (string list) -> bool = List.exists ~f:is_inline in
  let get_no_mutation : (string list) -> bool = List.exists ~f:is_no_mutation in
  let get_view : (string list) -> bool = List.exists ~f:is_view in
  let get_hidden : (string list) -> bool = List.exists ~f:is_hidden in
  let get_public : (string list) -> bool = fun attr -> not (List.mem attr "private" ~equal:String.equal) in
  let get_ast_inline : (string list) -> bool = List.exists ~f:is_ast_inline in
  let inline = get_inline attributes in
  let no_mutation = get_no_mutation attributes in
  let public = get_public attributes in
  let view = get_view attributes in
  let hidden = get_hidden attributes in
  let ast_inline = get_ast_inline attributes in
  O.{inline; no_mutation; view; public; hidden; ast_inline}

let compile_type_attributes : I.attributes -> O.type_attribute = fun attributes ->
  let get_public : (string list) -> bool = fun attr -> not (List.mem attr "private" ~equal:String.equal) in
  let get_hidden : (string list) -> bool = fun attr -> List.mem attr "hidden" ~equal:String.equal in
  let public = get_public attributes in
  let hidden = get_hidden attributes in
  O.{public;hidden}
let compile_module_attributes : I.attributes -> O.module_attribute = compile_type_attributes

let rec compile_type_expression : I.type_expression -> O.type_expression =
  fun te ->
  let self = compile_type_expression in
  let return tc = O.make_t ~loc:te.location ~sugar:te tc in
  match te.type_content with
    | I.T_variable type_variable -> return @@ T_variable type_variable
    | I.T_app a ->
      let a' = type_app compile_type_expression a in
      return @@ T_app a'
    | I.T_sum {fields ; attributes} ->
      let fields =
        O.LMap.map (fun v ->
          let {associated_type ; attributes ; decl_pos} : _ C.row_element = v in
          let michelson_annotation = get_michelson_annotation attributes in
          let associated_type = compile_type_expression associated_type in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          v'
        ) fields
      in
      let layout = get_layout attributes in
      return @@ O.T_sum {fields ; layout }
    | I.T_record {fields ; attributes} ->
      let fields =
        O.LMap.map (fun v ->
          let {associated_type ; attributes ; decl_pos} : _ C.row_element = v in
          let associated_type = compile_type_expression associated_type in
          let michelson_annotation = get_michelson_annotation attributes in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          v'
        ) fields
      in
      let layout = get_layout attributes in
      return @@ O.T_record {fields ; layout }
    | I.T_tuple tuple ->
      let aux (i,acc) el =
        let el = self el in
        (i+1,(O.Label (string_of_int i), ({associated_type=el;michelson_annotation=None;decl_pos=i}:_ O.row_element_mini_c))::acc) in
      let (_, lst ) = List.fold ~f:aux ~init:(0,[]) tuple in
      let record = O.LMap.of_list lst in
      return @@ O.T_record {fields = record ; layout = None}
    | I.T_arrow arr ->
      let arr = arrow self arr in
      return @@ T_arrow arr
    | I.T_module_accessor ma -> return @@ O.T_module_accessor ma
    | I.T_singleton x ->
      return @@ O.T_singleton x
    | I.T_abstraction x ->
      let type_ = self x.type_ in
      return @@ O.T_abstraction { x with type_ }
    | I.T_for_all x ->
      let type_ = self x.type_ in
      return @@ O.T_for_all { x with type_ }

let compile_binder = binder compile_type_expression

let rec compile_expression : I.expression -> O.expression =
  fun sugar ->
  let self = compile_expression in
  let self_type = compile_type_expression in
  let return expr = O.make_e ~loc:sugar.location ~sugar expr in
  match sugar.expression_content with
    | I.E_literal literal -> return @@ O.E_literal literal
    | I.E_constant cons ->
      let cons = constant self cons in
      return @@ O.E_constant cons
    | I.E_variable name -> return @@ O.E_variable name
    | I.E_application app ->
      let app = application self app in
      return @@ O.E_application app
    | I.E_lambda lamb ->
      let lamb = lambda self self_type lamb in
      return @@ O.E_lambda lamb
    | I.E_type_abstraction ty_abs ->
      let ty_abs = type_abs self ty_abs in
      return @@ O.E_type_abstraction ty_abs
    | I.E_recursive recs ->
      let recs = recursive self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;attributes;rhs;let_result;mut=_} ->
      let let_binder = binder self_type let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      let attr = compile_exp_attributes attributes in
      return @@ O.E_let_in {let_binder;attr;rhs;let_result}
    | I.E_type_in {type_binder; rhs; let_result} ->
      let rhs = self_type rhs in
      let let_result = self let_result in
      return @@ O.E_type_in {type_binder; rhs; let_result}
    | I.E_mod_in mi ->
      let mi = (mod_in self self_type compile_exp_attributes compile_type_attributes compile_module_attributes) mi in
      return @@ O.E_mod_in mi
    | I.E_raw_code rc ->
      let rc = raw_code self rc in
      return @@ O.E_raw_code rc
    | I.E_constructor const ->
      let const = constructor self const in
      return @@ O.E_constructor const
    | I.E_matching {matchee; cases} ->
      let matchee = compile_expression matchee in
      let cases =
        List.map
          ~f:(fun ({pattern ; body} : (I.expression, I.type_expression) I.match_case) ->
            let pattern = Stage_common.Helpers.map_pattern_t (binder compile_type_expression) pattern in
            let body = compile_expression body in
            ({pattern ; body} : (O.expression, O.type_expression) I.match_case)
          )
          cases
      in
      return @@ O.E_matching {matchee ; cases}
    | I.E_record recd ->
      let recd = record self recd in
      return @@ O.E_record recd
    | I.E_accessor {record;path} ->
      let record = self record in
      let accessor ~loc expr a =
        match a with
          I.Access_tuple  i -> O.e_record_accessor ~loc expr (Label (Z.to_string i))
        | I.Access_record a -> O.e_record_accessor ~loc expr (Label a)
        | I.Access_map k ->
          let k = self k in
          O.e_constant ~loc C_MAP_FIND_OPT [k;expr]
      in
      List.fold ~f:(accessor ~loc:sugar.location) ~init:record path
    | I.E_update {record;path;update} ->
      let record = self record in
      let update = self update in
      let accessor ~loc expr a =
        match a with
          I.Access_tuple  i -> O.e_record_accessor ~loc expr (Label (Z.to_string i))
        | I.Access_record a -> O.e_record_accessor ~loc expr (Label a)
        | I.Access_map k ->
          let k = self k in
          O.e_constant ~loc C_MAP_FIND_OPT [k;expr]
      in
      let updator ~loc (s:O.expression) a expr =
        match a with
          I.Access_tuple  i -> O.e_record_update ~loc s (Label (Z.to_string i)) expr
        | I.Access_record a -> O.e_record_update ~loc s (Label a) expr
        | I.Access_map k ->
          let k = self k in
          O.e_constant ~loc C_MAP_ADD [k;expr;s]
      in
      let aux (s, e : O.expression * _) lst =
        let s' = accessor ~loc:s.location s lst in
        let e' = fun expr ->
          let u = updator ~loc:s.location s lst (expr) in
          e u
        in
        (s',e')
      in
      let (_,rhs) = List.fold ~f:aux ~init:(record, fun e -> e) path in
      rhs @@ update
    | I.E_map map -> (
      let map = List.dedup_and_sort ~compare:Caml.compare map in
      let aux = fun (k, v) prev ->
        let (k', v') = Pair.map ~f:(self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let init = return @@ E_constant {cons_name=C_MAP_EMPTY;arguments=[]} in
      List.fold_right ~f:aux ~init map
    )
    | I.E_big_map big_map -> (
      let big_map = List.dedup_and_sort ~compare:Caml.compare big_map in
      let aux = fun (k, v) prev ->
        let (k', v') = Pair.map ~f:(self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let init = return @@ E_constant {cons_name=C_BIG_MAP_EMPTY;arguments=[]} in
      List.fold_right ~f:aux ~init big_map
    )
    | I.E_list lst ->
      let lst' = List.map ~f:(self) lst in
      let aux = fun cur prev ->
        return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
      let init  = return @@ E_constant {cons_name=C_LIST_EMPTY;arguments=[]} in
      List.fold_right ~f:aux ~init lst'
    | I.E_set set -> (
      let lst' = List.map ~f:(self) set in
      let lst' = List.dedup_and_sort ~compare:Caml.compare lst' in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
      let init = return @@ E_constant {cons_name=C_SET_EMPTY;arguments=[]} in
      List.fold ~f:aux ~init:init lst'
      )
    | I.E_ascription {anno_expr; type_annotation} ->
      let anno_expr = self anno_expr in
      let type_annotation = self_type type_annotation in
      return @@ O.E_ascription {anno_expr; type_annotation}
    | I.E_module_accessor ma -> return @@ O.E_module_accessor ma
    | I.E_cond {condition; then_clause; else_clause} ->
      let matchee = self condition in
      let match_true = self then_clause in
      let match_false = self else_clause in
      return @@ O.E_matching {
          matchee ;
          cases = [
            { pattern = Location.wrap @@ O.P_variant (Label "True" , Location.wrap O.P_unit) ; body = match_true  } ;
            { pattern = Location.wrap @@ O.P_variant (Label "False", Location.wrap O.P_unit) ; body = match_false } ;
          ]
        }
    | I.E_sequence {expr1; expr2} ->
      let expr1 = self expr1 in
      let expr2 = self expr2 in
      let let_binder : _ O.binder = {var = I.ValueVar.fresh ~name:"()" () ; ascr = Some (O.t_unit ()) ; attributes = Stage_common.Helpers.empty_attribute} in
      return @@ O.E_let_in {let_binder; rhs=expr1;let_result=expr2; attr = {inline=false; no_mutation=false; view = false ; public=true ; hidden = false; ast_inline = false}}
    | I.E_skip -> O.e_unit ~loc:sugar.location ~sugar ()
    | I.E_tuple t ->
      let aux (i,acc) el =
        let el = self el in
        (i+1,(O.Label (string_of_int i), el)::acc) in
      let (_, lst ) = List.fold ~f:aux ~init:(0,[]) t in
      let m = O.LMap.of_list lst in
      return @@ O.E_record m
    | I.E_assign a -> let a = assign self self_type a in return @@ O.E_assign a

and compile_module : I.module_ -> O.module_ = fun x ->
  let pass = Stage_common.Maps.declarations
    compile_expression compile_type_expression
    compile_exp_attributes compile_type_attributes compile_module_attributes
  in
  pass x
