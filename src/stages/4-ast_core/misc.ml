open Ligo_prim
open Types

let rec equal_value expr1 expr2 : bool =
  match expr1.expression_content, expr2.expression_content with
  | E_literal lit1, E_literal lit2 -> Literal_value.equal lit1 lit2
  | ( E_constant { cons_name = const1; arguments = args1 }
    , E_constant { cons_name = const2; arguments = args2 } )
    when Constant.equal_constant' const1 const2 ->
    List.for_all2_exn args1 args2 ~f:equal_value
  | ( E_constructor { constructor = constr1; element = elem1 }
    , E_constructor { constructor = constr2; element = elem2 } )
    when Label.equal constr1 constr2 -> equal_value elem1 elem2
  | ( E_module_accessor { module_path = path1; element = var1 }
    , E_module_accessor { module_path = path2; element = var2 } ) ->
    Value_var.equal var1 var2 && List.equal Module_var.equal path1 path2
  | E_record record1, E_record record2 -> Map.equal equal_value record1 record2
  | ( E_update { struct_ = struct1; path = path1; update = update1 }
    , E_update { struct_ = struct2; path = path2; update = update2 } )
    when Label.equal path1 path2 ->
    equal_value struct1 struct2 && equal_value update1 update2
  | E_tuple tuple1, E_tuple tuple2 ->
    (match List.for_all2 tuple1 tuple2 ~f:equal_value with
    | Ok result -> result
    | Unequal_lengths -> false)
  | E_ascription { anno_expr = expr1; _ }, _ -> equal_value expr1 expr2
  | _, E_ascription { anno_expr = expr2; _ } -> equal_value expr1 expr2
  | E_skip, E_skip -> true
  | E_list list1, E_list list2 ->
    (match List.for_all2 list1 list2 ~f:equal_value with
    | Ok result -> result
    | Unequal_lengths -> false)
  | E_variable _, _
  | E_lambda _, _
  | E_type_abstraction _, _
  | E_application _, _
  | E_let_in _, _
  | E_let_mut_in _, _
  | E_assign _, _
  | E_for _, _
  | E_for_each _, _
  | E_while _, _
  | E_type_in _, _
  | E_mod_in _, _
  | E_raw_code _, _
  | E_recursive _, _
  | E_accessor _, _
  | E_matching _, _
  | E_module_accessor _, _
  | E_update _, _
  | E_literal _, _
  | E_constant _, E_constant _
  | E_constant _, _
  | E_constructor _, E_constructor _
  | E_record _, _
  | E_constructor _, _
  | E_cond _, _
  | E_sequence _, _
  | E_skip, _
  | E_tuple _, _
  (* These should be values, but requires an improvement in the datastructures *)
  | E_set _, _
  | E_map _, _
  | E_big_map _, _
  | E_list _, _ -> false
