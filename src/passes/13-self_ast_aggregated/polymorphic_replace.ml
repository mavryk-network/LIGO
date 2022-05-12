open Ast_aggregated

let expression ~protocol : expression -> expression = fun expr ->
  match expr.expression_content with
  | E_constant { cons_name = C_POLYMORPHIC_ADD ; arguments = [l; r] } ->
     let decide e = match e with
       | { type_expression ; _ } when is_t_string type_expression -> Some `Concat
       | _ -> None in
     let cons_name =
       Option.value ~default:`Add @@ List.find_map [l; r] ~f:decide in
     let s = match cons_name with
       | `Add -> Ligo_string.verbatim "{ UNPAIR ; ADD }"
       | `Concat -> Ligo_string.verbatim "{ UNPAIR ; CONCAT }" in
     let e_a_string = e_a_string s in
     let lamb = e_a_raw_code "Michelson" ({ e_a_string with type_expression = (t_arrow (t_pair l.type_expression r.type_expression) expr.type_expression ()) }) (t_arrow (t_pair l.type_expression r.type_expression) expr.type_expression ()) in
     let expression = e_a_application lamb (e_a_pair l r) expr.type_expression in
     let expression_content = expression.expression_content in
     { expr with expression_content }
  | E_constant { cons_name = C_POLYMORPHIC_SUB ; arguments = [l; r] } ->
     let decide e = match e with
       | { type_expression ; _ } when is_t_tez type_expression && Environment.Protocols.(equal Ithaca protocol) -> Some `Sub_mutez
       | _ -> None in
     let cons_name =
       Option.value ~default:`Sub @@ List.find_map [l; r] ~f:decide in
     let s = match cons_name with
       | `Sub_mutez -> Ligo_string.verbatim "{ UNPAIR ; SUB_MUTEZ }"
       | `Sub -> Ligo_string.verbatim "{ UNPAIR ; SUB }" in
     let e_a_string = e_a_string s in
     let lamb = e_a_raw_code "Michelson" ({ e_a_string with type_expression = (t_arrow (t_pair l.type_expression r.type_expression) expr.type_expression ()) }) (t_arrow (t_pair l.type_expression r.type_expression) expr.type_expression ()) in
     let expression = e_a_application lamb (e_a_pair l r) expr.type_expression in
     let expression_content = expression.expression_content in
     { expr with expression_content }
  | _ -> expr
