open Ast_typed

let insert_declaration contract decl =
  let Module_Fully_Typed contract = contract in
  let contract = decl :: contract in
  Module_Fully_Typed contract


let rec add_expression contract {expr_var;env_elt;public} = 
  if public then
    begin
      let {type_value=_;definition} = env_elt in  
      let {expression;free_variables=_;attr} = match definition with ED_binder -> failwith "corner_case"
        | ED_declaration decl -> decl in
      insert_declaration contract @@ Location.wrap @@ Declaration_constant {name = None; binder=expr_var;expr=expression;attr}
    end
  else contract

and add_type contract {type_variable;type_;public} =
  if public then
    begin
      let type_expr = match type_ with Kind () -> failwith "corner case"
        | Ty ty -> ty in
      insert_declaration contract @@ Location.wrap @@ Declaration_type {type_binder=type_variable;type_expr;type_attr={public}}
    end
  else contract

and add_module contract {module_variable;module_;public} =
  if public then
    begin
      let module_ = insert_as_header module_ @@ Module_Fully_Typed [] in
      insert_declaration contract @@ Location.wrap @@ Declaration_module {module_binder=module_variable;module_;module_attr={public}}
    end
  else contract


and insert_as_header (env: environment) contract = 
  let {type_environment;expression_environment;module_environment} = env in
  List.fold expression_environment ~f:add_expression ~init:contract
  |> fun init -> List.fold type_environment ~f:add_type ~init
  |> fun init -> List.fold module_environment ~f:add_module ~init
