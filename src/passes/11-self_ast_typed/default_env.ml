open Helpers
open Environment
open Ast_typed



let inline_env protocol curry contract = 
  let rec f env e = 
    match e.expression_content with
    E_variable var -> 
      (match Environment.get_opt var env with
        None -> e
      | Some (env) ->
        let expr = env.definition in
        (match expr with ED_binder -> failwith "corner case"
        | ED_declaration {expression;_} -> expression
        )
      )
    | E_module_accessor {module_name;element} ->
      (match Environment.get_module_opt module_name env with
        None -> e
      |  Some (env) -> f env element
      )
    | _ -> e
  in
  map_module (f (init_env ~test:true ~curry ~protocol ())) contract