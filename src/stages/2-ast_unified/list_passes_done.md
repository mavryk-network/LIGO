## pass 'type_asbtraction_declaration'
  - remove : D_Type_absctration
  - add    : D_Type

    The goal is to remove the type parameters in the D_TypeDecl node
    and inject them as T_Abstraction.
    
    D_TypeDecl
      name : my_type_name
      parameters : [ alpha ]
      type_expression : T_xxx (T_arg alpha)
    |->
    D_Type
      binder : my_type_name
      type_expresion :
        T_abstraction( (T_var alpha) , T_xxx (T_var alpha) )

## pass 'named_fun'

drop named parameter in arrow type (jsligo)

## pass 'freeze_operator' : bin_op , ternop ?
  - remove : E_binary_op E_unary_op E_set_membership E_map_lookup E_Map E_BigMap
  - add    : E_constant
  
  morph operators into hardcoded constants (later leave them be in the stdlib ?)
  morph special syntax to constants

## pass 'hack_literalize_jsligo'

- remove : .
- add    : .

also see Self_ast_imperative.Literals (for a check ?)

if option.syntax == jsligo :
They correspond to the following cases in the Jsligo abstractor :
```
| EAnnot {value = (EArith(Int i), _, TVar {value = "nat"; _}); region=_ } ->
| EAnnot {value = (EArith(Int i), _, TVar {value = "tez"; _}); region=_ } ->
| EAnnot {value = (EArith(Int i), _, TVar {value = "mutez"; _}); region=_ } ->
| EAnnot {value = (ECodeInj {value = {language; code};_ }, kwd_as, type_expr); region} ->
```

## pass 'restrict_t_app'

if T_App lhs should be a T_Var , else error


## pass t_app_michelson_types

Do an MR on dev ? or:
<!-- remove : T_App ( "michelson_or" | "michelson_pair" | "sapling_state" )
add    : AST_I.T_michelson_or
| AST_I.T_michelson_pair
| AST_I.T_sapling_state
| AST_I.T_sapling_transaction
needs  : - t_app_pascaligo -->

  T_ModA
  =============================================================================
## pass 'restrict_module_access'

note: note sure, after Open directive ? https://gitlab.com/ligolang/ligo/-/merge_requests/2149#note_1200301854

- remove : -
- add    : -

`T_ModA (T_Var x)` -> OK
`T_ModA (T_prod x y)` -> NOK

## pass 'list_as_function_call'
  
  - remove : -
  - add : -

  ONLY WHEN options.syntax == jsligo:

  `E_Call (list, [..])` |-> `E_literal (list ..)`
  `E_Call ..` |-> `E_Call ..`
  
## pass 'match_as_function_call'
  
  - remove : -
  - add : -
  
  ONLY WHEN options.syntax == jsligo:

  `E_Call (match, ..)` |-> `E_matching (..)`

## pass 'rev_app'

- remove : E_RevApp
- add : -

`E_RevApp(x, f)` |-> `E_Application (f x)`

## pass 'Switch_to_match'

- remove : I_Switch
- add :            

`S_Swtich` -> `S_Decl (D_Const _ (E_matching ...))`

## pass 'unseq'

- remove : E_Sequence
- add : -

`E_Sequence` -> `E_let_in`

## pass 'structural assignments'

note: see path_of_lvalue in pascaligo abstractor
- remove : I_Struct_assign ; I_Patch ; I_Remove
- add    : -

see pascaligo (path_of_lvalue)

`S_Instr (I_struct_assign/path/remove ..)` |-> `S_decl (_ (..))`