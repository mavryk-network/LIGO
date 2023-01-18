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

## pass 'top_level_restriction'

- remove : PE_Top_level_instruction

instruction not supported yet at top level

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
## pass 'constructor_application'

note: maybe one day, we won't need it anymore ? and see constructor as a function
or something?
note: is there any reason why we would like E_App ? GADT ? where Ctor is a function?

- remove : E_Ctor_App , E_constr
- add    : E_Constructor (bad names)

`E_Ctor_App A foo` |-> `E_Constructor A foo`
`E_Ctor_App A` |-> `E_Constructor A unit`
`E_Ctor_App A (foo bar baz))` |-> `E_Constructor A (E_tuple (foo bar baz))`

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

## pass 'loop_variable'

- remove : I_ForOf _
- add : -

morph for of in forIn with a prepended variable introduction if `Var

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

## pass 'reduce_switch'

- remove : I_Switch
- add :            

`I_Switch` -> `S_block (S_cond..)`

warns in case of statements after break

## pass 'unseq'

- remove : E_Sequence
- add : -

`E_Sequence` -> `E_let_in`

## pass 'structural assignments'

note: see path_of_lvalue in pascaligo abstractor
- remove : I_Struct_assign ; I_Patch ; I_Remove
- add    : I_assign (variable,expr)

`S_Instr (I_struct_assign/path/remove ..)` |-> `S_decl (_ (..))`

## pass 'assign transitivity'

- remove : E_AssignJsligo
- add: E_Assign

unsure that `a := b` in jsligo is equal to `b`

## pass 'return'

- remove : -
- add : -

emit warning on unreachable code (after returns)
handle weird cases like:
```
const g = n => {
  let output = n;

  {
    output += 1 ;
    if (n > 2) {
      return (output + 12)
    } else {
      output += 2;
    }
    output += 2;
  }

  return output
}
```
## pass 'multi_bindings'

- remove : D_multi_var , D_multi_const
- add : -

## pass 'unstate'

- remove : S_* ; E_Block_with ; E_Block_fun ; I_*
- add : E_sequence

S_Instr should only contain simple stuff like `I_While I_For_In I_For I_ForOf`

## pass 'unify_fun'

- remove : E_Block_fun
- add : -

unify with polyfun