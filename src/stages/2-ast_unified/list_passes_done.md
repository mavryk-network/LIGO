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

## pass 'pattern rest'

- remove : P_rest
- add : -
??



## pass 'restrict pattern'

- remove : P_literal _
- add : -

unsupported pattern

## pass 'restrict module opens'

only variable on the right A.t


## pass 'export_declaration'

- remove : D_Export

D_Export -> D_attr "private"/"public"

## pass 'external_hack'

see self_ast_imperative

## pass 'linearity'

- remove: E_Record_pun , T_Record_raw
- add: E_record , T_record

note: we should have unpun at type level as well .. check later :)
`E_record_pun { x ; a = 1}` |-> `E_Record { x = x ; a = 1}`
+ check linearity

```
| E_Record_pun of (Variable.t, 'self) Field.t Record.t it's a map
```


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

## pass 'freeze container'

- remove :  E_Map _ | E_BigMap _ | E_List _ | E_Set _
            | E_MapLookup _
- add : -

constantize containers


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


## pass 'object_jsligo'
  
  - remove : E_Object
  - add : ..

  note: in JS, `{...my_rec1 , x : 111 }` would be an update if `x` exists in `my_rec1`
        or would create a value of another type `{ <fields of myrec1> , x : 111}` if not.
        Unfortunately, we have no way to do that in LIGO, so we always perform the update.

  `E_Object { a : 1 , b : "2"}` |-> `E_Record_pun ..`
  `E_Object { a , b }` |-> `E_record_pun ..`
  `E_object { a , ...r}` | `E_object {...r, a}` |-> `E_Update ..`

## pass 'array_to_tuple'

  - remove : E_array
  - add : .
  ```
  [1 , ..x] |-> error
  [1 , 2 , 3] |-> TUPLE (1,2,3)
  ```

  The 'tuple_singleton' nanopass ensures E_Tuple never contains 1 element only ?

  ```
  [] |-> TUPLE ()
  [ x ] |-> TUPLE x
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

## pass 'let_syntax'

- remove : D_Let , E_Let_in
- add    : D_irrefutable_match

```
let <pattern list> <type params> : <type> = ..
|->
let <pattern> = (fun  ...)
```
  annotations in case of <pattern> needs to be propagated to lhs
  let <pattern> : <ty> = <rhs> in .. |-> let <pattern> = <rhs> : <ty>

  the rest are function with <pattern> being a variable pattern (possibly with type params, args ..)

  let <pattern> (type <type_params>) <param> : <ty> = <rhs> in ..
  |->
  let <pattern> : <forall_type> = \/ <type_params> -> <rhs>

  note: same for top-level let-ins (distinction between D_value & D_pattern)

## pass 'generalize_functions'

  - remove : E_Poly_fun D_Fun
  - add : E_Fun of param list * body , E_Abstraction , E_recursive

  `E_Poly_fun (type at bt) (var a : at) (const b : bt) : ret -> ...`
  |-> `E_Abstraction (at (E_Abstraction bt (E_Fun (var a : at) (const b : bt) : ret) -> ...))`

  `D_Fun rec f (type at bt) (var a : at) (const b : bt) : ret -> body`
  |-> `D_Const (P_var f) None = E_recursive (E_Abstraction (at (E_Abstraction bt (E_Fun (var a : at) (const b : bt) : ret))`


## pass 'curry'

- remove : E_Call, E_Fun
- add : E_Application of (expr, expr) , E_Lambda

IF option.syntax == jsligo, pascaligo :
`E_Call f ()`      |-> `E_Application f (E_literal (E_unit))`
`E_Call (f a b c)` |-> `E_Application (a,b,c)`
IF option.syntax == cameligo :
`E_Call (f a b c)` |-> `E_Application (E_Application (E_Application (f a) b) c)`

IF option.syntax == jsligo, pascaligo :
`E_Fun a b c ret body` |-> `E_lambda (E_tuple (a,b,c)) (E_Annot body)`

IF option.syntax == cameligo:
`E_Fun a b c ret body` |-> `E_lambda a (E_lambda b (E_lambda c (E_Annot body)))`

## pass 'e_update with lens'

- remove E_Update
- add : E_record_update

## pass 'record_access'

- remove : E_proj
- add : E_record_access

e_proj in structural_assignments, needs to be replaced

## pass 'tuple_as_record'

- remove : T_tuple E_tuple
- add : -

convert tuple as records


## pass 'cond to match'

- remove : E_cond
- add : -

morh conds to match on True/False