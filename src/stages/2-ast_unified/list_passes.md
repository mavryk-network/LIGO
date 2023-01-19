
## pass 'export_declaration'

- remove : D_Export

D_Export -> D_attr "private"/"public"

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

## pass 'unify type 1'

  we have too much representation for units:
  - Unit as E_Ctor Unit
  - () as E_tuple ()
  - () as E_literal (E_unit)
  - [%external UNIT] as E_raw_code

  same for types:
  - type unit = Unit
  - type unit = ()

  unify them all under a comon repr, should we do that ??


## pass 'expand_polymorphism'

  - remove : E_Poly_fun D_Fun
  - add : E_Fun of param list * body , E_Abstraction , E_recursive

  `E_Poly_fun (type at bt) (var a : at) (const b : bt) : ret -> ...`
  |-> `E_Abstraction (at (E_Abstraction bt (E_Fun (var a : at) (const b : bt) : ret) -> ...))`

  `D_Fun rec f (type at bt) (var a : at) (const b : bt) : ret -> body`
  |-> `D_Const (P_var f) None = E_recursive (E_Abstraction (at (E_Abstraction bt (E_Fun (var a : at) (const b : bt) : ret))`
## pass 'restrict proj'

  - remove : E_proj (.. Component_expr e)
  - add : ..

  morph it to a C_MAP_GET_FORCE / C_MAP_GET ? (not sure which one; might depend on the syntax)

## pass 'object_jsligo'
  
  - remove : E_Object
  - add : ..

  note: in JS, `{...my_rec1 , x : 111 }` would be an update if `x` exists in `my_rec1`
        or would create a value of another type `{ <fields of myrec1> , x : 111}` if not.
        Unfortunately, we have no way to do that in LIGO, so we always perform the update.

  `E_Object { a : 1 , b : "2"}` |-> `E_Record_pun ..`
  `E_Object { a , b }` |-> `E_record_pun ..`
  `E_object { a , ...r}` | `E_object {...r, a}` |-> `E_Update ..`

## pass 'linear_record'

- remove: E_Record_pun , T_Record_raw
- add: E_record , T_record

note: we should have unpun at type level as well .. check later :)
`E_record_pun { x ; a = 1}` |-> `E_Record { x = x ; a = 1}`
+ check linearity

```
| E_Record_pun of (Variable.t, 'self) Field.t Record.t it's a map
```

## pass 'array unify'

  - remove : E_array
  - add : .
  ```
  [1 , ..x] |-> error
  list([1, 2]) |-> [ 1 ; 2 ]
  list(1, ..x)) |-> 1::x
  [1 , 2 , 3] |-> TUPLE (1,2,3)
  list([]) |-> []
  ```

  The 'tuple_singleton' nanopass ensures E_Tuple never contains 1 element only ?

  ```
  [] |-> TUPLE ()
  [ x ] |-> TUPLE x
  ```

## pass 'decide curry'

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


  
## pass 'enum_attributes' TODO AT THE END ..
  - remove : E_Attr (string to string) D_attr P_attr
  - add : E_AttrEnum ((Annot | LAyout | ..)) D_attrenum P_attrEnum

  build small separated language for attributes
  - "[@@private]","[@@inline]" toplevel
  - "[@layout]" types
  - etc..

 
## pass 'discriminated union'

- remove : T_Disc [("a", Some b); ("c", Some d); ("e", None)]
- add    : -

See handling of TDisc in JsLIGO abstractor AND GOOD LUCK



