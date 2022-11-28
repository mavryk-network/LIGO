## pass 'freeze_operator' : bin_op , ternop ?
  - remove : E_binary_op E_unary_op
  - remove : E_set_membership
  - add    : E_constant
  
  morph operators into hardcoded constants (later leave them be in the stdlib ?)
  morph special syntax to constants

---

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
---

## pass 'restrict proj'

  - remove : E_proj (.. Component_expr e)
  - add : ..

  check that `e` is an int literal (for now we don't have partial exec) 

--


  EObject
  =============================================================================



  pass 'nosyntax' : [specific to.. jsligo ?]
    remove : -
    add : -

  handle cases like
    | ECall {value=(EVar {value = "list"; _}, ... -> E_literal (list ..)
    | ECall {value=(EVar {value = "match"; _}, ... -> E_matching (..)
  
  no need of a second node for that

  E_Tuple (check)
  =============================================================================
  pass 'tuple_singletons' :
    new : - 
    remove E_Tuple ( one_element_only )

    Remove singleton tuples
    E_Tuple ( E_xxx ) |-> E_xxx

  
  E_Record
  =============================================================================
  pass 'unpun' :  (PascaLigo)
    need : variabilize
    remove : E_Rec
    new : E_Record

    Unfold punned fields
    E_Rec (punned or complete fields) |-> E_Record (all complete fields)

    More precisely :
    E_Rec (expr_lhs, expr_rhs) |->
      match expr_lhs with
      | E_Variable str -> E_Record (name = str, expr_rhs)
      | E_Par inside   -> unpun (E_Rec (inside, expr_rhs))
      | _              -> failwith "Unexpected expression"
  
  
  E_RevApp
  =============================================================================
  pass 'e_rev_app' (CameLigo)
      remove : E_RevApp

      E_RevApp(x, f) |-> E_Application(lamb=f, args=x)

  E_Proj
  =============================================================================
  pass 'proj_to_access'
    remove : E_Proj
    new : E_Accessor

    Transforms :
      expr projection =
      expr                  expr
      list                  field_path
        | string            FieldName         (case 1)
        | Z.t               Component         (case 2)
    Into :
      'expr accessor =
      'expr                 record
      list                  path
        |  z                Access_tuple      (case 2)
        |  string           Access_record     (case 1)
        |  'expr            Access_map
    
    Note : the [Access_map[] case will be used for [E_MapLookup] compilation.
    Note : proj lhs is an expression (see pascaligo)
    
  E_ModA / E_ModPath
  =============================================================================
  pass 'flatten_modules'   (gone if factorized CSTs ?)
    remove : E_ModA
    add    : E_ModPath
  
    Replace nested module access of E_ModA by flat ones of E_ModPath

  pass 'modules_and_projections'     (yep, let's see with module language)
    need : - flatten_modules
           - proj_to_access
    remove : E_ModPath ( E_Proj () )
    add    : E_Proj ( E_ModPath () )

    The idea is to convert :
      M.N.(rec_or_tuple.x.1.y.2) |-> (M.N.rec_or_tuple).x.1.y.2

    In other words, transform :
    > E_ModPath
    >   module_path = ["M"; "N"]
    >   field =
    >     E_Proj
    >       expr = E_UserVar "rec_or_tuple"
    >       field_path = [
    >         FieldName", "x"
    >         Component", "1"
    >         FieldName", "y"
    >         Component", "2" 
    >       ]
    into
    > E_Proj
    >   expr =
    >     E_ModPath
    >       module_path = ["M"; "N"]
    >       field = E_UserVar "rec_or_tuple"
    >   field_path = [
    >     FieldName", "x"
    >     Component", "1"
    >     FieldName", "y"
    >     Component", "2" 
    >   ]


  E_Update
  =============================================================================
  pass : record_update_cameligo   (gone if factorized CSTs ?)
    remove : E_UpdateCameligo
    new    : E_Updates

  pass : record_update_pascaligo  (gone if factorized CSTs ?)
    needs  : - unpun
    remove : E_UpdatePascaligo
    new    : E_Updates
  
  pass : record_update_unflatten  (gone if factorized CSTs ?)
    remove : E_Updates
    new    : E_Update

    The goal of this pass is to transform flat record updates into nested ones.
    For example :
    E_Updates ( my_record, [update1; update2; update3] )
    becomes
    E_Update (
      E_Update (
        E_Update ( my_record, udpate1),
        update2
      ),
      update3
    )

  E_Fun
  =============================================================================
  
  
  pass : functions_cameligo      
    remove : E_FunCameligo
    add    : E_lambda
  
  note: same for FunJsligo , FunPascaligo ?
  note: lambda now has type Param.t for binders

  E_App
  =============================================================================

  pass : ctor_apply                (gone if factorized CSTs ? nope :()
    remove : E_App ( E_Ctor )
    add    : E_Ctor2
  
    In CameLIGO and JsLIGO, MyCtor 42 becomes E_Ctor ( "MyCtor", 42 )
    In PascaLIGO, we have MyCtor (42) which becomes
    E_App ( E_Ctor "MyCtor", 42 )
    
    We want equivalent ctor to lead to the same AST, so we want to transform :
    * E_App ( E_Ctor ("MyCtor", ()), 42 )   |-> E_Ctor ( "MyCtor", 42 )

    Also, E_App is only used in PascaLigo in conjunction with E_Ctor, so :
    * E_App ( E_xxx != E_Ctor )             |-> failwith "Unexpected expression"

    Third, there is this case :
    * E_App ( E_Ctor "Unit", None)         |-> E_Unit
    This third case should be apply for PascaLigo only,
    so it should be applied to E_App only.
    If we use a later nanopass to transform E_Ctor "Unit" into E_Unit,
    the CameLigo's Unit constructor will also be compiled as E_Unit.

  note: is there any reason why we would like E_App ? GADT ? where Ctor is a function?
    
  E_LetIn
  =============================================================================
  pass : annot_pattern
  remove: -

  annotations in case of <pattern> needs to be propagated to lhs
  let <pattern> : <ty> = <rhs> in .. |-> let <pattern> = <rhs> : <ty>

  the rest are function with <pattern> being a variable pattern (possibly with type params, args ..)

  let <pattern> (type <type_params>) <param> : <ty> = <rhs> in ..
  |->
  let <pattern> : <forall_type> = \/ <type_params> -> <rhs>

  note: same for top-level let-ins (distinction between D_value & D_pattern)
  
  E_seq
  =============================================================================
  pass e_seq
    remove : E_Seq
    add : E_Sequence | E_Unit | expression

    E_Seq []        becomes  E_Unit (is it possible to write an empty block ? in jsligo maybe)
    E_Seq [e]       becomes  e
    E_Seq [e1; e2]  becomes  E_Sequence (e1, e2)

    E_Seq [e1; e2; ...; en-1; en]  becomes
    E_Sequence ( e1,
      E_Sequence ( e2,
        ... E_Sequence ( en-1, en)
      ) 
    )

  E_MapLookup
  =============================================================================
  pass 'map_lookup_to_access' (PascaLigo)
    remove : E_MapLookup
    new : E_Access

    Transforms :
      map_lookup =
      expr                  map
      nseq                  keys
        expr
    Into :
      'expr accessor =
      'expr                 record
      list                  path
        |  z                Access_tuple
        |  string           Access_record
        |  'expr            Access_map         (use this one)
    
    Note : Only use the [Access_map] constructor here,
    the two others are used by the [proj_to_access] nanopass, for [E_Proj]
    
  E_Map / E_BigMap
  =============================================================================
  pass 'map_to_map' (PascaLigo)
    remove : AST_U.E_Map | AST_U.E_BigMap
    add    : AST_I.E_Map | AST_I.E_BigMap

    This is a direct 1-to-1 translation from AST Unified to AST Imperative
  
  pass ?
    remove : E_Call ( module = "Map" | "BigMap" )
    add    : AST_I.E_Map | AST_I.E_BigMap

    In PascaLIGO :
    > const moves : register =
    >   map [
    >     ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    >     ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
    gives a CST.E_Map, unified into AST_U.E_Map,
    then compiled to AST_I.E_Map with above 'map_to_map' nanopass

    In CameLIGO however,
    > let moves : register =
    >   Map.literal [
    >     (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    >     (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
    Is converted into a E_Call ( E_ModA (name = "Map") ... )
    This is converted somewhere in ktree_abstraction or later into a AST_I.E_Map,
    and we should do the same here.
  
  E_Attr
  =============================================================================
  pass 'check_no_attr'
    remove : E_Attr
    add : nothing | exception

    Attributes in PascaLigo are accumulated during recursive calls to
    [compile_expression], and are checked during evaluation of E_Fun.
    c.f. [check_no_attributes] in [04-tree_abstraction/pascaligo/compiler.ml]
    
    The goal of this nanopass is to accumulate those attributes
    and perform the same check, and then remove the attributes.

  E_CallJsligo
  =============================================================================
  pass 'e_call_jsligo'
    remove : E_CallJsligo
    add    : E_Call | ...

    CST.Jsligo.E_Call represent classic function calls but also some special calls.
    The classic calls will be converted to ASTU.E_Call,
    the special calls will be converted into the correct ASTU node,
    according to what's done in tree-abstraction.

    Special calls are :

    [.] Special calls to 'list'
      | ECall {value=(EVar {value = "list"; _}, Multiple {value = {inside = (EArray {value = {inside = Some (Expr_entry e, [(_, Rest_entry {value = {expr; _}; _})]); _}; _}, []); _}; _}); region } ->
      | ECall {value=(EVar {value = "list"; _}, Multiple {value = {inside = (EArray {value = {inside;lbracket=_;rbracket=_};region=_}, []); _}; _}); region } -> (
    [.] Pattern matching
      | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, EObject {value = {inside = fields; _}; region=finish})]); _}; _}); region=start} ->
      | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, ECall {value = EVar {value="list"; _}, Multiple { value = {inside = (CST.EArray {value = {inside; _}; _}, _); _} ;_} ;_})]); _}; _}); region} ->
    [.] ?
      | ECall {value=(EVar var,args);region} ->
    [X] pseudo-module call
      | ECall ({value=(EModA {value={module_name;field=_;selector=_};region=_} as value,args);region} as call) when List.mem ~equal:String.(=) built_ins module_name.value -> (

      Just convert it into ASTU.E_Call, a later nanopass will convert
      pseudo-module-calls from all syntaxes into appropriate constants.
      

  E_Constr
  =============================================================================
  pass 'abstract_e_constr
    remove : AST_U.E_Constr
    add    : AST_I.E_Constructor
    needs  :
      - tuple_singletons  (removing singleton tuples in ctor arguments)

    0 arg   : E_Constr (name, args = None)                     |-> E_Constructor name E_Unit
    1 args  : E_Constr (name, args = Some e) when e != E_Tuple |-> E_Constructor name e
    2+ args : E_Constr (name, args = Some e) when e == E_Tuple |-> E_Constructor name e

    Note : E_Constr with a E_Tuple with 1 argument are impossible
    because tuple singletons have been removed in [tuple_singleton] nanopass.



  pass 'object_jsligo'
    remove : EObject
    add : E_Record | E_Update | exception

    Handle the two cases in jsligo tree-abstraction :
    [ ] | EObject {value = {inside = (Property_rest {value = {expr; _}; _}, rest); _}; _} ->
    [ ] | EObject obj ->
  
    In the first case, object is transformed into nested record accesses
    In the second, it's transformed into a record
  
  EProjJsligo
  =============================================================================
  pass 'proj_jsligo
    remove : E_ProjJsligo
    add    : E_Proj

  It's the same story as E_ModA |-> E_ModPath, going from nested to flat.
  Transform E_ProjJsligo ( E_ProjJsligo (x, y), z) |-> E_Proj (x, [y, z])

  Also, [selection_jsligo]'s Component holds a [expr]
  While [selection]'s Componenent holds a [Z.t]
  We should convert expr |-> Z.t or exception un case of unexpected expression

  E_FunJsligo
  =============================================================================
  pass 'fun_jsligo
    remove : E_FunJsligo
    add    : E_lambda
  
    The work on functions is a bit unclear for now, whatever the syntax,
    but maybe we can merge part of this work between the various syntaxes.
    In which case we should start from a syntax-agnostic E_Fun node.
  
  E_AnnotJsligo
  =============================================================================
  pass 'literalize'
    remove : -
    add    : -

    also see Self_ast_imperative.Literals (for a check ?)

    E_AnnotJsligo and E_Annot are the same, but with Jsligo, unlike other syntaxes
    certain special sort of E_Annot are converted to other nodes.
    They correspond to the following cases in the Jsligo abstractor :
    | EAnnot {value = (EArith(Int i), _, TVar {value = "nat"; _}); region=_ } ->
    | EAnnot {value = (EArith(Int i), _, TVar {value = "tez"; _}); region=_ } ->
    | EAnnot {value = (EArith(Int i), _, TVar {value = "mutez"; _}); region=_ } ->
    | EAnnot {value = (ECodeInj {value = {language; code};_ }, kwd_as, type_expr); region} ->

    E_AnnotJsligo (special ones) |-> AST.E_some_other_node (see above)
    E_AnnotJsligo (normal ones)  |-> AST.E_Annot
  
  E_AssignJsligo
  =============================================================================
  pass 'e_assign_jsligo'
    remove : E_AssignJsligo
    add    : E_Assign | E_Sequence | exception

    Handle the 4 cases in the jsligo abstractor :
    | EAssign (EVar {value=_; region=_} as e1, op, (EAssign     (EVar _ as ev, _, _) as e2)) ->
    | EAssign (EVar {value; region} as e1, op, e2) ->
    | EAssign (EProj {value = {expr = EVar {value = evar_value; _}; selection = Component {value = {inside = EArith (Int _); _}; _} as selection}; region=_}, ({value = Eq; _} as op), e2) ->
    | EAssign _ as e ->
  
  
  D_Type / T_Arg
  =============================================================================
  pass 'd_type'
    remove : D_TypeDecl
    add    : D_Type

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

  D_Let
  =============================================================================
  pass 'd_let_tuple'
    remove : D_Let ( binder = CST.PTuple, ... )
    add    : D_Let

    The let declarations with a tuple as lhs :
      let x, y = (42, 24)
    Are actually abstracted into several let declarations, one per tuple element :
      let x = (42, 24).0  // (access 1st element of the tuple)
      let y = (42, 24).1  // (access 2nd element of the tuple)
  
  pass 'd_let_record'
    remove : D_Let (binder = CST.PRecord, ...)
    add    : D_Let

    Similarly to nanopass d_let_tuple, this nanopass split let declarations
    with records into several declarations, one per record element.
    For example :
      let {x=z, y} = my_record
    After unpunning, it becomes :
      let {x=z, y=y} = my_record
    With this nanopass d_let_record, it should become :
      let x = my_record.z
      let y = my_record.y
  
  pass 'd_let_lambda'
    remove : D_Let (binders : pattern nseq)
    add    : D_Let' (binder : pattern)

    Replace the list of patterns by E_Lambda :
    let x y z = E_xxx
    |->
    let x = E_lambda(x, E_lambda(y, E_xxx))

  pass 'd_let_type_params'
    remove : D_let (type_params = ...)
    add    : D_let (type_params = [] 
    )
    Add polymorphic binder to ascription
    For each type parameter "alpha",
    Replace rhs type : T_xxx |-> T_For_All(alpha, T_xxx)
  
  pass 'd_let_rec'
    remove : D_Let (is_rec = 1, expr = E_xxx)
    add    : D_Let (is_rec = 0, expr = E_Recursive (E_xxx...))

  T_Sum / T_Prod / T_Fun / T_Var / T_Arg  (AST_U |-> AST_I)
  =============================================================================
  pass 't_sum'
      remove : T_Sum
      add    : AST_I.T_Sum
  pass 't_prod'
      remove : T_Prod
      add    : AST_I.T_Tuple
  pass 't_fun'
      remove : T_Fun
      add    : AST_I.T_Arrow
  pass 't_var'
      remove : T_Var
      add    : AST_I.T_variable
  pass 't_record' :
      remove : T_Record
      add    : AST_I.T_Record
  
  T_RecordCameligo
  =============================================================================
  pass 't_recordcameligo'
    remove : T_RecordCameligo
    add    : T_Record (just converting nseq into list)

  T_Par
  =============================================================================
  pass 't_par'
    remove : T_Par
    add    : nothing, just unwrap T_Par( te ) |-> te

  T_AppPascaligo
  =============================================================================
  pass 't_app_pascaligo'
    remove : T_AppPascaligo
    add    : T_App | exception

    T_AppPascaligo (constr = T_Var s, type_args)
    |->
    T_App          (constr = s      , type_args)

    T_AppPascaligo (constr = _      , type_args)
    |->
    exception, a variable was expected

  T_App
  =============================================================================
  pass 't_app_michelson_types'
    remove : T_App ( "michelson_or" | "michelson_pair" | "sapling_state" )
    add    : AST_I.T_michelson_or
           | AST_I.T_michelson_pair
           | AST_I.T_sapling_state
           | AST_I.T_sapling_transaction
    needs  : - t_app_pascaligo

  pass 't_app'
    remove : T_App
    add    : AST_I.T_app
    needs  : t_app_michelson_types
  
  T_String / T_Int
  =============================================================================
  pass 't_string_and_int_unsupported' :
    remove : T_String | T_Int
    add    : exception
    needs  : - t_app_michelson_types
             - t_disc
      
    The T_String and T_Int can appear only in the context
    of a michelson type (see t_app_michelson_types)
    or a discriminated union (see t_disc).
    Once these are converted by the appropriate nanopasses,
    there should not be any remaining T_String / T_Int.
    Otherwise, it's an error.

  T_ModA
  =============================================================================
  pass 't_moda'
      remove : T_ModA
      add    : AST_I.T_module_accessor | exception

      T_ModA m1 ( T_ModA m2 ( ... ( T_Mod A mk ( T_Var s ))))
      |->
      T_module_accessor {
        module_path : [m1; m2; ...; mk]
        element     : s
      }

      T_ModA ( _ )
      |->
      exception "Expect a variable access"
  
  T_ModPath
  =============================================================================
  pass 't_modpath'
      remove : T_ModPath
      add    : AST_I.T_module_accessor | exception

      T_ModPath ( ["M1"; "M2"; "M3"], T_Var v )
      |->
      T_module_accessor {
        module_path = ["M1"; "M2"; "M3"]
        element     = v
      }

      T_ModPath ( ["M1"; "M2"; "M3"], _ ) |-> exception "Expected a variable"

  T_Attr
  =============================================================================
  pass 't_attr'
      remove : T_Attr
      add : ???
    
    Accumulate the attributes in a list
    and do whatever is done with them in the abstractor.
    Though I didn't quite understand what we do with them.
  
  T_RecordPascaligo
  =============================================================================
  pass 't_record_pascaligo'
    remove : T_RecordPascaligo
    add    : T_Record
  
    Remove type punning : { x } -> { x : x }
    Replace type_expr option by type_expr :
    Some te -> te
    None    -> T_unit
  
  T_Arg_sum_raw
  =============================================================================
  pass 'T_Arg_sum_raw'
    remove : T_Arg_sum_raw
    add    : T_Sum

    In other syntaxes, when there are several types associated with a variant,
    they are written in a tuple [MyVariant of int * string * tez],
    so we have one type, which is a tuple.
    However in JsLIGO, we write ["MyVariant", int, string, tez]
    and have a list of types [int; string; tez].
    Here, we put those types in a tuple, for unification with other syntaxes.

    T_Arg_sum_raw [int; string; tez]  |->  T_Sum (T_Tuple [int; string; tez])
  
  T_Object
  =============================================================================
  pass 't_object'
    remove : T_Object
    add    : T_Record
  
  T_Disc
  =============================================================================
  pass 't_disc' (or 'uncurry_sum_type')
    remove : T_Disc [("a", Some b); ("c", Some d); ("e", None)]
    add    : T_Sum  [("a",      b); ("c",      d); ("e", unit)] | exception

    See handling of TDisc in JsLIGO abstractor
  S_VarDecl
  =============================================================================
  pass 's_var_decl'
    remove : S_VarDecl
    add    : E_Matching

    The general case is S_VarDecl |-> E_Matching, this is the purpose of this nanopass.

    CAUTION : When the pattern is just a variable, the abstractor does
      S_VarDecl |-> E_LetIn
    However, other parts of the abstractor transform E_LetIn into E_Matching
    (which led to the idea of making nanopass 'matching_let_in')
    TODO : Between E_Matching ( pattern = just a variable ) and E_LetIn,
    which one to choose and when ?


  E_Block
  ==============================================================================
  pass 'assign_transitivity'
    remove : E_block
    add    : -
  
  [ JsLigo only ?? ]
  historically: https://gitlab.com/ligolang/ligo/-/issues/1462
  

  currently done in the same "pass" as "statements to let-in" / "pattern to let-in" / "switch case" to ifs
  const f = x => {
    let y = 0 ;
    let toto = (y = x + 1) ;
    return toto
  };
  /* f(1) == 2 both in jsligo and js */


  E_Block
  ==============================================================================
  pass 'block_to_let_in'
    remove :  E_block
    add    : --
  
  SHOULD NOT BE NEEDED SOON

  S_decl S_let S_const
  ==============================================================================
  pass 'pattern removal (?)'
    remove :  S_Decl S_Let S_Const
    add    : ??
  
  SHOULD NOT BE NEEDED SOON
  ```
  <pattern> = .. ;
  ```
  |->
  ```
  <pattern_destruct1> = .. ;
  ...
  <pattern_destructN> = .. ;
  ```

## attribify

- remove : D_Export

D_Export -> D_attr "private"/"public"

## restrict_top_level

- remove : M_Body_statements

convert statements to declaration when possible, or throw error

## multi lets

- remove : D_multi_var , D_multi_const

compile mutli var/const declarations to single var/const declaration


## simplify let_syntax

- remove : D_Let
- add    : D_Let_find_a_name

```
let <pattern list> <type params> : <type> = ..
|->
let <pattern> = (fun  ...)
```

## structural assignments

- remove : I_struct_assign
- add    : I_assign

map["blabla"].x := toto
|->
...


## end statements

- remove : I_Expr

I_expr can be I_call or I_return