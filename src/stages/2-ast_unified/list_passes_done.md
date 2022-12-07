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

## pass 'freeze_operator' : bin_op , ternop ?
  - remove : E_binary_op E_unary_op
  - remove : E_set_membership
  - add    : E_constant
  
  morph operators into hardcoded constants (later leave them be in the stdlib ?)
  morph special syntax to constants