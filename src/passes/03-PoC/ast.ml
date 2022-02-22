module Location = Simple_utils.Location
module List = Simple_utils.List

type source_attribute = (string * string option) Location.wrap

type expression_variable = string
type type_variable = string
type module_variable = string
type ('exp,'ty) match_case = ('exp, 'ty) Stage_common.Ast_common.match_case
type mut = Mutable | Immutable
type ('var, 'ty_exp) binder = {
    var  : 'var ;
    ascr : 'ty_exp option ;
    mut : mut ; 
  }
type literal = Stage_common.Ast_common.literal
type ligo_string = Stage_common.Ast_common.ligo_string
type label = Stage_common.Ast_common.label =
  Label of string
type 'expr access = 'expr Stage_common.Ast_common.access =
  | Access_tuple of Stage_common.Ast_common.z
  | Access_record of string
  | Access_map of 'expr


(*** Language of types ***)
type 'attr declaration_type_ = {
    type_binder : type_variable ;
    type_expr : type_expression ;
    type_attr : 'attr list ;
  }
and ty_expr = | Wait
and type_expression = ty_expr

(*** Language of expressions ***)
and 'attr declaration_expression_ = {
    binder : (expression_variable, type_variable) binder;
    attr : 'attr list ;
    expr : expression ;
  }

and expression_content =
  | E_source_variable of string (* x ; failwith ; is_nat ; _ *)
  (* Until we get a user notations and externals in the syntax (i.e. `let (+) = %add`) *)
  | E_external_operator of binary_operator_ (* a + b ; a <= b ; a or b ; a := b *)
  (* calls where all the arguments are needed at call site *)
  | E_complete_pseudo_call of complete_call_ 
  (* Tezos.get_contract x ; f x ; list [ a , b ] ; match x with y -> a *)
  | E_pseudo_call of unit
  (* jsligo construct that are casted,  *)
  | E_array of unit (* [ a, b ] ; [ ] *)
  | E_object of unit (* { a : 1 ; b : 2 } , {...r , b : 2 } *)
  | E_block of unit (* { ... } *)
  | E_literal of literal
  | E_tuple of tuple_
  (* can contain { a = 1 ; a = 2} *)
  | E_record_raw of source_attribute record_
  | E_expression_access of expression exp_access_
  
  
  (* module expression *)
  | E_module_access of expression mod_access_
  | E_let_open of expression let_open_

  | E_func_update of expression func_update_
  | E_function of (expression, type_expression , type_expression pattern) func_

  | E_let_pattern of unit (* cameligo : binders list ; pascaligo : *)
  | E_matching of unit
(* effect *)
  | E_assign
  | E_patch
  | E_remove

and expression = expression_content Location.wrap
and expr = expression


(*** Language of modules ***)
and 'attr declaration_module_ = {
    module_binder : module_variable ;
    module_ : module_expr_ ;
    module_attr : 'attr list
  }
and module_path_ = {
    mod_ : module_expr_ ;
    path : module_variable Location.wrap list
  }
and module_expr_content_ =
  | M_struct of program
  | M_source_variable of module_variable
  | M_module_path of module_path_
and module_expr_ =
  module_expr_content_ Location.wrap
  (* Future:
  | M_functor of ..
  | M_apply_function of .. *)

(*** Language of declarations ***)
and 'attr declaration_ =
  | Declaration_type       of 'attr declaration_type_
  | Declaration_expression of 'attr declaration_expression_
  | Declaration_module     of 'attr declaration_module_

and 'attr declaration_list_ =
  'attr declaration_ Location.wrap list

and program = source_attribute declaration_list_



and external_notation =
  | Assign    (* :=  *)
  | Concat    (* ^   *)
  | Cons      (* #   *)
  | Equal     (* =   *)
  | Geq       (* >=  *)
  | Gt        (* >   *)
  | Leq       (* <=  *)
  | Lt        (* <   *)
  | Minus     (* -   *)
  | Neq       (* =/= *)
  | Plus_numbers (* + *)
  | Plus_numbers_and_string (* + in jsligo *)
  (* | Seq      ;   *)
  | Div     (* /   *)
  | Mul     (* *   *)
  | Mod       (* mod , % *)
  | Or
  | And
  | Not

and binary_operator_ = { op : external_notation Location.wrap ; args : expression list}

and tuple_ = expression list

and complete_call_ = { f : expression ; arg_tuple : tuple_ Location.wrap }

and 'attr field_ = {
  label : label Location.wrap ;
  rhs : expression ;
  attributes : 'attr list
}
and 'attr record_ = 'attr field_ list

and 'expr exp_access_ =
  { expr : 'expr ; path : ('expr access Location.wrap) list }

and 'expr let_open_ =
  { mod_path : module_expr_ ; body : 'expr }

and 'expr mod_access_ =
  { mod_expr : module_expr_ ; var_or_proj : 'expr }

(* <exp> with a.x.y = 2 ; b += 3 *)
and functional_update_notation =
  | Id       (* =   *)
  | Add_eq   (* +=  *)
  | Sub_eq   (* -=  *)
  | Mult_eq  (* *=  *)
  | Div_eq   (* /=  *)
  | Map_eq   (* |=  *)
and 'expr field_upd_ = {
  lhs : label Location.wrap list ;
  op : functional_update_notation Location.wrap ;
  rhs : 'expr
}
and 'expr func_update_ = {
  expr : 'expr ;
  field_updates : 'expr field_upd_ Location.wrap list
}


and list_pattern =
  | Cons of pattern * pattern
  | List of pattern list
and  pattern_repr =
  | P_unit
  | P_var of expression_variable * mut
  | P_list of list_pattern
  | P_variant of label * 'ty_exp pattern
  | P_tuple of 'ty_exp pattern list
  | P_record of label list * 'ty_exp pattern list
and 'ty_exp pattern = 'ty_exp pattern_repr Location.wrap

type 'a toto = 'a Stage_common.Ast_common.pattern


and ('expr,'ty,'param) func_ = {
  parameters : 'param list ;
  body : 'expr ;
  return_type : 'ty
}




(** Passes:

  ## attributes : convert '_source_' attributes (strings) to '_known by ligo_' attributes (variant)
    remove : source_attribute <sub_type>
    add    : known_attribute <sub_type>
    example:
      - `[@"foo"] --> error unknown`
      - `[@"layout"] --> [@ Layout]`
      - `[@layout] let e = 2 in .. --> error unused`
    throwing: warning on unknown attributes ; warning on unused atributes
  
  ## variabilize :
      - convert source variable (string) to ligo variable (Var.t) descriminating module/type/exp vars
      - convert wildcards (`_`) in patterns, variables and rows (self pass) as "fresh" variables
    remove : E_source_variable ; M_source_variable
    add :
      (* Until we get a preloaded env with externals (i.e. `let failwith = %failwith) *)
      E_external_variable
    example: let e = { _ = 2 } in ..               --> error
             { x with _ }                          --> error (warning ?)
             let _ = 2 in ..                       --> let #gen_var = 2 in ..
             match .. with | { foo = a ; bar = _ } -->  match .. with | { foo = a ; bar = #gen_var }

  ## restrict module scopes : restrict module scope to what we currently support
    remove : E_let_open_in
    add : 
    example: `M.(x+y) --> error (unsupported yet)`

  ## unsupported function without return type (is that even true ??)

  ## Pseudo externals:
    remove : E_external_variable
    add : E_constant
    example:
      now                    --> C_NOW []
      failwith "foo"         --> C_FAILWITH ["foo"]
      Tezos.get_contract x y --> C_GET_CONTRACT [x; y]

  ## Linearization
    remove: E_record_raw; 
    add:    E_record    ;
    example:
      `let (e,e) = .. in ..`        --> error
      `match .. with | (x,x) -> ..` --> error
      `type ('a,'a) foo = ..`       --> error
  
  ## Flatten module accesses
    remove: 
    add: 
    example:
      `M.A.x.M   --> error (?)`
      `let open M.A in x + y --> error (unsupported)
    
  ## Record-tuple unification:    unifiy tuple and records
    remove: E_tuple ; E_exp_access of (expression, expression access) exp_access_
    add: E_exp_access of (expression, expression access') exp_access_
    example:
      `(1,2,3) --> { '0' = 1 ; '1' = 2 ; '2' = 3}`
      `x.0     --> x.'0'`
  
  ## Functionalize map accesses
    remove: E_tuple ; E_exp_access of (expression, expression access) exp_access_
    add: E_exp_access of (expression, expression access') exp_access_

  ## external notations : convert external notation to function calls/constants
    add : E_external_operator
    remove : 
    ( `a + b  --> C_ADD a b` )

  ## Casting jsligo (self pass)
    touches : E_pseudo_call
    ( `list (..) --> E_list` )
  
  ## calls to lambda : check that all complete function are completely called
    remove : E_complete_call ; E_complete_function
    add : -
    ( <complete f> (a,b,c) --> f a b c)
    throwing: error on incomplete call of complete function
**)
