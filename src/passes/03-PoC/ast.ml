module Location = Simple_utils.Location
module List = Simple_utils.List

type source_attribute = (string * string option) Location.wrap

type expression_variable = Stage_common.Ast_common.expression_variable
type type_variable = Stage_common.Ast_common.type_variable
type module_variable = Stage_common.Ast_common.module_variable
type 'exp access = 'exp Stage_common.Ast_common.access
type ('exp,'ty) match_case = ('exp, 'ty) Stage_common.Ast_common.match_case
type 'ty pattern = 'ty Stage_common.Ast_common.pattern
type 'ty_exp binder = {
  var  : expression_variable ;
  ascr : 'ty_exp option;
  }
type literal = Stage_common.Ast_common.literal
type ligo_string = Stage_common.Ast_common.ligo_string
type label = Stage_common.Ast_common.label = Label of string




type ('ty_exp,'attr) declaration_type = {
    type_binder : type_variable ;
    type_expr : 'ty_exp ;
    type_attr : 'attr list ;
  }

and ('exp,'ty_exp,'attr) declaration_constant = {
    binder : 'ty_exp binder;
    attr : 'attr list ;
    expr : 'exp ;
  }

and ('exp,'ty_exp,'attr) module' = ('exp,'ty_exp,'attr) declaration' Location.wrap list
and module_ = (expression, type_expression, source_attribute) module'

and ('exp,'ty_expr, 'attr) declaration_module = {
    module_binder : module_variable ;
    module_ : ('exp,'ty_expr,'attr) module' ;
    module_attr : 'attr list
  }

and module_alias = {
    alias   : module_variable ;
    binders : module_variable List.Ne.t;
}

(* Module types *)

and ('exp,'ty_exp,'attr) declaration' =
  | Declaration_type of ('ty_exp, 'attr) declaration_type
  | Declaration_constant of ('exp,'ty_exp,'attr) declaration_constant
  | Declaration_module   of ('exp, 'ty_exp,'attr) declaration_module
  | Module_alias         of module_alias
  | Declration_function of unit (** NEW ? **)
and declaration = (expression, type_expression, source_attribute) declaration'


and ty_expr = | Wait
and type_expression = ty_expr

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
  | Plus_eq   (* +=  *)
  | Minus_eq  (* -=  *)
  | Times_eq  (* *=  *)
  | Slash_eq  (* /=  *)
  | Map_eq    (* |=  *)
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


and expression_content =
  
  | E_source_variable of string (* x ; failwith ; is_nat ; _ *)
  
  (* Until we get a user notations and externals in the syntax (i.e. `let (+) = %add`) *)
  | E_external_operator of binary_operator_ (* a + b ; a <= b ; a or b ; a := b *)
  
  (* calls where all the arguments are needed at call site *)
  | E_complete_pseudo_call of complete_call_ 
  (* Tezos.get_contract x ; f x ; list [ a , b ] ; match x with y -> a *)
  | E_pseudo_call of unit


  (* Until we get standard libraries and externals (i.e. `module Tezos = struct let get_contract = %contract end`) *)
  | E_pseudo_module_accessor of unit
  (* jsligo construct that are casted,  *)
  | E_array of unit (* [ a, b ] ; [ ] *)
  | E_object of unit (* { a : 1 ; b : 2 } , {...r , b : 2 } *)
  | E_block of unit (* { ... } *)

  | E_literal of literal

(* structures *)
  | E_tuple of tuple_
  | E_record of source_attribute record_

(* binders *)
  | E_let_pattern of unit (* cameligo : binders list ; pascaligo : *)
  | E_matching of unit
  | E_function


  | E_assign
  | E_patch
  | E_remove

and expression = expression_content Location.wrap
and expr = expression




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
      - convert source variable (string) to ligo variable (Var.t)
      - handle wildcards (`_`) in patterns, variables and rows (self pass)
    remove : E_source_variable
    add :
      (* Until we get a preloaded env with externals (i.e. `let failwith = %failwith) *)
      E_external_variable
    example: let e = { _ = 2 } in ..               --> error
             let _ = 2 in ..                       --> let #gen_var = 2 in ..
             match .. with | { foo = a ; bar = _ } -->  match .. with | { foo = a ; bar = #gen_var }

  ## Pseudo externals:
    remove : E_external_variable
    add : E_constant
    example:
      now                    --> C_NOW []
      failwith "foo"         --> C_FAILWITH ["foo"]
      Tezos.get_contract x y --> C_GET_CONTRACT [x; y]

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
