[@@@warning "-30-32"]

module Ligo_string = Simple_utils.Ligo_string
include Stage_common.Types

type sugar =
  | Type_expr of Ast_sugar.type_expression
  | Expr of Ast_sugar.expression

type sugar_option = sugar option
type string_option = string option [@@deriving compare]

type type_attribute =
  { public : bool
  ; hidden : bool
  }

and module_attribute = type_attribute

and term_content =
  | T_variable of term_variable
  | T_literal of literal
  | T_constant of term constant
  | T_application of term application
  | T_lambda of (term, ty_expr) lambda
  | T_recursive of (term, ty_expr) recursive
  | T_let_in of let_in
  | T_mod_in of mod_in
  | T_module_accessor of term module_access
  | T_raw_code of term raw_code
  | T_constructor of term constructor
  | T_matching of matching
  | T_record of term_label_map
  | T_record_accessor of term record_accessor
  | T_record_update of term record_update
  | T_ascription of (term, ty_expr) ascription
  | T_assign of (term, ty_expr) assign
  | T_sum of rows
  | T_prod of rows
  | T_arrow of term arrow
  | T_type
  | T_pi of term pi
[@@deriving compare]

and term_label_map = term label_map

and rows =
  { fields : row_element label_map
  ; layout : layout option
  }

and row_element = term row_element_mini_c
and matching = (term, term) match_exp [@@deriving compare]

and term =
  { term_content : term_content
  ; sugar : sugar_option [@compare.ignore]
  ; location : location [@compare.ignore]
  }

and type_content = term_content
and type_expression = term
and ty_expr = term
and expression = term
and expr = expression
and expression_label_map = expression label_map
and expression_content = term
and type_expression_option = type_expression option

and mod_in =
  ( expression
  , ty_expr
  , known_attributes
  , type_attribute
  , module_attribute )
  mod_in'

and module_expr =
  ( expression
  , ty_expr
  , known_attributes
  , type_attribute
  , module_attribute )
  module_expr'

and let_in =
  { let_binder : ty_expr binder
  ; rhs : expression
  ; let_result : expression
  ; attr : known_attributes
  }

type module_ =
  ( expression
  , ty_expr
  , known_attributes
  , type_attribute
  , module_attribute )
  declarations'

and declaration =
  ( expression
  , ty_expr
  , known_attributes
  , type_attribute
  , module_attribute )
  declaration'

and declaration_content =
  ( expression
  , ty_expr
  , known_attributes
  , type_attribute
  , module_attribute )
  declaration_content'

and declaration_module =
  ( expression
  , ty_expr
  , known_attributes
  , type_attribute
  , module_attribute )
  declaration_module'

and declaration_constant =
  (expression, ty_expr, known_attributes) declaration_constant'

and declaration_type = (ty_expr, type_attribute) declaration_type'

type program = module_
