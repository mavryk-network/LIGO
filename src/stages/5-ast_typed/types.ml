[@@@warning "-30-32"]

include Stage_common.Types

type meta = Ast_core.term option

type value_content = 
  | V_literal of literal
  | V_constant of value constant
  | V_lambda of (value, term) lambda
  | V_recursive of (term, value) recursive
  | V_constructor of value constructor
  | V_record of value label_map
  | V_sum of value rows
  | V_prod of value rows
  | V_arrow of value arrow
  | V_type 
  | V_pi of (term, value) pi

and value = 
  { value_content : value_content 
  ; meta : meta [@hash.ignore] [@compare.ignore]
  ; location : location [@hash.ignore] [@compare.ignore]
  }

and type_expression = value
and ty_expr = type_expression

and term = 
  { term_content : term_content
  ; type_expression : type_expression
  ; location : location [@compare.ignore]
  }

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
  | T_record of term label_map
  | T_record_accessor of term record_accessor
  | T_record_update of term record_update
  | T_ascription of (term, ty_expr) ascription
  | T_assign of (term, ty_expr) assign
  | T_sum of term rows
  | T_prod of term rows
  | T_arrow of term arrow
  | T_type
  | T_pi of (term, ty_expr) pi
[@@deriving hash, compare]

and expression = term

and 'a rows = 
  { content : 'a row_element label_map 
  ; layout : layout
  }

and 'a row_element = 'a row_element_mini_c

and ('ty, 'exp) lambda =  
  { binder: 'ty binder 
  ; result: 'exp 
  }

and let_in = 
  { let_binder: ty_expr binder 
  ; rhs: term 
  ; let_result: term 
  ; attr: known_attributes 
  }

and mod_in = (expression , ty_expr , known_attributes , type_attribute , module_attribute) mod_in'

and 'a raw_code = 
  { language : string
  ; code : 'a
  }

and ('ty, 'exp) recursive = 
  { fun_name : term_variable
  ; fun_type : 'ty
  ; lambda : ('ty, 'exp) lambda
  }

and matching = 
  { matchee: term
  ; cases: cases
  }

and matching_content_case = 
  { constructor : label 
  ; pattern : term_variable 
  ; body : term
  }

and matching_content_variant = 
  { cases: matching_content_case list
  ; tv: type_expression
  }

and matching_content_record = 
  { fields : (type_expression binder) label_map
  ; body : term
  ; tv : type_expression;
  }

and cases =
  | Match_variant of matching_content_variant
  | Match_record of matching_content_record

and type_attribute = { public : bool ; hidden : bool }
and module_attribute = type_attribute
  
and program              = module_
and module_expr          = (expression , ty_expr , known_attributes , type_attribute , module_attribute) module_expr'
and module_              = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declarations'
and declaration          = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declaration'
and declaration_content  = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declaration_content'
and declaration_module   = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declaration_module'
and declaration_constant = (expression , ty_expr , known_attributes) declaration_constant'
and declaration_type     = (ty_expr , type_attribute) declaration_type'



