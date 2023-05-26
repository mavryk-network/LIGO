open Ligo_prim
module Location = Simple_utils.Location
module Row = Row.With_layout

type type_variable = Type_var.t
type expression_variable = Value_var.t

type module_variable = Module_var.t

and type_content =
  | T_variable of Type_var.t
  | T_constant of type_injection
  | T_sum of type_expression Row.t
  | T_record of type_expression Row.t
  | T_arrow of ty_expr Arrow.t
  | T_singleton of Literal_value.t
  | T_abstraction of ty_expr Abstraction.t
  | T_for_all of ty_expr Abstraction.t

and type_injection =
  { language : string
  ; injection : Ligo_prim.Literal_types.t
  ; parameters : ty_expr list
  }

and row = ty_expr Row.t
and te_list = type_expression list
and annot_option = string option
and row_element = ty_expr Row.t

and type_expression_data =
  { type_content : type_content
  ; orig_var : Type_var.t option [@ignore]
  }

and type_expression = type_expression_data Location.wrap
and ty_expr = type_expression [@@deriving equal, compare, yojson, hash]

module Value_attr = Value_attr
module Access_label = Access_label
module Accessor = Accessor (Access_label)
module Update = Update (Access_label)
module Value_decl = Value_decl (Value_attr)
module Type_decl = Type_decl (Type_or_module_attr)
module Module_decl = Module_decl (Type_or_module_attr)
module Pattern = Linear_pattern
module Match_expr = Match_expr.Make (Pattern)
module Let_in = Let_in.Make (Pattern) (Value_attr)
module Pattern_decl = Pattern_decl (Pattern) (Value_attr)

type expression_content =
  (* Base *)
  | E_variable of Value_var.t
  | E_literal of Literal_value.t
  | E_constant of
      expr Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expr Application.t
  | E_lambda of (expr, ty_expr) Lambda.t
  | E_recursive of (expr, ty_expr) Recursive.t
  | E_let_in of (expr, ty_expr) Let_in.t
  | E_mod_in of (expr, module_expr) Mod_in.t
  | E_raw_code of expr Raw_code.t
  | E_type_inst of type_inst
  | E_type_abstraction of expr Type_abs.t
  (* Variant *)
  | E_constructor of expr Constructor.t (* For user defined constructors *)
  | E_matching of (expr, ty_expr) Match_expr.t
  (* Record *)
  | E_record of expr Record.t
  | E_accessor of expr Accessor.t
  | E_update of expr Update.t
  | E_module_accessor of Value_var.t Module_access.t
  (* Imperative *)
  | E_let_mut_in of (expr, ty_expr) Let_in.t
  | E_assign of (expr, ty_expr) Assign.t
  | E_deref of Value_var.t
  | E_for of expr For_loop.t
  | E_for_each of expr For_each_loop.t
  | E_while of expr While_loop.t

and type_inst =
  { forall : expression
  ; type_ : type_expression
  }

and expression_data =
  { expression : expression_content
  ; type_expression : type_expression
  }

and expression = expression_data Location.wrap

and expr = expression [@@deriving eq, compare, yojson, hash]

and declaration_content =
  | D_value of (expr, ty_expr) Value_decl.t
  | D_irrefutable_match of (expr, ty_expr) Pattern_decl.t
  | D_type of ty_expr Type_decl.t
  | D_module of module_expr Module_decl.t

and declaration = declaration_content Location.wrap
and decl = declaration [@@deriving eq, compare, yojson, hash]
and module_content = decl Module_expr.t [@@deriving eq, compare, yojson, hash]

and module_expr =
  { module_content : module_content
  ; module_location : Location.t [@eq.ignore] [@hash.ignore]
  ; signature : signature
  }
[@@deriving eq, compare, yojson, hash]

and sig_item =
  | S_value of Value_var.t * ty_expr * sig_item_attribute
  | S_type of Type_var.t * ty_expr
  | S_module of Module_var.t * signature

and sig_item_attribute =
  { entry : bool
  ; view : bool
  }

and signature = sig_item list

type module_ = decl list [@@deriving eq, compare, yojson, hash]
type program = declaration list [@@deriving eq, compare, yojson, hash]
