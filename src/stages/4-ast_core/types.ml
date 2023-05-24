open Ligo_prim
module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Row = Row.With_optional_layout

type string_option = string option

type type_content =
  | T_variable of Type_var.t
  | T_constant of Literal_types.t * int
  | T_sum of row
  | T_record of row
  | T_arrow of ty_expr Arrow.t
  | T_app of (Type_var.t Module_access.t, ty_expr) Type_app.t
  | T_module_accessor of Type_var.t Module_access.t
  | T_singleton of Literal_value.t
  | T_abstraction of ty_expr Abstraction.t
  | T_for_all of ty_expr Abstraction.t

and row = type_expression Row.t

and type_expression = type_content Location.wrap

and ty_expr = type_expression [@@deriving eq, compare, yojson, hash]
and type_expression_option = type_expression option [@@deriving eq, compare, yojson, hash]



module Access_label = struct
  type 'a t = Label.t

  let equal _ = Label.equal
  let compare _ = Label.compare
  let to_yojson _ = Label.to_yojson
  let of_yojson _ = Label.of_yojson
  let hash_fold_t _ = Label.hash_fold_t
  let pp _ = Label.pp
  let fold _ = Fun.const
  let map _ = Fun.id
  let t_of_sexp _ = Label.t_of_sexp
  let sexp_of_t _ = Label.sexp_of_t
  let iter _ = Label.iter
  let fold_map _ a b = a, b
end

module Accessor = Accessor (Access_label)
module Update = Update (Access_label)
module Value_decl = Value_decl (Value_attr)
module Type_decl = Type_decl (Type_or_module_attr)
module Module_decl = Module_decl (Type_or_module_attr)
module Pattern = Linear_pattern
module Match_expr = Match_expr.Make (Pattern)
module Pattern_decl = Pattern_decl (Pattern) (Value_attr)
module Let_in = Let_in.Make (Pattern) (Value_attr)

type expression_content =
  (* Base *)
  | E_variable of Value_var.t
  | E_literal of Literal_value.t
  | E_constant of
      expr Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expr Application.t
  | E_lambda of (expr, ty_expr option) Lambda.t
  | E_recursive of (expr, ty_expr) Recursive.t
  | E_type_abstraction of expr Type_abs.t
  | E_let_in of (expr, ty_expr option) Let_in.t
  | E_type_in of (expr, ty_expr) Type_in.t
  | E_mod_in of (expr, module_expr) Mod_in.t
  | E_raw_code of expr Raw_code.t
  (* Variant *)
  | E_constructor of expr Constructor.t (* For user defined constructors *)
  | E_matching of (expr, ty_expr option) Match_expr.t
  (* Record *)
  | E_record of expr Record.t
  | E_accessor of expr Accessor.t
  | E_update of expr Update.t
  (* Advanced *)
  | E_ascription of (expr, ty_expr) Ascription.t
  | E_module_accessor of Value_var.t Module_access.t
  (* Imperative *)
  | E_let_mut_in of (expr, ty_expr option) Let_in.t
  | E_assign of (expr, ty_expr option) Assign.t
  | E_for of expr For_loop.t
  | E_for_each of expr For_each_loop.t
  | E_while of expr While_loop.t

and expression = expression_content Location.wrap

and expr = expression [@@deriving eq, compare, yojson, hash]

and declaration_content =
  | D_value of (expr, ty_expr option) Value_decl.t
  | D_irrefutable_match of (expr, ty_expr option) Pattern_decl.t
  | D_type of ty_expr Type_decl.t
  | D_module of module_expr Module_decl.t

and declaration = declaration_content Location.wrap
and decl = declaration [@@deriving eq, compare, yojson, hash]
and module_expr_content = decl Module_expr.t
and module_expr = module_expr_content Location.wrap [@@deriving eq, compare, yojson, hash]

type module_ = decl list [@@deriving eq, compare, yojson, hash]
type program = declaration list [@@deriving eq, compare, yojson, hash]
