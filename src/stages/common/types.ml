[@@@warning "-39"]
module Location = Simple_utils.Location
module Var      = Simple_utils.Var
module List     = Simple_utils.List
include Enums

module SMap = Simple_utils.Map.Make(String)

type location = Location.t [@@deriving sexp]
type 'a location_wrap = 'a Location.wrap [@@deriving sexp]

type attributes = string list [@@deriving sexp]
type known_attributes = {
  inline: bool ;
  no_mutation: bool;
  view : bool;
  public: bool;
} [@@deriving sexp]

type expression_
and expression_variable = expression_ Var.t Location.wrap [@@deriving sexp]
let expression_variable_to_yojson var = Location.wrap_to_yojson (Var.to_yojson) var
let expression_variable_of_yojson var = Location.wrap_of_yojson (Var.of_yojson) var
let equal_expression_variable t1 t2 = Location.equal_content ~equal:Var.equal t1 t2
let compare_expression_variable t1 t2 = Location.compare_content ~compare:Var.compare t1 t2

type type_
and type_variable = type_ Var.t [@@deriving sexp]
let type_variable_to_yojson var = Var.to_yojson var
let type_variable_of_yojson var = Var.of_yojson var
type module_variable = string [@@deriving sexp]
let module_variable_to_yojson var = `String var
let module_variable_of_yojson var = `String var
let compare_module_variable = String.compare
let equal_module_variable = String.equal
type kind = unit [@@deriving sexp]
let equal_kind = Unit.equal
let compare_kind = Unit.compare

type label = Label of string [@@deriving sexp]
let label_to_yojson (Label l) = `List [`String "Label"; `String l]
let label_of_yojson = function
  | `List [`String "Label"; `String l] -> Ok (Label l)
  | _ -> Simple_utils.Utils.error_yojson_format "Label of string"
let equal_label (Label a) (Label b) = String.equal a b
let compare_label (Label a) (Label b) = String.compare a b

module LMap = struct
  include Map.Make(struct type t = label [@@deriving sexp] let compare = compare_label end)
  let fold_map ~f ~init map =
    let aux ~key ~data (init,map) =
      let acc,data = f key data init in
      acc, set ~key ~data map
    in
    fold ~f:aux map ~init:(init,empty)

  let of_list l =
    List.fold_right ~f:(fun (key, data) map -> set ~key ~data map) ~init:empty l

  let add_bindings kvl m =
    let aux prev (k, v) = set ~key:k ~data:v prev in
    List.fold_left ~f:aux ~init:m kvl
end

type 'a label_map = 'a LMap.t  [@@deriving sexp]

let const_name = function
  | Deprecated {const;_} -> const
  | Const      const     -> const
let bindings_to_yojson f g xs = `List (List.map ~f:(fun (x,y) -> `List [f x; g y]) xs)
let label_map_to_yojson row_elem_to_yojson m =
  bindings_to_yojson label_to_yojson row_elem_to_yojson (LMap.to_alist m)

type 'ty_expr row_element_mini_c = {
  associated_type      : 'ty_expr ;
  michelson_annotation : string option ;
  decl_pos : int ;
  } [@@deriving sexp]

type 'ty_exp type_app = {
  type_operator : type_variable ;
  arguments     : 'ty_exp list ;
} [@@deriving sexp]

type 'ty_expr row_element = {
  associated_type : 'ty_expr ;
  attributes      : string list ;
  decl_pos        : int ;
  } [@@deriving sexp]

type 'a module_access = {
  module_name : module_variable ;
  element     : 'a ;
} [@@deriving sexp]

(* Type level types *)
type 'ty_exp abstraction = {
  ty_binder : type_variable Location.wrap ;
  kind : kind ;
  type_ : 'ty_exp ;
} [@@deriving sexp]

type 'ty_exp rows = {
  fields     : 'ty_exp row_element label_map;
  attributes : string list ;
  } [@@deriving sexp]

type 'ty_exp arrow = {
  type1: 'ty_exp ;
  type2: 'ty_exp ;
  } [@@deriving sexp]

(* Expression level types *)
type binder_attributes = {
    const_or_var : [`Const | `Var] option;
  } [@@deriving sexp]

type 'ty_exp binder = {
  var  : expression_variable ;
  ascr : 'ty_exp option;
  attributes : binder_attributes ;
  } [@@deriving sexp]


type 'exp application = {
  lamb: 'exp ;
  args: 'exp ;
  } [@@deriving sexp]

type 'exp constant = {
  cons_name: constant' ; (* this is in enum *)
  arguments: 'exp list ;
  } [@@deriving sexp]

type ('exp,'ty_exp) lambda = {
  binder: 'ty_exp binder ;
  output_type : 'ty_exp option;
  result: 'exp ;
  } [@@deriving sexp]

type ('exp, 'ty_exp) recursive = {
  fun_name :  expression_variable ;
  fun_type : 'ty_exp ;
  lambda   : ('exp, 'ty_exp) lambda ;
  } [@@deriving sexp]

type ('exp, 'ty_exp) let_in = {
    let_binder: 'ty_exp binder ;
    rhs       : 'exp ;
    let_result: 'exp ;
    attributes: attributes ;
  } [@@deriving sexp]

type ('exp, 'ty_exp) type_in = {
    type_binder: type_variable ;
    rhs        : 'ty_exp ;
    let_result : 'exp ;
  } [@@deriving sexp]

type 'exp raw_code = {
  language : string ;
  code : 'exp ;
  } [@@deriving sexp]

type 'exp constructor = {constructor: label; element: 'exp} [@@deriving sexp]

type 'exp access =
  | Access_tuple of z
  | Access_record of string
  | Access_map of 'exp [@@deriving sexp]

type 'exp accessor = {record: 'exp; path: 'exp access list} [@@deriving sexp]
type 'exp update   = {record: 'exp; path: 'exp access list; update: 'exp} [@@deriving sexp]

type 'exp record_accessor = {record: 'exp; path: label} [@@deriving sexp]
type 'exp record_update   = {record: 'exp; path: label; update: 'exp} [@@deriving sexp]

type ('exp,'ty_exp) ascription = {anno_expr: 'exp; type_annotation: 'ty_exp} [@@deriving sexp]

type 'exp conditional = {
  condition   : 'exp ;
  then_clause : 'exp ;
  else_clause : 'exp ;
  } [@@deriving sexp]

and 'exp sequence = {
  expr1: 'exp ;
  expr2: 'exp ;
  }

and 'exp assign = {
  variable    : expression_variable ;
  access_path : 'exp access list ;
  expression  : 'exp ;
  }

and 'exp for_ = {
  binder : expression_variable ;
  start  : 'exp ;
  final  : 'exp ;
  incr   : 'exp ;
  f_body : 'exp ;
  }

and 'exp for_each = {
  fe_binder : expression_variable * expression_variable option ;
  collection : 'exp ;
  collection_type : collect_type ;
  fe_body : 'exp ;
  }

and collect_type =
  | Map
  | Set
  | List
  | Any

and 'exp while_loop = {
  cond : 'exp ;
  body : 'exp ;
  }

type 'ty_exp list_pattern =
  | Cons of 'ty_exp pattern * 'ty_exp pattern
  | List of 'ty_exp pattern list

and 'ty_exp pattern_repr =
  | P_unit
  | P_var of 'ty_exp binder
  | P_list of 'ty_exp list_pattern
  | P_variant of label * 'ty_exp pattern
  | P_tuple of 'ty_exp pattern list
  | P_record of label list * 'ty_exp pattern list

and 'ty_exp pattern = 'ty_exp pattern_repr Location.wrap [@@deriving sexp]

type ('exp , 'ty_exp) match_case = {
  pattern : 'ty_exp pattern ;
  body : 'exp
} [@@deriving sexp]

type ('exp , 'ty_exp) match_exp = {
  matchee : 'exp ;
  cases : ('exp , 'ty_exp) match_case list
} [@@deriving sexp]

(* Declaration types *)
type 'ty_exp declaration_type = {
    type_binder : type_variable ;
    type_expr : 'ty_exp ;
    type_attr : attributes ;
  }

and ('exp,'ty_exp) declaration_constant = {
    name : string option;
    binder : 'ty_exp binder;
    attr : attributes ;
    expr : 'exp ;
  }

and ('exp,'ty_expr) declaration_module = {
    module_binder : module_variable ;
    module_ : ('exp,'ty_expr) module' ;
    module_attr : attributes
  }

and module_alias = {
    alias   : module_variable ;
    binders : module_variable List.Ne.t;
}

(* Module types *)

and ('exp,'ty_exp) declaration' =
  | Declaration_type of 'ty_exp declaration_type
  | Declaration_constant of ('exp,'ty_exp) declaration_constant
  | Declaration_module   of ('exp, 'ty_exp) declaration_module
  | Module_alias         of module_alias


(* Program types *)

and ('exp,'ty_exp) module' = ('exp,'ty_exp) declaration' location_wrap list [@@deriving sexp]


type ('exp,'type_exp) mod_in = {
  module_binder : module_variable ;
  rhs           : ('exp,'type_exp) module' ;
  let_result    : 'exp ;
}

and 'exp mod_alias = {
  alias   : module_variable ;
  binders : module_variable List.Ne.t;
  result  : 'exp ;
} [@@deriving sexp]
