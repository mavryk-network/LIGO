module AST = Ast_unified

type 'a expr = AST.expr
type fix_expr (*dummy ... after we have AST.types == polyvariants *)


(* type ty_variable = [%import: Ty_variable.t] [@@deriving sexp] *)

(* module Tyvar = Ty_variable [@@deriving sexp] *)


open AST

module Ty_variable_with_sexp = struct
  include Ty_variable

  let t_of_sexp : Sexplib0.Sexp.t -> t = function
    | Atom s -> Ty_variable.of_input_var s
    | (List _) as other -> Sexplib0.Sexp_conv_error.no_matching_variant_found
    "Expected atom but got list"
    other
  
  let sexp_of_t : t -> Sexplib0.Sexp.t = fun t ->
    let s = Format.asprintf "%a" Ty_variable.pp t in
    Sexplib0.Sexp.Atom s
end

module Z_with_sexp = struct
  include Z

  let t_of_sexp : Sexplib0.Sexp.t -> t = function
  | Atom s -> Z.of_string s
  | (List _) as other ->
    Sexplib0.Sexp_conv_error.no_matching_variant_found
    "Expected atom but got list"
    other

  let sexp_of_t : t -> Sexplib0.Sexp.t = fun t ->
    Sexplib0.Sexp.Atom (Z.to_string t)
end

module Loc = struct
  type location = int [@@deriving sexp]
  type 'a t = 'a * location [@@deriving map, sexp]

  (* Just don't print locations for readability *)
  (* TODO : How to add an option to toggle printing of locations on sexp ? *)
  let sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun sexp_of_a t ->
      match sexp_of_t sexp_of_a t with
      | Sexplib0.Sexp.List [sexp_a; _sexp_loc] -> sexp_a
      | _ as other -> other

end

type 't type_expr = [
| `T_Prod         of unit Loc.t
| `T_App          of unit Loc.t
| `T_Fun          of ('t * 't) Loc.t
| `T_Named_fun    of 't Named_fun.t Loc.t
| `T_Par          of 't Loc.t
| `T_Var          of Ty_variable_with_sexp.t Loc.t
| `T_String       of string Loc.t
| `T_Int          of (string * Z_with_sexp.t) Loc.t
| `T_ModA         of unit
| `T_Arg          of string Loc.t
| `T_Sum_raw      of 't option Non_linear_rows.t Loc.t
| `T_Arg_sum_raw  of 't list option Non_linear_rows.t Loc.t
| `T_Record_raw   of 't option Non_linear_rows.t Loc.t
| `T_Disc_union   of 't Non_linear_disc_rows.t Loc.t
| `T_Attr         of Attribute.t * 't Loc.t
]  [@@deriving map, sexp]

(* and 't type_expr = {
  type_expression_content : 't type_expression_content;
  location                : int
} *)

type fix_type_expr = fix_type_expr type_expr [@@deriving sexp]


let rec fold_expr
  (f:'a type_expr -> 'a)
  (t:fix_type_expr) : 'a =
  f (map_type_expr (fold_expr f) t)


let pass_t_arg : fix_type_expr Small_passes.pass =
  let name = "pass_remove_t_arg" in
  let compile : Small_passes.syntax -> fix_type_expr -> fix_type_expr =
    fun _syntax te ->
    let f : fix_type_expr -> fix_type_expr = function
    | `T_Arg (s, loc) -> `T_Var (Ty_variable.of_input_var s, loc)
    | _ as common -> common
    in
    fold_expr f te
  in
  let decompile : Small_passes.syntax -> fix_type_expr -> fix_type_expr =
    fun _syntax t -> t
  in
  let check_reductions : fix_type_expr -> bool =
    fun _ -> true
  in
  {name; compile; decompile; check_reductions}


