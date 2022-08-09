(* This file represente the context which give the association of values to types *)
open Ast_typed

module Exists_var : sig
  type t = private type_variable

  val of_type_var : type_variable -> t option
end
type exists_variable = Exists_var.t


type t

val empty : t
val pp : Format.formatter -> t -> unit
val add_value : t -> expression_variable -> type_expression -> t
val add_type : t -> type_variable -> type_expression -> t
val add_type_var : t -> type_variable -> kind -> t
val add_exists_var : t -> exists_variable -> kind -> t
val add_marker : t -> exists_variable -> t
val add_module : t -> module_variable -> t -> t
val get_value : t -> expression_variable -> type_expression option
val get_type : t -> type_variable -> type_expression option
val get_module : t -> module_variable -> t option
val get_type_vars : t -> type_variable list
val get_exists_vars : t -> exists_variable list

val get_record
  :  type_expression row_element_mini_c label_map
  -> t
  -> (type_variable option * rows) option

val get_sum
  :  label
  -> t
  -> (type_variable * type_variable list * type_expression * type_expression) list

module Well_formed : sig
  val context : t -> bool
  val type_expr : ctx:t -> type_expression -> kind option
end


val add_solution : t -> exists_variable -> type_expression -> t
val get_solution : t -> exists_variable -> type_expression option


val context_of_module_expr : outer_context:t -> Ast_typed.module_expr -> t
val init : ?env:Environment.t -> unit -> t

module Hashes : sig
  val set_context : t -> unit
  val hash_types : unit -> unit
  val find_type : type_expression -> (module_variable list * type_variable) option
end
