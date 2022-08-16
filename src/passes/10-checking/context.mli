(* This file represente the context which give the association of values to types *)
open Ast_typed

module Exists_var : sig
  type t = private type_variable [@@deriving compare]

  val pp : Format.formatter -> t -> unit
  val yojson_of_t : t -> Yojson.json
  val loc : t -> Location.t
  val equal : t -> t -> bool
  val of_type_var : type_variable -> t option
  val fresh : ?loc:Location.t -> unit -> t
end

type exists_variable = Exists_var.t

type t

and item =
  | C_value of expression_variable * type_expression
  | C_type of type_variable * type_expression
  | C_type_var of type_variable * kind
  | C_exists_var of exists_variable * kind
  | C_exists_eq of exists_variable * kind * type_expression
  | C_marker of exists_variable
  | C_module of module_variable * t

val empty : t
val add : t -> item -> t
val of_list : item list -> t
val ( |:: ) : t -> item -> t
val join : t -> t -> t
val ( |@ ) : t -> t -> t
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
val get_type_var : t -> type_variable -> kind option
val get_exists_var : t -> exists_variable -> kind option
val add_exists_eq : t -> exists_variable -> kind -> type_expression -> t
val get_exists_eq : t -> exists_variable -> type_expression option
val insert_at : t -> at:item -> hole:t -> t
val split_at : t -> at:item -> t * t
val drop_until : t -> at:item -> t
val apply : t -> type_expression -> type_expression

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

module Elaboration : sig
  type context := t
  type 'a t

  include Monad.S with type 'a t := 'a t

  val run : 'a t -> ctx:context -> 'a
end

val context_of_module_expr : outer_context:t -> Ast_typed.module_expr -> t
val init : ?env:Environment.t -> unit -> t

module Hashes : sig
  val set_context : t -> unit
  val hash_types : unit -> unit
  val find_type : type_expression -> (module_variable list * type_variable) option
end
