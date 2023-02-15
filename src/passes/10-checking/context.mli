(* This file represente the context which give the association of values to types *)
module Location = Simple_utils.Location
module List = Simple_utils.List
open Ligo_prim

module Signature : sig
  type module_
  type contract

  type 'a t = 'a item list

  and _ item =
    | S_value : Value_var.t * Type.t -> 'a item
    | S_type : Type_var.t * Type.t -> 'a item
    | S_module : Module_var.t * module_ t -> 'a item
    | S_contract : Contract_var.t * contract t -> 'a item
    | S_entry : Value_var.t * Type.t Entry_type.t -> contract item
    | S_view : Value_var.t * Type.t View_type.t -> contract item

  type m = module_ t
  type c = contract t

  val get_value : 'a t -> Value_var.t -> Type.t option
  val get_type : 'a t -> Type_var.t -> Type.t option
  val get_module : 'a t -> Module_var.t -> module_ t option
  val get_contract : 'a t -> Contract_var.t -> contract t option
  val pp : Format.formatter -> 'a t -> unit
  val pp_item : Format.formatter -> 'a item -> unit

  (** Contract signature  specific functions *)
  val get_entry : c -> Value_var.t -> Type.t Entry_type.t option

  val get_view : c -> Value_var.t -> Type.t View_type.t option

  val get_entry_or_view
    :  c
    -> Value_var.t
    -> ( [ `Entry of Type.t Entry_type.t | `View of Type.t View_type.t ]
       , [> `Not_found | `Both_found ] )
       Result.t

  val get_contract_storage : c -> Type.t option

  val to_contract_type
    :  c
    -> (Type.t Contract_type.t, [> `Undefined_storage | `No_entry_point ]) Result.t
end

type t
and pos
and mut_lock

and mutable_flag = Param.mutable_flag =
  | Mutable
  | Immutable

and item =
  | C_value of Value_var.t * mutable_flag * Type.t
  | C_type of Type_var.t * Type.t
  | C_type_var of Type_var.t * Kind.t
  | C_module of Module_var.t * Signature.m
  | C_contract of Contract_var.t * Signature.c
  | C_texists_var of Type_var.t * Kind.t
  | C_texists_eq of Type_var.t * Kind.t * Type.t
  | C_lexists_var of Layout_var.t * fields
  | C_lexists_eq of Layout_var.t * fields * Type.layout
  | C_pos of pos
      (** A mutable lock is a "fitch-style lock". A lock is used to 
          "lock" mutable variables in the context from being used. 
          
          Namely, this is used to prevent functions from capturing 
          mutable variables. 
      *)
  | C_mut_lock of mut_lock

and fields = Label.Set.t

val empty : t
val add : t -> item -> t
val of_list : item list -> t
val ( |:: ) : t -> item -> t
val join : t -> t -> t
val ( |@ ) : t -> t -> t
val pp : Format.formatter -> t -> unit
val add_value : t -> Value_var.t -> mutable_flag -> Type.t -> t
val add_mut : t -> Value_var.t -> Type.t -> t
val add_imm : t -> Value_var.t -> Type.t -> t
val add_type : t -> Type_var.t -> Type.t -> t
val add_type_var : t -> Type_var.t -> Kind.t -> t
val add_texists_var : t -> Type_var.t -> Kind.t -> t
val add_texists_eq : t -> Type_var.t -> Kind.t -> Type.t -> t
val add_lexists_var : t -> Layout_var.t -> fields -> t
val add_lexists_eq : t -> Layout_var.t -> fields -> Type.layout -> t
val add_module : t -> Module_var.t -> Signature.m -> t
val add_contract : t -> Contract_var.t -> Signature.c -> t

val get_value
  :  t
  -> Value_var.t
  -> (mutable_flag * Type.t, [> `Mut_var_captured | `Not_found ]) result

val get_imm : t -> Value_var.t -> Type.t option
val get_mut : t -> Value_var.t -> (Type.t, [> `Mut_var_captured | `Not_found ]) result
val get_type : t -> Type_var.t -> Type.t option
val get_module : t -> Module_var.t -> Signature.m option
val get_contract : t -> Contract_var.t -> Signature.c option
val get_type_vars : t -> Type_var.Set.t
val get_texists_vars : t -> Type_var.Set.t
val get_lexists_vars : t -> Layout_var.Set.t
val get_type_var : t -> Type_var.t -> Kind.t option
val get_texists_var : t -> Type_var.t -> Kind.t option
val get_texists_eq : t -> Type_var.t -> Type.t option
val get_lexists_eq : t -> Layout_var.t -> (fields *  Type.layout) option
val get_signature : t -> Module_var.t List.Ne.t -> Signature.m option
val get_contract_signature : t -> Contract_var.t Module_access.t -> Signature.c option

val get_type_or_type_var
  :  t
  -> Type_var.t
  -> [ `Type of Type.t | `Type_var of Kind.t ] option

val item_of_signature_item : 'a Signature.item -> item option
val add_signature_item : t -> 'a Signature.item -> t
val add_signature_items : t -> 'a Signature.item list -> t
val insert_at : t -> at:item -> hole:t -> t
val split_at : t -> at:item -> t * t
val mark : t -> t * pos
val lock : t -> t * mut_lock

val generalize
  :  t
  -> Type.t
  -> pos:pos
  -> loc:Location.t
  -> t * Type.t * (Type_var.t * Kind.t) list * Substitution.t

type 'a apply = t -> 'a -> 'a

type _ exit =
  | Drop : t exit
  | Lift : 'a apply -> (t * 'a) exit

val drop_until : 'a -> on_exit:'a exit -> pos:pos -> 'a * Substitution.t
val unlock : 'a -> on_exit:'a exit -> lock:mut_lock -> 'a * Substitution.t
val get_record : t -> Type.t Label.Map.t -> (Type_var.t option * Type.row) option
val get_sum : t -> Label.t -> (Type_var.t * Type_var.t list * Type.t * Type.t) list

module Well_formed : sig
  val context : t -> bool
  val type_ : ctx:t -> Type.t -> Kind.t option
  val layout : ctx:t -> Type.layout -> bool
end

module Apply : sig
  val type_ : t -> Type.t -> Type.t
  val row : t -> Type.row -> Type.row
  val sig_item : t -> 'a Signature.item -> 'a Signature.item
  val sig_ : t -> 'a Signature.t -> 'a Signature.t
  val contract_sig : t -> Type.t Contract_signature.t -> Type.t Contract_signature.t
end

module Hashes : sig
  val set_context : t -> unit
  val hash_types : unit -> unit
  val find_type : Type.t -> (Module_var.t list * Type_var.t) option
end

module Diff : sig
  val pp : Format.formatter -> t * t -> unit
end
