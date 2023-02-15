module Location = Simple_utils.Location

type 'a of_yojson = Yojson.Safe.t -> ('a, string) Result.t
type 'a to_yojson = 'a -> Yojson.Safe.t

module type VAR = sig
  type t [@@deriving compare, yojson, hash, sexp]

  (* Create a compiler generated variable *)
  val reset_counter : unit -> unit
  val fresh : loc:Location.t -> ?name:string -> unit -> t
  val fresh_like : ?loc:Location.t -> t -> t
  (* Construct a user variable directly from a string. This should only
      be used for embedding user variable names. For programmatically
      generated variables, use `fresh`. Take care not to cause
      shadowing/capture except as the user intended. *)

  val of_input_var : loc:Location.t -> string -> t

  (* Warning : do not use *)
  val to_name_exn : t -> string
  val get_location : t -> Location.t
  val set_location : Location.t -> t -> t
  val is_generated : t -> bool

  (* Prints vars as %s or %s#%d *)
  val pp : Format.formatter -> t -> unit

  include Comparable.S with type t := t

  module Map : sig
    include module type of Map

    val hash_fold_t : 'a Hash.folder -> 'a t Hash.folder
    val to_yojson : 'a to_yojson -> 'a t to_yojson
    val of_yojson : 'a of_yojson -> 'a t of_yojson
  end
end

module Value_var : sig
  include VAR

  val is_name : t -> string -> bool

  (* Maybe bad *)
  val internal_get_name_and_counter : t -> string * int
  val wildcard : loc:Location.t -> t
end

module Type_var : sig
  include VAR

  val is_name : t -> string -> bool
end

module Module_var : sig
  include VAR

  val is_name : t -> string -> bool
end

module Layout_var : sig
  include VAR
end

module Contract_var : VAR
