module Location = Simple_utils.Location

module type VAR = sig
  type t [@@deriving compare, yojson, hash, sexp]

  (* Create a compiler generated variable *)
  val reset_counter : unit -> unit
  val fresh : ?loc:Location.t -> ?name:string -> unit -> t
  val fresh_like : ?loc:Location.t -> t -> t

  (* Construct a user variable directly from a string. This should only
      be used for embedding user variable names. For programmatically
      generated variables, use `fresh`. Take care not to cause
      shadowing/capture except as the user intended. *)
  val of_input_var : ?loc:Location.t -> string -> t

  (* Warning : do not use *)
  val to_name_exn : t -> string
  val get_location : t -> Location.t
  val set_location : Location.t -> t -> t
  val is_generated : t -> bool

  (* Prints vars as %s or %s#%d *)
  val pp : Format.formatter -> t -> unit

  include Comparable.S with type t := t
end

module Internal () = struct
  module T = struct
    type wrap_content =
      { name : string
      ; counter : int [@default 0] [@sexp_drop_default.equal]
      ; generated : bool [@default false] [@sexp_drop_default.equal]
      }
    and t = wrap_content Location.wrap
    [@@deriving equal, compare, yojson, hash, sexp]
  end

  include T

  let global_counter = ref 1
  let reset_counter () = global_counter := 1

  let fresh ?(loc = Location.dummy) ?(name = "gen") () =
    let counter =
      incr global_counter;
      !global_counter
    in
    Location.wrap ~loc { name; counter; generated = true }


  let fresh_like ?loc v =
    let counter =
      incr global_counter;
      !global_counter
    in
    let loc = Option.value ~default:(Location.get_location v) loc in
    Location.wrap ~loc { (Location.unwrap v) with counter }


  let exists_prefix = "^gen"
  let fresh_exists ?(loc = Location.dummy) () = fresh ~loc ~name:exists_prefix ()

  (* should be removed in favor of a lift pass before ast_imperative *)
  let of_input_var ?(loc = Location.dummy) name =
    if String.equal name "_"
    then fresh ~name ~loc ()
    else Location.wrap ~loc { name; counter = 0; generated = false }


  (* This exception indicates that some code tried to throw away the
   counter of a generated variable. It is not supposed to happen. *)
  exception Tried_to_unfreshen_variable

  (* TODO delete this *)
  let to_name_exn var =
    let {name; generated; _} = Location.unwrap var in
    if generated then raise Tried_to_unfreshen_variable else name


  (* TODO remove this *)
  let internal_get_name_and_counter var =
    let {name; counter; _} = Location.unwrap var in name, counter
  let get_location var = Location.get_location var
  let set_location loc var = Location.wrap ~loc (Location.unwrap var)
  let is_generated var = (Location.unwrap var).generated

  let is_name var name = String.equal (Location.unwrap var).name name

  (* PP *)
  let pp ppf v =
    let v = Location.unwrap v in
    if v.counter <> 0
    then Format.fprintf ppf "%s#%d" v.name v.counter
    else Format.fprintf ppf "%s" v.name


  let _pp ppf v = Format.fprintf ppf "%s#%d" v.name v.counter
  let wildcard = Location.wrap ~loc:Location.dummy { name = "_"; counter = 0; generated = false }

  include Comparable.Make (T)
end

module Module_var = Internal ()
module Value_var = Internal ()
module Type_var = Internal ()
module Layout_var = Internal ()
