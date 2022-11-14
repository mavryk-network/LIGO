module type Row_lhs = sig
  type t [@@deriving eq, compare, yojson, hash, sexp]

  val of_string : string -> t
  val to_string : t -> string
end

module Make (Row_lhs : Row_lhs) = struct
  type 'ty row_element =
    { associated_type : 'ty
    ; attributes : Attribute.t list
    ; decl_pos : int
    }
  [@@deriving yojson, map, sexp]

  type 'ty row = Row_lhs.t * 'ty row_element [@@deriving yojson, map, sexp]
  type 'ty t = 'ty row list [@@deriving yojson, map, sexp]

  let make : type ty. (Row_lhs.t * ty * Attribute.t list) list -> ty t =
   fun lst ->
    let make_row : int -> Row_lhs.t * ty * Attribute.t list -> ty row =
     fun i (label, associated_type, attributes) ->
      let rows = { decl_pos = i; associated_type; attributes } in
      label, rows
    in
    List.mapi ~f:make_row lst
end