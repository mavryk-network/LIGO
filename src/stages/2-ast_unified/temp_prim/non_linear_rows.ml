open Ligo_prim

type 'ty row_element = {
  associated_type : 'ty ;
  attributes      : Attribute.t list ;
  decl_pos        : int ;
  } [@@deriving yojson]

type 'ty row = Label.t * 'ty row_element [@@deriving yojson]
type 'ty t = ('ty row) list [@@deriving yojson]