(* Wrapping attributes and regions with tokens *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Local dependencies *)

type attributes = Attr.attribute Region.reg list

(* Wrapping tokens with metadata *)

type 'payload wrap = <
  payload    : 'payload;
  attributes : attributes;
  region     : Region.t;

  set_attributes : attributes -> 'payload wrap;
  payload_to_yojson : 'payload -> Yojson.Safe.t
>

type 'a t = 'a wrap

val wrap : ?attributes:attributes -> 'a -> ('a -> Yojson.Safe.t) -> Region.t -> 'a wrap
val make : ?attributes:attributes -> 'a -> ('a -> Yojson.Safe.t) -> Region.t -> 'a wrap

val ghost : 'a -> ('a -> Yojson.Safe.t) -> 'a wrap

val to_yojson : ('payload -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
