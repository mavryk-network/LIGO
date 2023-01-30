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

let wrap ?(attributes=[]) payload payload_to_yojson region =
  object
    method payload    = payload
    val attributes    = attributes
    method attributes = attributes
    method region     = region

    method set_attributes attr = {< attributes = attr >}
    method payload_to_yojson = payload_to_yojson
  end

let make = wrap

let ghost payload payload_to_yojson = wrap ~attributes:[] payload payload_to_yojson Region.ghost

let attributes_to_yojson attributes =
  `List (List.map ~f:(Region.reg_to_yojson Attr.to_yojson) attributes)

let to_yojson payload_to_yojson wrap =
  `Assoc ([
      ("payload", payload_to_yojson wrap#payload);
      ("attributes", attributes_to_yojson wrap#attributes);
      ("region", Region.to_yojson wrap#region);
    ])
