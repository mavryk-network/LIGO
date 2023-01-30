(* Wrapping attributes and regions with tokens *)
open Simple_utils

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
>

type 'a t = 'a wrap

let wrap ?(attributes=[]) payload _payload_to_yojson region =
  object
    method payload    = payload
    val attributes    = attributes
    method attributes = attributes
    method region     = region

    method set_attributes attr = {< attributes = attr >}
  end

let make = wrap

let ghost payload payload_to_yojson = wrap ~attributes:[] payload payload_to_yojson Region.ghost

let attributes_to_yojson attributes =
  `List (List.map ~f:(Region.reg_to_yojson Attr.to_yojson) attributes)

let attributes_of_yojson region (yojson: Yojson.Safe.t) =
  match yojson with
  | `List attributes -> Ok ( List.map ~f:(fun a -> match (a |> Attr.of_yojson) with | Ok a -> Region.{ region; value = a} | Error e -> failwith e) attributes)
  | _ ->
     Utils.error_yojson_format "{payload: payload, attributes: attributes, region: Region.t}"

let to_yojson payload_to_yojson wrap =
  `Assoc ([
      ("payload", payload_to_yojson wrap#payload);
      ("attributes", attributes_to_yojson wrap#attributes);
      ("region", Region.to_yojson wrap#region);
    ])

let of_yojson payload_of_yojson (wrap: Yojson.Safe.t) =
  match wrap with
  | `Assoc ([
      ("payload", payload);
      ("attributes", attributes);
      ("region", region);
    ]) -> ( match Region.of_yojson region with
| Error _ -> Error "Region.of_yojson failed."
| Ok region ->
   (match (payload_of_yojson payload, attributes_of_yojson region attributes ) with
                         | Ok payload, Ok attributes -> let wrap =  object
    method payload    = payload
    val attributes    = attributes
    method attributes = attributes
    method region     = region
    method set_attributes attr = {< attributes = attr >}
  end
           in Ok wrap

           | _ -> Error "Wrap.of_yojson failed.") )
  | _ ->
     Utils.error_yojson_format "{payload: payload, attributes: attributes, region: Region.t}"
