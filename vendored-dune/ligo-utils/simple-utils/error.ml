type content =
  { message : string
  ; location : Location.t option [@equal.ignore] [@hash.ignore] [@compare.ignore]
  ; children : t option
  }

and t =
  { status : string
  ; stage : string
  ; content : content
  }
[@@deriving equal, compare, hash, yojson]

let make_content ~message ?location ?children () =
  { message; location; children }

let status = "error"
let make ~stage ~content = { status; stage; content }

(* TODO: Can be replaced by derivied [to_yojson] *)
let rec to_yojson : t -> Yojson.Safe.t
  = fun e ->
    let open Yojson.Safe in
    let { status ; stage ; content } = e in
    let { message ; location ; children } = content in
    let location =
      Option.map ~f:Location.to_yojson location
      |> Option.map ~f:(fun x -> ["location", x])
      |> Option.value ~default:[]
    in
    let children =
      Option.map ~f:to_yojson children
      |> Option.map ~f:(fun x -> ["children", x])
      |> Option.value ~default:[]
    in
    let content =
      `Assoc
        ([ ("message", `String message) ] @ location @ children)
    in
    `Assoc
      [ ("status", `String status)
      ; ("stage", `String stage)
      ; ("content", content)
      ]

let ppformat_content_without_children : no_colour:bool -> Format.formatter -> content -> unit
  = fun ~no_colour f {message; location; children = _} ->
    let snippet_pp = Snippet.pp ~no_colour in
    let location = Option.value ~default:Location.dummy location in
    Format.fprintf f "@[<hv>%a@.%s@]" snippet_pp location message
