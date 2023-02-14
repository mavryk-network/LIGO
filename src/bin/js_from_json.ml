open Js_of_ocaml

type 'value op =
  { path : string
  ; v : 'value
  }

let debugEnabled = false
let debug msg = if debugEnabled then print_endline msg

let isInteger : Js.number Js.t -> bool Js.t =
  Js.Unsafe.js_expr ("Number.isInteger" [@reason.raw_literal "Number.isInteger"])


module Path : sig
  type t

  val toString : t -> string
  val ofString : string -> t

  type component =
    [ `ArrayIndex of int
    | `AssocKey of string
    ]

  val descend : f:('a -> component -> 'a) -> init:'a -> t -> 'a
  val root : t -> component option
  val base : t -> t option
  val last : t -> component option
  val componentOfString : string -> component
  val componentToString : component -> string
end = struct
  type t = string list

  type component =
    [ `ArrayIndex of int
    | `AssocKey of string
    ]

  let ofString input = String.split_on_char '.' input
  let toString input = String.concat ("." [@reason.raw_literal "."]) input

  let componentOfString str =
    try `ArrayIndex (str |> int_of_string) with
    | _ -> `AssocKey str


  let componentToString = function
    | `ArrayIndex i -> string_of_int i
    | `AssocKey k -> k


  let root = function
    | [] -> None
    | [ "" ] -> None
    | "" :: a :: rest -> Option.some @@ componentOfString a
    | _ -> None


  let base = function
    | [] -> None
    | [ "" ] -> None
    | "" :: rest ->
      let skipLast l =
        let counter = ref (-1) in
        let f item acc =
          counter := !counter + 1;
          if !counter = 0 then [] else item :: acc
        in
        List.fold_right f l []
      in
      Option.some @@ ("" :: skipLast rest)
    | _ -> None


  let last = function
    | [] -> None
    | [ "" ] -> None
    | "" :: rest ->
      let last l =
        let counter = ref (-1) in
        let f item acc =
          counter := !counter + 1;
          if !counter = 0 then Some (item |> componentOfString) [@explicit_arity] else acc
        in
        List.fold_right f l None
      in
      last rest
    | _ -> None


  let rec descend ~f ~init path =
    match path with
    | "" :: rest -> descend ~f ~init rest
    | hd :: rest ->
      let hd =
        try `ArrayIndex (int_of_string hd) with
        | _ -> `AssocKey hd
      in
      let init = f init hd in
      descend ~f ~init rest
    | [] -> init
end

module Compile = struct
  type json =
    | Int of int
    | Float of float
    | String of string
    | List of json array
    | Assoc of (string, json) Hashtbl.t

  let rec toYojson = function
    | ((Int i) [@explicit_arity]) -> `Int i
    | ((Float f) [@explicit_arity]) -> `Float f
    | ((String s) [@explicit_arity]) -> `String s
    | ((List arr) [@explicit_arity]) -> `List (arr |> Array.map toYojson |> Array.to_list)
    | ((Assoc tbl) [@explicit_arity]) ->
      let f k v acc = (k, toYojson v) :: acc in
      `Assoc (Hashtbl.fold f tbl [])


  let rec toString v = v |> toYojson |> Yojson.Safe.to_string

  let resolvePathAndSetValue path json acc =
    let f acc component =
      match acc, component with
      | ((Some ((List accArray) [@explicit_arity])) [@explicit_arity]), `ArrayIndex i ->
        (try Some accArray.(i) [@explicit_arity] with
        | Invalid_argument _ -> None)
      | ((Some ((Assoc tbl) [@explicit_arity])) [@explicit_arity]), `AssocKey key ->
        Hashtbl.find_opt tbl key
      | _ -> None
    in
    let fullPath = Path.ofString path in
    match fullPath |> Path.base with
    | ((Some basePath) [@explicit_arity]) ->
      let lastComponent = Path.last fullPath in
      (match basePath |> Path.descend ~f ~init:(Option.some acc), lastComponent with
      | ( ((Some (Assoc _)) [@explicit_arity])
        , ((Some (`ArrayIndex lastComponent)) [@explicit_arity]) ) ->
        failwith
          (("Base is Assoc but key is an array index: " [@reason.raw_literal
                                                          "Base is Assoc but key is an \
                                                           array index: "])
          ^ Path.toString fullPath
          ^ (" json: " [@reason.raw_literal " json: "])
          ^ toString json
          ^ (" last: " [@reason.raw_literal " last: "])
          ^ string_of_int lastComponent)
      | ( ((Some ((Assoc tbl) [@explicit_arity])) [@explicit_arity])
        , ((Some (`AssocKey lastComponent)) [@explicit_arity]) ) ->
        debug
          (("Setting path: " [@reason.raw_literal "Setting path: "])
          ^ Path.toString basePath
          ^ (" last: " [@reason.raw_literal " last: "])
          ^ lastComponent);
        Hashtbl.add tbl lastComponent json;
        acc
      | ( ((Some ((List arr) [@explicit_arity])) [@explicit_arity])
        , ((Some (`ArrayIndex lastComponent)) [@explicit_arity]) ) ->
        (try arr.(lastComponent) <- json with
        | e ->
          print_endline
            (("Failed setting array index: " [@reason.raw_literal
                                               "Failed setting array index: "])
            ^ string_of_int lastComponent);
          raise e);
        acc
      | ((Some json) [@explicit_arity]), None ->
        failwith
          (("lastComponent is None: " [@reason.raw_literal "lastComponent is None: "])
          ^ Path.toString fullPath
          ^ (" " [@reason.raw_literal " "])
          ^ toString json)
      | ((Some json) [@explicit_arity]), ((Some component) [@explicit_arity]) ->
        failwith
          (("Path resolved to json primitive value. Not Assoc or List: " [@reason.raw_literal
                                                                           "Path \
                                                                            resolved to \
                                                                            json \
                                                                            primitive \
                                                                            value. Not \
                                                                            Assoc or \
                                                                            List: "])
          ^ Path.toString fullPath
          ^ (" json: " [@reason.raw_literal " json: "])
          ^ toString json
          ^ (" last: " [@reason.raw_literal " last: "])
          ^ Path.componentToString component)
      | None, _ ->
        failwith
          (("Path doesn't exist: " [@reason.raw_literal "Path doesn't exist: "])
          ^ Path.toString fullPath))
    | None -> acc


  let run ops =
    let f acc item =
      let path =
        Js.Unsafe.get item ("path" [@reason.raw_literal "path"]) |> Js.to_string
      in
      let v = Js.Unsafe.get item ("v" [@reason.raw_literal "v"]) in
      match v |> Js.typeof |> Js.to_string with
      | ("number" [@reason.raw_literal "number"]) ->
        let n = Js.float_of_number v in
        let jsonNumber =
          match isInteger v |> Js.to_bool with
          | true -> Int (n |> int_of_float) [@explicit_arity]
          | false -> Float n [@explicit_arity]
        in
        resolvePathAndSetValue path jsonNumber acc
      | ("string" [@reason.raw_literal "string"]) ->
        resolvePathAndSetValue path (String (Js.to_string v) [@explicit_arity]) acc
      | ("object" [@reason.raw_literal "object"]) ->
        let type_ =
          Js.Unsafe.get v ("type" [@reason.raw_literal "type"]) |> Js.to_string
        in
        let size =
          Js.Unsafe.get v ("length" [@reason.raw_literal "length"])
          |> Js.float_of_number
          |> int_of_float
        in
        let jsonValue =
          match type_ = ("array" [@reason.raw_literal "array"]) with
          | true -> List (Array.make size (Int 0 [@explicit_arity])) [@explicit_arity]
          | false -> Assoc (Hashtbl.create size) [@explicit_arity]
        in
        resolvePathAndSetValue path jsonValue acc
      | unrecognisedString ->
        failwith (("got " [@reason.raw_literal "got "]) ^ unrecognisedString)
    in
    let head = List.hd ops in
    let v = Js.Unsafe.get head ("v" [@reason.raw_literal "v"]) in
    let init =
      match v |> Js.typeof |> Js.to_string with
      | ("number" [@reason.raw_literal "number"]) ->
        failwith ("non sense" [@reason.raw_literal "non sense"])
      | ("string" [@reason.raw_literal "string"]) ->
        failwith ("non sense" [@reason.raw_literal "non sense"])
      | ("object" [@reason.raw_literal "object"]) ->
        let type_ =
          Js.Unsafe.get v ("type" [@reason.raw_literal "type"]) |> Js.to_string
        in
        let size =
          Js.Unsafe.get v ("length" [@reason.raw_literal "length"])
          |> Js.float_of_number
          |> int_of_float
        in
        (match type_ == ("array" [@reason.raw_literal "array"]) with
        | true -> List (Array.make size (Obj.magic 0)) [@explicit_arity]
        | false -> Assoc (Hashtbl.create size) [@explicit_arity])
      | unrecognisedString ->
        failwith (("got " [@reason.raw_literal "got "]) ^ unrecognisedString)
    in
    List.fold_left f init ops
end

let _ =
  Js.export
    ("ocaml" [@reason.raw_literal "ocaml"])
    [%js
      object
        method compile ops =
          try
            ops
            |> Js.to_array
            |> Array.to_list
            |> Compile.run
            |> Compile.toString
            |> print_endline
          with
          | e -> print_endline (Printexc.to_string e)
      end]
