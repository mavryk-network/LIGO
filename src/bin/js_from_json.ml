open Js_of_ocaml

type 'value op =
  { path : string
  ; v : 'value
  }

let debugEnabled = false
let debug msg = if debugEnabled then print_endline msg
let isInteger : Js.number Js.t -> bool Js.t = Js.Unsafe.js_expr "Number.isInteger"

module Path : sig
  type t

  val toString : t -> string
  val ofString : string -> t

  type component =
    [ `Root
    | `ArrayIndex of int
    | `AssocKey of string
    ]

  val descend : f:('a -> component -> 'a) -> init:'a -> t -> 'a
  val root : t -> component option
  val base : t -> t option
  val last : t -> component option
  val componentOfString : string -> component
  val componentToString : component -> string
end = struct
  type component =
    [ `Root
    | `ArrayIndex of int
    | `AssocKey of string
    ]

  type t = component list

  let componentOfString str =
    let regexp = Str.regexp "\\[\\([0-9]+\\)\\]" in
    match str, Str.string_match regexp str 0 with
    | "", _ -> `Root
    | _, true ->
      let index_str = Str.matched_group 1 str in
      (try `ArrayIndex (index_str |> int_of_string) with
      | _ -> failwith "For some reason, matched [...] pattern didn't get an integer?")
    | _, false -> `AssocKey str


  let componentToString = function
    | `ArrayIndex i -> Printf.sprintf "[%s]" (string_of_int i)
    | `Root -> ""
    | `AssocKey k -> k


  let ofString input = String.split_on_char '.' input |> List.map componentOfString
  let toString input = input |> List.map componentToString |> String.concat "."

  let root = function
    | [] -> None
    | [ `Root ] -> None
    | `Root :: a :: rest -> Some a
    | _ -> None


  let base = function
    | [] -> None
    | [ `Root ] -> None
    | `Root :: rest ->
      let skipLast l =
        let counter = ref (-1) in
        let f item acc =
          counter := !counter + 1;
          if !counter = 0 then [] else item :: acc
        in
        List.fold_right f l []
      in
      Option.some @@ (`Root :: skipLast rest)
    | _ -> None


  let last = function
    | [] -> None
    | [ `Root ] -> None
    | `Root :: rest ->
      let last l =
        let counter = ref (-1) in
        let f item acc =
          counter := !counter + 1;
          if !counter = 0 then Some item [@explicit_arity] else acc
        in
        List.fold_right f l None
      in
      last rest
    | _ -> None


  let rec descend ~f ~init path =
    match path with
    | `Root :: rest -> descend ~f ~init rest
    | hd :: rest ->
      let init = f init hd in
      descend ~f ~init rest
    | [] -> init
end

module Compile = struct
  type json =
    | Int of int
    | Bool of bool
    | Null
    | Float of float
    | String of string
    | List of json array
    | Assoc of (string, json) Hashtbl.t

  let rec toYojson = function
    | ((Int i) [@explicit_arity]) -> `Int i
    | ((Float f) [@explicit_arity]) -> `Float f
    | Null -> `Null
    | ((Bool s) [@explicit_arity]) -> `Bool s
    | ((String s) [@explicit_arity]) -> `String s
    | ((List arr) [@explicit_arity]) -> `List (arr |> Array.map toYojson |> Array.to_list)
    | ((Assoc tbl) [@explicit_arity]) ->
      let f k v acc = (k, toYojson v) :: acc in
      `Assoc (Hashtbl.fold f tbl [])


  let rec ofYojson = function
    | `Int i -> Int i
    | `Bool b -> Bool b
    | `Null -> Null
    | `String s -> String s
    | `Float f -> Float f
    | `List l -> List (l |> List.map ofYojson |> Array.of_list)
    | `Assoc kvPairs ->
      let f acc (k, v) =
        Hashtbl.add acc k (ofYojson v);
        acc
      in
      Assoc (List.fold_left f (Hashtbl.create 20) kvPairs)


  let rec toString (v : json) : string = v |> toYojson |> Yojson.Safe.to_string

  let resolvePathAndSetValue path json acc =
    let f acc component =
      match acc, component with
      | ((Some ((List accArray) [@explicit_arity])) [@explicit_arity]), `ArrayIndex i ->
        (try Some accArray.(i) [@explicit_arity] with
        | Invalid_argument _ -> None)
      | ((Some ((Assoc tbl) [@explicit_arity])) [@explicit_arity]), `Root ->
        Hashtbl.find_opt tbl ""
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
          ^ " json: "
          ^ toString json
          ^ " last: "
          ^ string_of_int lastComponent)
      | ( ((Some ((Assoc tbl) [@explicit_arity])) [@explicit_arity])
        , ((Some (`AssocKey lastComponent)) [@explicit_arity]) ) ->
        debug ("Setting path: " ^ Path.toString basePath ^ " last: " ^ lastComponent);
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
        failwith ("lastComponent is None: " ^ Path.toString fullPath ^ " " ^ toString json)
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
          ^ " json: "
          ^ toString json
          ^ " last: "
          ^ Path.componentToString component)
      | None, _ -> failwith ("Path doesn't exist: " ^ Path.toString fullPath))
    | None -> acc


  let json_key = "json_so_far"
  let file_index_key = "fileIndex"
  let op_index_key = "opIndex"

  let with_store f =
    let storage = Dom_html.window##.localStorage in
    let f s =
      (* anything useful goes here *)
      f s
    in
    Js.Optdef.map storage f


  let store (json : json) (file_index : int) (op_index : int) : unit =
    ignore
    @@ with_store (fun s ->
           s##setItem (Js.string json_key) (toString json |> Js.string);
           s##setItem (Js.string file_index_key) (string_of_int file_index |> Js.string);
           s##setItem (Js.string op_index_key) (string_of_int op_index |> Js.string))


  let store_get () : json option * int * int =
    let r =
      with_store (fun s ->
          ( (match Js.Opt.to_option @@ s##getItem (Js.string json_key) with
            | Some str_json ->
              let str = str_json |> Js.to_string in
              print_endline str;
              (try
                 let json : Yojson.Basic.t = Yojson.Basic.from_string str in
                 Option.some @@ ofYojson json
               with
              | e ->
                print_endline @@ Printexc.to_string e;
                None)
            | None -> None)
          , (match Js.Opt.to_option @@ s##getItem (Js.string file_index_key) with
            | None -> 0
            | Some v -> v |> Js.to_string |> int_of_string)
          , match Js.Opt.to_option @@ s##getItem (Js.string op_index_key) with
            | None -> 0
            | Some v -> v |> Js.to_string |> int_of_string ))
    in
    match Js.Optdef.to_option r with
    | None -> failwith "No localStorage"
    | Some v -> v


  let run ops =
    let f acc item =
      let path = Js.Unsafe.get item "path" |> Js.to_string in
      let v = Js.Unsafe.get item "v" in
      match v |> Js.typeof |> Js.to_string with
      | "number" ->
        let n = Js.float_of_number v in
        let jsonNumber =
          match isInteger v |> Js.to_bool with
          | true -> Int (n |> int_of_float) [@explicit_arity]
          | false -> Float n [@explicit_arity]
        in
        resolvePathAndSetValue path jsonNumber acc
      | "string" ->
        resolvePathAndSetValue path (String (Js.to_string v) [@explicit_arity]) acc
      | "object" ->
        let type_ = Js.Unsafe.get v "type" |> Js.to_string in
        let size = Js.Unsafe.get v "length" |> Js.float_of_number |> int_of_float in
        let jsonValue =
          match type_ = "array" with
          | true -> List (Array.make size (Int 0 [@explicit_arity])) [@explicit_arity]
          | false -> Assoc (Hashtbl.create size) [@explicit_arity]
        in
        resolvePathAndSetValue path jsonValue acc
      | unrecognisedString -> failwith ("got " ^ unrecognisedString)
    in
    let head = List.hd ops in
    let v = Js.Unsafe.get head "v" in
    let init =
      match v |> Js.typeof |> Js.to_string with
      | "number" -> failwith "non sense"
      | "string" -> failwith "non sense"
      | "object" ->
        let type_ = Js.Unsafe.get v "type" |> Js.to_string in
        let size = Js.Unsafe.get v "length" |> Js.float_of_number |> int_of_float in
        (match type_ = "array" with
        | true -> List (Array.make size (Int 0 [@explicit_arity])) [@explicit_arity]
        | false -> Assoc (Hashtbl.create size) [@explicit_arity])
      | unrecognisedString -> failwith ("got " ^ unrecognisedString)
    in
    List.fold_left f init ops


  let acc_ref : json option ref =
    match store_get () with
    | Some json, _, _ -> ref (Some json)
    | None, _, _ -> ref None


  let incremental op file_index op_index =
    let path = Js.Unsafe.get op "path" |> Js.to_string in
    let v = Js.Unsafe.get op "v" in
    match !acc_ref with
    | None ->
      (match v |> Js.typeof |> Js.to_string with
      | "number" -> failwith "non sense"
      | "string" -> failwith "non sense"
      | "object" ->
        let type_ = Js.Unsafe.get v "type" |> Js.to_string in
        let size = Js.Unsafe.get v "length" |> Js.float_of_number |> int_of_float in
        (match type_ = "array" with
        | true ->
          acc_ref
            := Option.some
               @@ (List (Array.make size (Int 0 [@explicit_arity])) [@explicit_arity])
        | false ->
          acc_ref := Option.some @@ (Assoc (Hashtbl.create size) [@explicit_arity]))
      | unrecognisedString -> failwith ("got " ^ unrecognisedString))
    | Some acc ->
      let json =
        match v |> Js.typeof |> Js.to_string with
        | "number" ->
          let n = Js.float_of_number v in
          let jsonNumber =
            match isInteger v |> Js.to_bool with
            | true -> Int (n |> int_of_float) [@explicit_arity]
            | false -> Float n [@explicit_arity]
          in
          resolvePathAndSetValue path jsonNumber acc
        | "string" ->
          resolvePathAndSetValue path (String (Js.to_string v) [@explicit_arity]) acc
        | "object" ->
          let type_ = Js.Unsafe.get v "type" |> Js.to_string in
          let size = Js.Unsafe.get v "length" |> Js.float_of_number |> int_of_float in
          let jsonValue =
            match type_ = "array" with
            | true -> List (Array.make size (Int 0 [@explicit_arity])) [@explicit_arity]
            | false -> Assoc (Hashtbl.create size) [@explicit_arity]
          in
          resolvePathAndSetValue path jsonValue acc
        | unrecognisedString -> failwith ("got " ^ unrecognisedString)
      in
      acc_ref := Option.some @@ json;
      if op_index mod 1000 = 0 then store json file_index op_index


  let serializeYojson () : string =
    match !acc_ref with
    | None -> failwith "acc_ref is None. incremental() hasn't been run yet"
    | Some acc -> toString acc
end

let _ =
  Js.export
    "ocaml"
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

        method compileIncremental op fileIndex opIndex =
          let file_index = fileIndex |> Js.float_of_number |> int_of_float in
          let op_index = opIndex |> Js.float_of_number |> int_of_float in
          try Compile.incremental op file_index op_index with
          | e -> print_endline (Printexc.to_string e)

        method toString () =
          try () |> Compile.serializeYojson |> Js.string with
          | e ->
            print_endline (Printexc.to_string e);
            Js.string ""

        method readJsonFromLocalStorage () =
          match Compile.store_get () with
          | Some json, file_index, op_index ->
            print_endline @@ Compile.toString json;
            Printf.printf "Having read file no %d and op no %d" file_index op_index
          | None, _, _ -> print_endline "No json found in localStorage"
      end]
