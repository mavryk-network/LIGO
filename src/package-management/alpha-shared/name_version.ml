open Package_management_external_libs
module Semver = Ligo_semver

type version =
  | SemverVersion of Semver.t
  | StringVersion of string

type t =
  { name : string
  ; version : version
  }

let compare a b =
  match String.compare a.name b.name with
  | 0 ->
    (match a.version, b.version with
    | SemverVersion a, SemverVersion b -> Semver.compare a b
    | StringVersion a, StringVersion b -> String.compare a b
    (* In case of Node _, Default _ or Default _, Node _ what comes first is small by default *)
    | _, _ -> -1)
  | n -> n


let make ~name ~version = { name; version }

let is_root { version; _ } =
  match version with
  | StringVersion _ -> true
  | _ -> false


let is_default s =
  let f = function
    | Str.Delim _ -> true
    | Str.Text _ -> false
  in
  let regexp = Str.regexp "./ligo.json" in
  let elements = Str.full_split regexp s in
  let contains_default_string =
    List.map ~f elements |> List.fold ~init:false ~f:(fun init v -> init || v)
  in
  if contains_default_string then Some s else None


let version_of_string ~input_string version =
  match Semver.of_string version with
  | Some v -> Some (SemverVersion v)
  | None ->
    (match is_default input_string with
    | Some _ -> Some (StringVersion version)
    | None -> None)


let version_to_string = function
  | SemverVersion s -> Semver.to_string s
  | StringVersion s -> s


let to_string { name; version } =
  match version with
  | SemverVersion version ->
    Printf.sprintf "%s@%s@ffffffff" name @@ Semver.to_string version
  | StringVersion version -> Printf.sprintf "%s@%s" name version


let of_string input_string =
  match String.split_on_chars ~on:[ '@' ] input_string with
  | [ ""; package_name_with_slash; version; _unused ] ->
    let name = Printf.sprintf "@%s" package_name_with_slash in
    (match version_of_string ~input_string version with
    | Some version -> Ok { name; version }
    | None ->
      let msg_str = Printf.sprintf "Couldn't parse version in \"%s\"" input_string in
      let msg = `Msg msg_str in
      Error msg)
  | [ name; version; _unused ] ->
    (match version_of_string ~input_string version with
    | Some version -> Ok { name; version }
    | None ->
      (* Note : This where default like -> "@ligo/dao-jsligo@link-dev:./ligo.json" goes and why we pass input_string *)
      let msg_str = Printf.sprintf "Couldn't parse version in \"%s\"" input_string in
      let msg = `Msg msg_str in
      Error msg)
  | [ name; version ] ->
    (match version_of_string ~input_string version with
    | Some version -> Ok { name; version }
    | None ->
      let msg_str = Printf.sprintf "Couldn't parse version in \"%s\"" input_string in
      let msg = `Msg msg_str in
      Error msg)
  | _ ->
    let msg_str = Printf.sprintf "Couldn't parse \"%s\"" input_string in
    let msg = `Msg msg_str in
    Error msg


let to_yojson v = `String (to_string v)

let of_yojson = function
  | `String s ->
    (match of_string s with
    | Error (`Msg m) -> Error m
    | Ok v -> Ok v)
  | y ->
    let msg_str = Printf.sprintf "Expected a string. Got %s" (Yojson.Safe.to_string y) in
    Error msg_str
