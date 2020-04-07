open Trace
open Display

let error_ppformat : display_format:string display_format ->
  Format.formatter -> [> error ] -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with 
    | `Simple_error s ->
      let i' = Format.asprintf " TODO simple error : %s" s in 
      Format.pp_print_string f i'
    | `Sys_error _ ->
      let i' = Format.asprintf " TODO sys error" in 
      Format.pp_print_string f i'
    | `Different_literals_types i ->
      let i' = Format.asprintf " TODO error : %i" i in 
      Format.pp_print_string f i'
  )

let error_jsonformat : [> error] -> J.t = fun a ->
  match a with
  | `Simple_error s -> `Assoc [("simple_error", `String s)] 
  | `Sys_error _ -> `Assoc [("sys_error", `String "todo")]

(* let rec error_pp ?(dev = false) out (e : error) =
  let open JSON_string_utils in
  let message =
    let opt = e |> member "message" |> string in
    match opt with
    | Some msg -> ": " ^ msg
    | None -> "" in
  let error_code =
    let error_code = e |> member "error_code" in
    match error_code with
    | `Null -> ""
    | _ -> " (" ^ (J.to_string error_code) ^ ")" in
  let title =
    let opt = e |> member "title" |> string in
    Option.unopt ~default:"" opt in
  let data =
    let data = e |> member "data" in
    match data with
    | `Null -> ""
    | _ -> " " ^ (J.to_string data) ^ "\n" in
  let infos =
    let infos = e |> member "infos" in
    match infos with
    | `List lst -> lst
    | `Null -> []
    | x -> [ x ] in
  let children =
    let infos = e |> member "children" in
    match infos with
    | `List lst -> lst
    | `Null -> []
    | x -> [ x ] in
  let location =
    let opt = e |> member "data" |> member "location" |> string in
    let aux cur prec =
      match prec with
      | None -> cur |> member "data" |> member "location" |> string
      | Some s -> Some s
    in
    match List.fold_right aux infos opt with
    | None -> ""
    | Some s -> s ^ ". "
  in
  let print x = Format.fprintf out x in
  if not dev then (
    print "%s%s%s%s%s" location title error_code message data
  ) else (
    print "%s%s%s.\n%s%s\n%a\n%a\n" title error_code message data location
      (Format.pp_print_list (error_pp ~dev)) infos
      (Format.pp_print_list (error_pp ~dev)) children
  ) *)

let error_format : [> error] Display.format = {
  pp = error_ppformat;
  to_json = error_jsonformat;
}