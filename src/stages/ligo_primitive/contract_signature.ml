module PP_helpers = Simple_utils.PP_helpers
open Var

type 'type_ t = 'type_ decl list

and 'type_ decl =
  | C_type of
      { binder : Type_var.t
      ; type_ : 'type_
      }
  | C_entry of
      { binder : Value_var.t
      ; entry_type : 'type_ Entry_type.t
      }
  | C_view of
      { binder : Value_var.t
      ; view_type : 'type_ View_type.t
      }
[@@deriving equal, compare, yojson, hash, map, fold]

let fold_map_decl f init decl =
  match decl with
  | C_type { binder; type_ } ->
    let res, type_ = f init type_ in
    res, C_type { binder; type_ }
  | C_entry { binder; entry_type } ->
    let res, entry_type = Entry_type.fold_map f init entry_type in
    res, C_entry { binder; entry_type }
  | C_view { binder; view_type } ->
    let res, view_type = View_type.fold_map f init view_type in
    res, C_view { binder; view_type }


let fold_map f init t = List.fold_map t ~init ~f:(fold_map_decl f)

let pp_decl pp_type ppf decl =
  match decl with
  | C_type { binder; type_ } ->
    Format.fprintf ppf "@[type %a = %a@]" Type_var.pp binder pp_type type_
  | C_entry { binder; entry_type } ->
    Format.fprintf
      ppf
      "@[entry %a : %a@]"
      Value_var.pp
      binder
      (Entry_type.pp pp_type)
      entry_type
  | C_view { binder; view_type } ->
    Format.fprintf
      ppf
      "@[view %a : %a@]"
      Value_var.pp
      binder
      (View_type.pp pp_type)
      view_type


let pp pp_type ppf t =
  Format.fprintf
    ppf
    "@[<hv>sig@;%aend@]"
    (PP_helpers.list_sep (pp_decl pp_type) (fun ppf () -> Format.fprintf ppf "@;"))
    t


let get_contract_storage t =
  List.find_map t ~f:(function
      | C_type { binder; type_ } when Type_var.is_name binder "storage" -> Some type_
      | _ -> None)


let to_contract_type t =
  let open Result.Let_syntax in
  let%bind storage =
    Result.of_option (get_contract_storage t) ~error:`Undefined_storage
  in
  let entry_points, views =
    List.fold
      t
      ~init:Value_var.Map.(empty, empty)
      ~f:(fun ((entry_points, views) as acc) item ->
        match item with
        | C_entry { binder = var; entry_type } ->
          Map.set entry_points ~key:var ~data:entry_type, views
        | C_view { binder = var; view_type } ->
          entry_points, Map.set views ~key:var ~data:view_type
        | _ -> acc)
  in
  if Map.is_empty entry_points
  then Error `No_entry_point
  else return Contract_type.{ storage; views; entry_points }


let of_contract_type ~loc Contract_type.{ storage; views; entry_points } =
  [ C_type { binder = Type_var.of_input_var ~loc "storage"; type_ = storage } ]
  @ (views
    |> Map.to_alist
    |> List.map ~f:(fun (binder, view_type) -> C_view { binder; view_type }))
  @ (entry_points
    |> Map.to_alist
    |> List.map ~f:(fun (binder, entry_type) -> C_entry { binder; entry_type }))
