module PP_helpers = Simple_utils.PP_helpers
open Var

type 'expr t =
  { contract : Contract_var.t
  ; address : 'expr
  ; method_ : Value_var.t
  ; params : 'expr list
  ; on_none : 'expr option
  }
[@@deriving equal, compare, yojson, hash, fold, map]

let fold_map f acc { contract; address; method_; params; on_none } =
  let acc, address = f acc address in
  let acc, params = List.fold_map params ~init:acc ~f in
  let acc, on_none =
    match on_none with
    | None -> acc, None
    | Some on_none ->
      let acc, on_none = f acc on_none in
      acc, Some on_none
  in
  acc, { contract; address; method_; params; on_none }


let pp pp_expr ppf { contract; address; method_; params; on_none } =
  match on_none with
  | None ->
    Format.fprintf
      ppf
      "@[(contract ( %a : %a )).%a %a@]"
      pp_expr
      address
      Contract_var.pp
      contract
      Value_var.pp
      method_
      PP_helpers.(list_sep pp_expr (tag "@,"))
      params
  | Some on_none ->
    Format.fprintf
      ppf
      "@[<v>try (contract ( %a : %a )).%a %a@;with Failure -> %a@]"
      pp_expr
      address
      Contract_var.pp
      contract
      Value_var.pp
      method_
      PP_helpers.(list_sep pp_expr (tag "@,"))
      params
      pp_expr
      on_none


module Entry = struct
  type ('expr, 'type_) t =
    { contract : Contract_var.t
    ; address : 'expr
    ; entry : Value_var.t
    ; param : 'expr
    ; tez : 'expr
    ; on_none : 'expr option
    ; entry_type : 'type_ Entry_type.t
    }
  [@@deriving equal, compare, yojson, hash, fold, map]

  let fold_map f g acc { contract; address; entry; param; tez; on_none; entry_type } =
    let acc, address = f acc address in
    let acc, param = f acc param in
    let acc, tez = f acc tez in
    let acc, on_none =
      match on_none with
      | None -> acc, None
      | Some on_none ->
        let acc, on_none = f acc on_none in
        acc, Some on_none
    in
    let acc, entry_type = Entry_type.fold_map g acc entry_type in
    acc, { contract; address; entry; param; tez; on_none; entry_type }


  let pp
      pp_expr
      pp_type
      ppf
      { contract = _; address; entry; param; tez; on_none; entry_type }
    =
    Format.fprintf
      ppf
      "@[(%a : %a).%a ~param:%a ~tez:%a ~on_none:%a@]"
      pp_expr
      address
      (Entry_type.pp pp_type)
      entry_type
      Value_var.pp
      entry
      pp_expr
      param
      pp_expr
      tez
      (PP_helpers.option pp_expr)
      on_none
end

module View = struct
  type ('expr, 'type_) t =
    { contract : Contract_var.t
    ; address : 'expr
    ; view : Value_var.t
    ; param : 'expr
    ; on_none : 'expr option
    ; view_type : 'type_ View_type.t
    }
  [@@deriving equal, compare, yojson, hash, fold, map]

  let fold_map f g acc { contract; address; view; param; on_none; view_type } =
    let acc, address = f acc address in
    let acc, param = f acc param in
    let acc, on_none =
      match on_none with
      | None -> acc, None
      | Some on_none ->
        let acc, on_none = f acc on_none in
        acc, Some on_none
    in
    let acc, view_type = View_type.fold_map g acc view_type in
    acc, { contract; address; view; param; on_none; view_type }


  let pp pp_expr pp_type ppf { contract = _; address; view; param; on_none; view_type } =
    Format.fprintf
      ppf
      "@[(%a : %a).%a ~param:%a ~on_none:%a@]"
      pp_expr
      address
      (View_type.pp pp_type)
      view_type
      Value_var.pp
      view
      pp_expr
      param
      (PP_helpers.option pp_expr)
      on_none
end
