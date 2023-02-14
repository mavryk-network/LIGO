type 'a t =
  { ty_binder : Var.Type_var.t
  ; kind : Kind.t
  ; type_ : 'a
  }
[@@deriving eq, compare, hash, fold, map, sexp]
(* Lambda (a : kind). term *)

let to_yojson v_to_yojson { ty_binder; kind; type_ } =
  `List [ Var.Type_var.to_yojson ty_binder; Kind.to_yojson kind; v_to_yojson type_ ]


let of_yojson v_of_yojson (yojson : Yojson.Safe.t) =
  match yojson with
  | `List [ ty_binder_yojson; kind_yojson; v_yojson ] ->
    (match
       ( Var.Type_var.of_yojson ty_binder_yojson
       , Kind.of_yojson kind_yojson
       , v_of_yojson v_yojson )
     with
    | Ok ty_binder, Ok kind, Ok type_ -> Ok { ty_binder; kind; type_ }
    | _ -> Error "Failed to parse abstract.t")
  | _ -> Error "Failed to parse abstract.t"


let pp_forall f ppf { ty_binder; kind; type_ } : unit =
  Format.fprintf ppf "∀ %a : %a . %a" Var.Type_var.pp ty_binder Kind.pp kind f type_


let pp_type_abs f ppf { ty_binder; kind; type_ } : unit =
  Format.fprintf ppf "funtype %a : %a . %a" Var.Type_var.pp ty_binder Kind.pp kind f type_
