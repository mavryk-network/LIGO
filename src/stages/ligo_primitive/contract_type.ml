open Var

type 't t =
  { storage : 't
  ; views : 't View_type.t Value_var.Map.t
  ; entry_points : 't Entry_type.t Value_var.Map.t
  }
[@@deriving equal, compare, sexp, yojson, hash]

let pp pp_t ppf { storage; views; entry_points } =
  (* Edge cases (prints empty lines when views or entrypoints is empty). *)
  let pp_map ~keyword ~f ppf views =
    Map.iteri views ~f:(fun ~key:view_var ~data:view_type ->
        Format.fprintf
          ppf
          "@[%s %a : %a@]@;"
          keyword
          Value_var.pp
          view_var
          (f pp_t)
          view_type)
  in
  let pp_views = pp_map ~keyword:"view" ~f:View_type.pp in
  let pp_entry_points = pp_map ~keyword:"entry" ~f:Entry_type.pp in
  Format.fprintf
    ppf
    "@[<v>{@ storage = %a;@;%a@;%a}@]"
    pp_t
    storage
    pp_views
    views
    pp_entry_points
    entry_points


let map f { storage; views; entry_points } =
  { storage = f storage
  ; views = Map.map views ~f:(View_type.map f)
  ; entry_points = Map.map entry_points ~f:(Entry_type.map f)
  }


let iter f { storage; views; entry_points } =
  f storage;
  Map.iter views ~f:(View_type.iter f);
  Map.iter entry_points ~f:(Entry_type.iter f)


let fold f init { storage; views; entry_points } =
  let init = f init storage in
  let init =
    Map.fold views ~init ~f:(fun ~key:_ ~data:view_type acc ->
        View_type.fold f acc view_type)
  in
  let init =
    Map.fold entry_points ~init ~f:(fun ~key:_ ~data:entry_type acc ->
        Entry_type.fold f acc entry_type)
  in
  init
