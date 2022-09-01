type collect_type =
  | Map
  | Set
  | List
  | Any
  [@@deriving eq,compare,yojson,hash]

type 'e t = {
  fe_binder : Var.ValueVar.t * Var.ValueVar.t option ;
  collection : 'e ;
  collection_type : collect_type ;
  fe_body : 'e ;
  } [@@deriving eq,compare,yojson,hash]

let option_map ppf (k,v_opt) =
  match v_opt with
  | None   -> Format.fprintf ppf "%a" Var.ValueVar.pp k
  | Some v -> Format.fprintf ppf "%a -> %a" Var.ValueVar.pp k Var.ValueVar.pp v

let pp f ppf = fun {fe_binder; collection; fe_body; _} ->
  Format.fprintf ppf "for each %a in %a do %a"
    option_map fe_binder
    f collection
    f fe_body

let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc {fe_binder=_;collection;fe_body;collection_type=_} ->
  let acc = f acc collection in
  let acc = f acc fe_body in
  acc

let map
= fun f {fe_binder; collection; fe_body; collection_type} ->
  let collection = f collection in
  let fe_body    = f fe_body in
  {fe_binder; collection; fe_body ; collection_type}

let fold_map
= fun f acc {fe_binder; collection; fe_body ;  collection_type} ->
  let acc,collection = f acc collection in
  let acc,fe_body    = f acc fe_body in
  (acc, {fe_binder; collection; fe_body ; collection_type})
