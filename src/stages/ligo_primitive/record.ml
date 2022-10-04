module type Map = sig
  type 'a t [@@deriving eq, compare, yojson, hash, map, fold]

  val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
  val to_alist : 'a t -> (Label.t * 'a) list
end

module Make (Map : Map) = struct
  type 'a t = 'a Map.t [@@deriving eq, compare, yojson, hash, map, fold]

  let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t =
    Map.fold_map


  let pp ppa ppf t =
    let pp_bindings ppf bindings =
      Simple_utils.PP_helpers.(
        list_sep
          (fun ppf (label, a) ->
            Format.fprintf ppf "@[<hv>%a -> %a@]" Label.pp label ppa a)
          (tag " ,@ "))
        ppf
        bindings
    in
    Format.fprintf ppf "@[<hv 7>record[%a]@]" pp_bindings (Map.to_alist t)
end

module List = Make (struct
  type 'a t = (Label.t * 'a) list
  [@@deriving eq, compare, yojson, hash, map, fold]

  let fold_map f init t =
    List.fold_map t ~init ~f:(fun acc (label, data) ->
        let acc, data = f acc data in
        acc, (label, data))


  let to_alist t = t
end)

module Map = Make (struct
  include Label.Map.Deriving

  let to_alist t = Label.Map.to_alist t
end)