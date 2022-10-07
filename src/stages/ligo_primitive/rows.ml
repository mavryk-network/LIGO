module type Attr = sig
  type t [@@deriving eq, compare, yojson, hash]
end

module Make (Map : Record.Map) (Attr : Attr) (Annot : Annotation.S) = struct
  module Elem = struct
    type 'a t = 'a content Annot.t

    and 'a content =
      { associated_type : 'a
      ; decl_pos : int [@hash.ignore]
      }
    [@@deriving eq, compare, yojson, hash]

    let map_content f { associated_type; decl_pos } =
      { associated_type = f associated_type; decl_pos }


    let map f t = Annot.map (map_content f) t
    let fold_content f init { associated_type; _ } = f init associated_type
    let fold f init t = Annot.fold (fold_content f) init t

    let fold_map_content f init { associated_type; decl_pos } =
      let acc, associated_type = f init associated_type in
      acc, { associated_type; decl_pos }


    let fold_map f init t = Annot.fold_map (fold_map_content f) init t
  end

  type 'a fields = 'a Elem.t Map.t [@@deriving eq, compare, yojson, hash]

  let map_fields f fields = Map.map (Elem.map f) fields
  let fold_fields f init fields = Map.fold f init fields
  let fold_map_fields f init fields = Map.fold_map f init fields

  type 'a t =
    { fields : 'a Elem.t Map.t
    ; attributes : Attr.t
    }
  [@@deriving eq, compare, yojson, hash]

  let map f { fields; attributes } =
    { fields = Map.map (Elem.map f) fields; attributes }


  let fold f init { fields; _ } = Map.fold f init fields

  let fold_map f init { fields; attributes } =
    let acc, fields = Map.fold_map f init fields in
    acc, { fields; attributes }


  let pp ppa ppf { fields; _ } =
    let pp_bindings ppf bindings =
      Simple_utils.PP_helpers.(
        list_sep
          (fun ppf (label, elem) ->
            Format.fprintf
              ppf
              "@[<hv>%a -> %a@]"
              Label.pp
              label
              ppa
              (Annot.content elem).Elem.associated_type)
          (tag " ,@ "))
        ppf
        bindings
    in
    Format.fprintf ppf "@[<hv 7>[%a]@]" pp_bindings (Map.to_alist fields)
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

  let to_alist t = Map.to_alist t
end)