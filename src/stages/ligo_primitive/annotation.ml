module type S = sig
  type 'a t [@@deriving eq, compare, yojson, hash, map, fold]

  val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
  val content : 'a t -> 'a
end

module No_annot : S with type 'a t = 'a = struct
  type 'a t = 'a [@@deriving eq, compare, yojson, hash, map, fold]

  let fold_map f init t = f init t
  let content t = t
end

module Michelson_annot : sig
  type 'a t =
    { content : 'a
    ; michelson_annotation : string option
    }

  val create : ?michelson_annotation:string -> 'a -> 'a t

  include S with type 'a t := 'a t
end = struct
  type 'a t =
    { content : 'a
    ; michelson_annotation : string option
    }
  [@@deriving eq, compare, hash, yojson, map, fold]

  let content { content; _ } = content

  let fold_map f init { content; michelson_annotation } =
    let acc, content = f init content in
    acc, { content; michelson_annotation }


  let create ?michelson_annotation content = { content; michelson_annotation }
end