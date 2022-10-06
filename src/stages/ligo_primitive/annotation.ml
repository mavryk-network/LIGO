module type S = sig
  type 'a t [@@deriving eq, compare, yojson, hash, map, fold]

  val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
  val content : 'a t -> 'a
end

module None : S with type 'a t = 'a = struct
  type 'a t = 'a [@@deriving eq, compare, yojson, hash, map, fold]

  let fold_map f init t = f init t
  let content t = t
end

module Attr : sig
  type 'a t =
    { content : 'a
    ; attributes : string list
    }

  val create : ?attributes:string list -> 'a -> 'a t

  include S with type 'a t := 'a t
end = struct
  type 'a t =
    { content : 'a
    ; attributes : string list
    }
  [@@deriving eq, compare, hash, yojson, map, fold]

  let content { content; _ } = content

  let fold_map f init { content; attributes } =
    let acc, content = f init content in
    acc, { content; attributes }


  let create ?(attributes = []) content = { content; attributes }
end

module Michelson : sig
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