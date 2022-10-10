module T = struct
  type t = Label of string [@@deriving eq, compare, yojson, hash, sexp]
end

include T

let of_string str = Label str
let to_string (Label str) = str
let of_int i = string_of_int i |> of_string
let pp ppf (l : t) : unit = Format.fprintf ppf "%s" (to_string l)
let range i j = List.map ~f:(fun i -> Label (string_of_int i)) @@ List.range i j

include Comparable.Make (T)

module Map = struct
  include Map

  let to_yojson a_to_yojson t : Yojson.Safe.t =
    Map.to_alist t |> [%to_yojson: (T.t * a) list]


  let of_yojson a_of_yojson yojson : (_ Map.t, _) Result.t =
    let open Result.Let_syntax in
    let%bind alist = [%of_yojson: (T.t * a) list] yojson in
    match Map.of_alist alist with
    | `Duplicate_key _label -> Error "Duplicate label"
    | `Ok map -> Ok map


  let hash_fold_t (type a) (hash_fold_a : a Hash.folder) state t =
    Map.to_alist t |> [%hash_fold: (T.t * a) list] state


  (* Special module for deriving code *)
  module Deriving = struct
    type nonrec 'a t = 'a t

    let equal = equal
    let compare = compare
    let hash_fold_t = hash_fold_t
    let to_yojson = to_yojson
    let of_yojson = of_yojson
    let map f t = map t ~f

    let fold_map f init t =
      fold t ~init:(init, empty) ~f:(fun ~key ~data (acc, t) ->
          let acc, data = f acc data in
          acc, set t ~key ~data)


    let fold f init t = fold t ~init ~f:(fun ~key:_ ~data acc -> f acc data)
  end

  let fold_map t ~init ~f = Deriving.fold_map f init t
end
