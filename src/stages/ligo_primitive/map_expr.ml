type 'a t = ('a * 'a) list
[@@deriving eq, compare, yojson, hash, sexp, map, fold]

let fold_map f init t =
  List.fold_map t ~init ~f:(fun acc (a1, a2) ->
      let acc, a1 = f acc a1 in
      let acc, a2 = f acc a2 in
      acc, (a1, a2))


let pp' str f ppf m =
  let assoc ppf : 'a * 'a -> unit =
   fun (a, b) -> Format.fprintf ppf "%a -> %a" f a f b
  in
  Format.fprintf ppf "%s[%a]" str Simple_utils.PP_helpers.(list_sep_d assoc) m


let pp f ppf = pp' "map" f ppf
let pp_big_map f ppf = pp' "big_map" f ppf
