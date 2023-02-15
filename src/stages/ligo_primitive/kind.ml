type t =
  | Type
  | Singleton
  | Arrow of t * t
  | Contract
[@@deriving yojson, equal, compare, hash, sexp]

let rec pp ppf t =
  match t with
  | Type -> Format.fprintf ppf "*"
  | Singleton -> Format.fprintf ppf "singleton"
  | Contract -> Format.fprintf ppf "contract"
  | Arrow (t1, t2) -> Format.fprintf ppf "%a -> %a" pp t1 pp t2
