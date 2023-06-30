(* Type definitions for all the CSTs *)

(* Vendor dependencies *)

module Utils  = Simple_utils.Utils
module Region = Simple_utils.Region

(* Lists *)

type ('a,'sep) nsep_or_term = [
  `Sep  of ('a,'sep) Utils.nsepseq
| `Term of ('a * 'sep) Utils.nseq
]

type ('a,'sep) sep_or_term = ('a,'sep) nsep_or_term option

type ('a,'sep) nsep_or_pref = [
  `Sep  of ('a,'sep) Utils.nsepseq
| `Pref of ('sep * 'a) Utils.nseq
]

let nsep_or_term_to_list = function
  `Sep  s -> Utils.nsepseq_to_list s
| `Term s -> List.map ~f:fst (Utils.nseq_to_list s)

let sep_or_term_to_list = function
  None -> []
| Some seq -> nsep_or_term_to_list seq

let nsep_or_pref_to_list = function
  `Sep s -> Utils.nsepseq_to_list s
| `Pref (hd,tl) -> List.map ~f:snd (hd::tl)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd,tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)
