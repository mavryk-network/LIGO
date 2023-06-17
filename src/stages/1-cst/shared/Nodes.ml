(* Type definitions for all the CSTs *)

(* Vendor dependencies *)

module Utils = Simple_utils.Utils

(* Lists *)

type ('a, 'sep) sep_or_term = [
  `Sep  of ('a, 'sep) Utils.sepseq
| `Term of ('a * 'sep) list
]

type ('a, 'sep) nsep_or_term = [
  `NSep  of ('a, 'sep) Utils.nsepseq
| `NTerm of ('a * 'sep) Utils.nseq
]

type ('a, 'sep) nsep_or_pref = [
  `NSep of ('a, 'sep) Utils.nsepseq
| `Pref of ('sep * 'a) Utils.nseq
]

let sep_or_term_to_list = function
  `Sep  s -> Utils.sepseq_to_list s
| `Term s -> List.map ~f:fst s

let nsep_or_term_to_list = function
  `NSep  s -> Utils.nsepseq_to_list s
| `NTerm s -> List.map ~f:fst (Utils.nseq_to_list s)

let nsep_or_pref_to_list = function
  `NSep s -> Utils.nsepseq_to_list s
| `Pref (hd,tl) -> List.map ~f:snd (hd::tl)
