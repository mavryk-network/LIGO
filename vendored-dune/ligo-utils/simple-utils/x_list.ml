open Core
include List

let rec remove n = function
  | [] -> raise (Failure "List.remove")
  | _ :: tl when n = 0 -> tl
  | hd :: tl -> hd :: remove (n - 1) tl

let set_nth new_i l new_v =
  mapi ~f:(fun old_i old_v -> if old_i = new_i then new_v else old_v) l

let rec remove_element ?compare:cmp x lst =
  let compare = Option.value ~default:Caml.compare cmp in
  match lst with
  | [] -> raise (Failure "List.remove_element")
  | hd :: tl when compare x hd = 0 -> tl
  | hd :: tl -> hd :: remove_element ~compare x tl

let repeat n x = init ~f:(fun _ -> x) n

let to_pair = function
  | [ a; b ] -> Some (a, b)
  | _ -> None

let to_singleton = function
  | [ a ] -> Some a
  | _ -> None

let fold_map2_exn ~f ~init a b =
  fold_map ~f:(fun init (a, b) -> f init a b) ~init @@ zip_exn a b

let rec fold_map_right ~f ~init = function
  | [] -> init, []
  | hd :: tl ->
    let init, tl = fold_map_right ~f ~init tl in
    let init, hd = f init hd in
    init, hd :: tl

let uncons = function
  | [] -> None
  | hd :: tl -> Some (hd, tl)

let repeat x n =
  let rec aux n xs = if n <= 0 then xs else aux (n - 1) (x :: xs) in
  aux n []

let zip_opt a b =
  match List.zip a b with
  | Or_unequal_lengths.Ok x -> Some x
  | Or_unequal_lengths.Unequal_lengths -> None

let rec deoptionalize xs =
  match xs with
  | [] -> Some []
  | None :: _ -> None
  | Some x :: xs ->
    let xs = deoptionalize xs in
    (match xs with
    | None -> None
    | Some xs -> Some (x :: xs))

(** {|
map_result ~f [x1; ...; xn] = let%bind y1 = f x1 in
                              ...
                              let%bind yn = f xn in
                              [yn; ...; y1] |} *)
let rev_map_result ~f l =
  let open Result.Let_syntax in
  List.fold_result
    ~init:[]
    ~f:(fun rev_tl' hd ->
      let%bind hd' = f hd in
      Ok (hd' :: rev_tl'))
    l

(** [find_first_dup_pair comparator l] searches for a pair of elements [(x1, x2)] (at distinct positions) in [l] such that [compare x1 x2 = 0] (and [x1] appears before [x2] in the list). If such pairs exist, it returns [Some (x1, x2)] where [(x1, x2)] is the first such pair (meaning that there is a prefix of the list in which it is the only such pair), and otherwise, it returns [None]. *)
let find_first_dup_pair (type k) ~compare l =
  let module Elt : Set_intf.Elt_plain with type t = k = struct
    type t = k

    let compare = compare
    let sexp_of_t = sexp_of_opaque
  end
  in
  let module EltSet = Set.Make_plain (Elt) in
  (* Invariant: there exists [l_seen] such that:
     - [l = List.append l_seen l_unseen];
     - [seen = EltSet.of_list l_seen]; and
     - [List.contains_dup ~compare l_seen = false] (or equivalently, [List.length l_seen = EltSet.length seen]). *)
  let rec aux l_unseen seen =
    match l_unseen with
    | [] -> None
    | hd :: tl ->
      (match Set.binary_search ~compare seen `First_equal_to hd with
      | Some x -> Some (x, hd)
      | None -> aux tl (EltSet.add seen hd))
  in
  aux l EltSet.empty

module Ne = struct
  (** Non-empty lists *)

  type 'a t = 'a * 'a list [@@deriving eq, compare, yojson, hash, sexp, fold]

  let sexp_of_t (f : 'a -> Sexp.t) ((hd, tl) : 'a t) : Sexp.t = List.sexp_of_t f (hd :: tl)

  let t_of_sexp : type a. (Sexp.t -> a) -> Sexp.t -> a t =
   fun f x ->
    match x with
    | Atom _ -> f x, []
    | List lst ->
      let lst = List.map ~f lst in
      List.hd_exn lst, List.tl_exn lst

  let unzip ((hd, tl) : _ t) =
    let a, b = hd
    and la, lb = unzip tl in
    (a, la), (b, lb)

  let of_list lst = List.hd_exn lst, List.tl_exn lst (* TODO: Remove *)

  let of_list_opt = function
    | [] -> None
    | x :: xs -> Some (x, xs)

  let to_list ((hd, tl) : _ t) = hd :: tl
  let singleton hd : 'a t = hd, []

  let last ((hd, tl) : _ t) =
    match tl with
    | [] -> hd
    | _ -> List.last_exn tl

  let hd : 'a t -> 'a = fst
  let cons : 'a -> 'a t -> 'a t = fun hd' (hd, tl) -> hd', hd :: tl

  let iter f ((hd, tl) : _ t) =
    f hd;
    List.iter ~f tl

  let map f ((hd, tl) : _ t) = f hd, List.map ~f tl
  let fold_left ~f ~init ((hd, tl) : _ t) = List.fold_left ~f ~init:(f init hd) tl

  let rec fold_right1 ~f ((hd, tl) : _ t) =
    match tl with
    | [] -> hd
    | hdtl :: tltl -> f hd @@ fold_right1 ~f (hdtl, tltl)

  let fold_right ~f ~init ((hd, tl) : _ t) = f hd (List.fold_right ~f ~init tl)

  let fold_map ~f ~init ((hd, tl) : _ t) =
    let init, hd = f init hd in
    let init, tl = List.fold_map ~f ~init tl in
    init, (hd, tl)

  let hd_map : _ -> 'a t -> 'a t = fun f (hd, tl) -> f hd, tl

  let mapi f ((hd, tl) : _ t) =
    let lst = List.mapi ~f (hd :: tl) in
    of_list lst

  let concat ((hd, tl) : _ t) = hd @ List.concat tl
  let rev (lst : _ t) = of_list @@ List.rev @@ to_list lst

  let find_map f ((hd, tl) : _ t) =
    match f hd with
    | Some x -> Some x
    | None -> find_map ~f tl

  let append : 'a t -> 'a t -> 'a t =
   fun (hd, tl) (hd', tl') -> hd, List.append tl @@ (hd' :: tl')

  let length (_, tl) = 1 + List.length tl

  (* [head_permute] [lst] : if lst == [A ; B ; C] returns [A ; B ; C] [B ; C ; A] [C ; A ; B] *)
  let head_permute : 'a t -> 'a t t =
   fun lst ->
    let rec aux (acc : 'a t t) (lst : 'a t) =
      if length acc = length lst
      then acc
      else (
        let hd, tl = lst in
        (* match lst with *)
        (* | [] -> raise (Failure "List.head_permute") *)
        (* | hd::tl -> *)
        let p = append (of_list tl) (singleton hd) in
        aux (cons p acc) p)
    in
    rev @@ aux (singleton lst) lst

  let deoptionalize : 'a option t -> 'a t option =
   fun lst ->
    let hd, tl = lst in
    match hd with
    | None -> None
    | Some hd ->
      let tl = deoptionalize tl in
      (match tl with
      | None -> None
      | Some tl -> Some (hd, tl))

  let rev_map_result ~f l =
    let open Result.Let_syntax in
    let rec loop hd tl rev_l' =
      let%bind hd' = f hd in
      match tl with
      | [] -> Ok (hd', rev_l')
      | hd2 :: tl2 -> loop hd2 tl2 (hd' :: rev_l')
    in
    let hd0, tl0 = l in
    loop hd0 tl0 []

  let find_first_dup_pair ~compare (hd, tl) = find_first_dup_pair ~compare (hd :: tl)
end
