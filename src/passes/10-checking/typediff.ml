(**
    This modules aims at finding the difference between two large tuple types
    for improved error message during type mismatch.
    For example when type [a * b * c * d * e] cannot unify with [c * d * e],
    the module will find the "diff" between the two, [a * b] here,
    so the message can be augmented into :
    > Cannot unify a * b * c * d * e * y * z with c * d * f * y * z
    > Diff :
    - a
    - b
      c
      d
    - e
    + f
      y
      z

    This module is based on the [Diffing] library.
    It will try to find the simplest diff between the two lists.
    To find the simplest one, we tell it how costly is a change.

    For example,
      from :  a  b  c  d  e
      to :    a  b  c  e
    The most trivial patch is :
      patch 1 : (keep a) (keep b) (keep c) (REMOVE D) (keep e)
    But another possible patch is :
      patch 2 : (keep a) (keep b) (keep c) (REPLACE d BY e) (REMOVE e)

    For the first  patch, cost = 1 REMOVE = 1
    For the second patch, cost = 1 REPLACE + 1 REMOVE = 2
    weight patch 1 < weight patch 2, so the algorithm will prefer patch 1.

    Weights will be used to construct a "cost matrix" to find the lightest patch,
    see : https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm

    ---

    In above example, the weight of a INSERT/DELETE/CHANGE was assumed to always be 1,
    but all changes are not created equal.
    In below example, there is a [string] added, and a [nat->int] change in the big tuple.
      from :          tuple1=(int * tez * int * nat)
      to   : string * tuple2=(int * tez * nat * nat) 
              ^^^^^^                       ^^^
    Here, if all changes are weighted 1, then the diff would be :
      patch 1 : (CHANGE tuple1 to STRING), (ADD tuple2)
    However, we would rather like the following patch :
      patch 2 : (ADD string), (CHANGE tuple1 to tuple2)
    
    More generally, in those cases when two big tuples t1 and t2 are similar, we prefer a (CHANGE t1 to t2).
    To do this, we account for the type_expression in the computation of the weight. For example :
      1. weight of (INSERT  int)                     = 1
      2. weight of (INSERT  nat * int * tez)         = 3
      3. weight of (CHANGE  string TO int)           = 1
      4. weight of (CHANGE  int    * int * tez
                        TO  string * nat * string)   = 3
                            ^^^^^^   ^^^   ^^^^^^
      5. weight of (CHANGE  int * int * tez
                        TO  int * int * nat)         = 1
                                        ^^^
    In this last example (5.), the only real change is [tez] -> [nat], so its weight is 1.
    More generally :
      * For singleton types, weights of changes is 1
      * For tuples, weight of INSERT / DELETE is the length of the tuple
      * Weight of CHANGE tuple_a to tuple_b is the number of changes to do within the two tuples (see example 5. above)
*)

open Ligo_prim
open Simple_utils

module Defs = struct
  type left = Type.t
  type right = Type.t
  type eq = unit
  type diff = unit
  type state = unit
end

module Define = Diffing.Define (Defs)

module Size = struct
  let of_row (row : Type.row) = Record.LMap.cardinal row.fields

  let of_type (t : Type.t) =
    match t.content with
    | T_record row -> of_row row
    | _ -> 1
end
(* of Size *)

module rec Arg : sig
  val weight : Define.change -> int
  val test : unit -> Type.t -> Type.t -> (unit, unit) result
  val update : Define.change -> unit -> unit
end = struct
  let rec weight : Define.change -> int = function
    | Delete t | Insert t -> Size.of_type t
    | Keep _ -> 0
    | Change (type1, type2, _) ->
      (match type1.content, type2.content with
      (* We consider the weight to change a record into another
        as the weight of the diff between them,
        so that "close" records are gather together in the diff *)
      | T_record row1, T_record row2 ->
        let patch = Patch.of_rows row1 row2 in
        let diff_weights = List.map ~f:(fun change -> weight change) patch in
        let total_weight = List.fold ~init:0 ~f:( + ) diff_weights in
        total_weight
      (* one single insertion + n removals (or the contrary) *)
      | T_record row, _ | _, T_record row -> 1 + Size.of_row row
      (* one single insertion + one signle removal *)
      | _ -> 1 + 1)


  let test : Defs.state -> Defs.left -> Defs.right -> (Defs.eq, Defs.diff) result =
   fun _state type1 type2 ->
    match Type.equal type1 type2 with
    | true -> Ok ()
    | false -> Error ()


  let update : Define.change -> Defs.state -> Defs.state = fun _change _state -> ()
end
(* of Arg *)

(* The [Patch] module will compute the minimal list of changes
   between two lists of type_expressions,
   using the above computation of "weights" of type_expression changes. *)
and Patch : sig
  val of_rows : Type.row -> Type.row -> Define.patch
end = struct
  (* generates the [diff] function :
     val diff : unit -> Type.t array -> Type.t array -> Define.patch *)
  include Define.Simple (Arg)

  (* Note : When converting from list to arrays and vice versa,
     we have to use List.rev to get the diff in the right order.
     For example :   [int; nat] vs [int; tez]
     should give :   [KEEP int; REPLACE nat BY tez]
     and not         [REPLACE nat BY tez; KEEP int] *)
  let of_rows (row1 : Type.row) (row2 : Type.row) : Define.patch =
    let type_of_row_elt (e : Type.row_element) = e.associated_type in
    let tarray_of_row (row : Type.row) : Type.t array =
      row.fields
      |> Record.to_list
      |> List.map ~f:snd
      |> List.map ~f:type_of_row_elt
      |> List.rev
      |> Array.of_list
    in
    List.rev @@ diff () (tarray_of_row row1) (tarray_of_row row2)
end
(* of Patch *)

type t =
  | Diff_change of Define.change (* single change *)
  | Diff_patch of Define.patch (* list of changes (like a classic diff) *)
  | Diff_record of t Record.t
(* a record of diff for diffing records
                                     rdiff[key] = diff r1[key] r2[key] *)

let rec diff : Type.t -> Type.t -> t =
 fun type1 type2 ->
  let self = diff in
  match type1.content, type2.content with
  (* ---- Tuples (unlabeled records) --------------------- *)
  (* Call the [Diffing] to get the optimal diff *)
  | T_record row1, T_record row2
    when Record.is_tuple row1.fields && Record.is_tuple row2.fields ->
    let patch = Patch.of_rows row1 row2 in
    Diff_patch patch
  (* ---- Records (labeled ones, not tuples) ------------- *)
  (* We build a diff record dr, where dr[key] = diff of r1[key] r2[key] *)
  | T_record row1, T_record row2 ->
    let type_of_row_elt (e : Type.row_element) = e.associated_type in
    let r1 : Type.t Record.t = Record.map row1.fields ~f:type_of_row_elt in
    let r2 : Type.t Record.t = Record.map row2.fields ~f:type_of_row_elt in
    let lmap_merge : Label.t -> Type.t option -> Type.t option -> t option =
     fun _key t1_opt t2_opt ->
      match t1_opt, t2_opt with
      (* This case shouldn't happen in the record merge *)
      | None, None -> None
      (* If only the first record has an entry at this label,
           it means it's been "deleted", from the record diff persepective *)
      | Some t1, None -> Some (Diff_change (Delete t1))
      (* ... and vice-versa *)
      | None, Some t2 -> Some (Diff_change (Insert t2))
      (* Otherwise, dr[key] = diff of r1[key] vs. r2[key] *)
      | Some t1, Some t2 -> Some (self t1 t2)
    in
    let record : t Record.t = Record.LMap.merge lmap_merge r1 r2 in
    Diff_record record
  (* TODO : Add Variant types *)
  | _ -> Diff_change (Change (type1, type2, ()))


(* The [ANSI] module uses Ocaml Format's semantic tags to enable styling of output.
   After calling [add_ansi_marking] on the given [ppf],
   you can add colored text by enclosing it with a semantic tag, like this :
      [Format.fprintf ppf "normal text, @{<red>some red text@}, normal text again"

   The module will basically surround "some red text"
   with [mark_open_stag "red"] string output as prefix
   and [mark_close_stag "red"] string output as suffix.

   See :
    https://ocamlpro.com/blog/2020_06_01_fr_tutoriel_format
    https://hal.archives-ouvertes.fr/hal-01503081/file/format-unraveled.pdf
*)
module ANSI = struct
  type style =
    | Normal
    | Red
    | Green

  let style_of_stag = function
    | Format.String_tag s ->
      (match s with
      | "normal" -> Normal
      | "red" -> Red
      | "green" -> Green
      | _ -> failwith "Unknown ANSI style" (* TODO NP : How to report errors properly ? *))
    | _ -> failwith "Unknown ANSI semantic tag"


  (* TODO NP : How to report errors properly ? *)

  let closing_style = function
    | Red | Green | Normal -> Normal


  let code_of_style = function
    | Normal -> 0
    | Red -> 31
    | Green -> 32


  let ansi_of_code code = Format.sprintf "\027[%dm" code
  let ansi_of_style style = ansi_of_code @@ code_of_style style
  let mark_open_stag t = ansi_of_style @@ style_of_stag t
  let mark_close_stag t = ansi_of_style @@ closing_style @@ style_of_stag t

  let add_ansi_marking ppf =
    let open Format in
    pp_set_mark_tags ppf true;
    let old_fs = pp_get_formatter_stag_functions ppf () in
    pp_set_formatter_stag_functions ppf { old_fs with mark_open_stag; mark_close_stag }
end
(* ANSI *)

module PP = struct
  let pp_list_newline pp_content ppf content =
    PP_helpers.list_sep pp_content (PP_helpers.tag "@,") ppf content


  let _pp_te_debug ppf (type_ : Type.t) : unit =
    Format.fprintf ppf "%a <hash:%d>" Type.pp type_ (Type.hash type_)


  let pp_type ~tbl ppf (t : Type.t) =
    Format.fprintf ppf "%a" (Type.pp_with_name_tbl ~tbl) t


  let rec pp_change ~tbl ppf (c : Define.change) : unit =
    let self = pp_change ~tbl in
    match c with
    | Delete l -> Format.fprintf ppf "@{<red>- %a@}" (pp_type ~tbl) l
    | Insert r -> Format.fprintf ppf "@{<green>+ %a@}" (pp_type ~tbl) r
    | Keep (l, _r, _eq) -> Format.fprintf ppf "  %a" (pp_type ~tbl) l
    | Change (l, r, _diff) -> pp_list_newline self ppf [ Delete l; Insert r ]


  let pp_patch ~tbl ppf (patch : Define.patch) =
    match patch with
    | [] -> Format.fprintf ppf ""
    | _ -> Format.fprintf ppf "@[<v>%a@]" (pp_list_newline (pp_change ~tbl)) patch


  let pp_record ~tbl (pp : Format.formatter -> t -> unit) ppf (r : t Record.t) =
    let pp_type = pp_type ~tbl in
    let _pp_type_prepend_field_name field_name ppf (t : Type.t) =
      Format.fprintf ppf "<field '%s'> %a" field_name pp_type t
    in
    let pp_record_entry (label : Label.t) (diff : t) =
      let label = Label.to_string label in
      match diff with
      | Diff_change (Delete t) ->
        Format.fprintf ppf "@[@{<red>- field '%s' : %a@}@]," label pp_type t
      | Diff_change (Insert t) ->
        Format.fprintf ppf "@[@{<green>+ field '%s' : %a@}@," label pp_type t
      | _ -> Format.fprintf ppf "@[<v 2>  field '%s' :@,%a@]@," label pp diff
    in
    Record.LMap.iter pp_record_entry r


  let pp_diff ~(no_color : bool) ~tbl ppf (diff : t) : unit =
    let rec aux ppf (diff : t) : unit =
      let self = aux in
      if not no_color then ANSI.add_ansi_marking ppf;
      match diff with
      | Diff_change c -> pp_change ~tbl ppf c
      | Diff_patch p -> pp_patch ~tbl ppf p
      | Diff_record r -> pp_record ~tbl self ppf r
    in
    match diff with
    | Diff_change _ -> () (* Trivially short diff (int vs. nat) aren't displayed *)
    | _ -> Format.fprintf ppf "@,@[<v>Difference between the types:@,%a@]" aux diff
end
(* of PP *)

let pp = PP.pp_diff
