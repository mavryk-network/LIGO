open Ast_unified
open Pass_type
open Errors
open Simple_utils.Trace
module Location = Simple_utils.Location

(* Apply the following heuristic for patterns:
   - empty tuple is unit when no type annotation provided
   - ellipsed tuple is a list
*)
include Flag.No_arg ()

let valid_pattern (ps : pattern Ellipsis_pattern.t)
    : [> `List of pattern List.Ne.t | `Tuple of pattern list ] option
  =
  let open Ellipsis_pattern in
  match List.rev ps with
  | [] -> None
  | _ :: rest when List.exists rest ~f:(fun x -> x.ellipsis) -> None
  | last :: _ ->
    let ps = ps |> List.map ~f:(fun x -> x.pattern) in
    Some (if last.ellipsis then `List (List.Ne.of_list ps) else `Tuple ps)


let compile ~raise =
  let default_fold, default_unfold =
    (* propagation across sorts *)
    let default_fold, default_unfold =
      default_refold_acc ~plus:(fun _ _ -> false) ~init:false
    in
    (* propagation across patterns *)
    let default_fold_pattern = (Morphing.default_fold ( || ) false).pattern in
    { default_fold with pattern = default_fold_pattern }, default_unfold
  in
  (* tag annotated pattern and it's sub patterns *)
  let tag_annotated_patterns p =
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_typed ((ty, _), (p, _sub)) -> p_typed ~loc ty p, true
    | P_tuple_with_ellipsis _ ->
      let p, sub_flags = default_fold.pattern p in
      ignore sub_flags;
      p, false
    | _ -> default_fold.pattern p
  in
  let heuristics (p, is_annotated) =
    let loc = get_p_loc p in
    let return x = default_unfold.pattern (x, is_annotated) in
    match get_p p with
    | P_tuple_with_ellipsis [] when not is_annotated -> return @@ p_unit ~loc
    | P_tuple_with_ellipsis [] when is_annotated -> return @@ p_list ~loc (List [])
    | P_tuple_with_ellipsis ps ->
      (match valid_pattern ps with
      | Some (`Tuple l) -> return @@ p_tuple ~loc l
      | Some (`List l) ->
        return @@ List.Ne.fold_right1 l ~f:(fun p q -> p_list ~loc (Cons (p, q)))
      | None -> raise.error (invalid_list_pattern_match loc))
    | p -> return @@ make_p ~loc p
  in
  Refold_acc
    ( { default_fold with pattern = tag_annotated_patterns }
    , { default_unfold with pattern = heuristics } )


let name = __MODULE__
let decompile ~raise:_ = Nothing

let reduction ~raise =
  { Iter.defaults with
    pattern =
      (function
      | { wrap_content = P_tuple_with_ellipsis _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


open Unit_test_helpers.Pattern

let%expect_test "empty tuple with annotation" =
  {|
    (P_typed
      (TY_EXPR)
      (P_tuple_with_ellipsis ()))
  |} |-> compile;
  [%expect {|
    (P_typed (TY_EXPR) (P_list (List ()))) |}]

let%expect_test "empty tuple with annotation higher" =
  {|
    (P_typed
      (TY_EXPR)
      (P_tuple
        ((PATTERN1)
         (P_tuple_with_ellipsis ()))))
  |}
  |-> compile;
  [%expect {|
    (P_typed (TY_EXPR) (P_tuple ((PATTERN1) (P_list (List ()))))) |}]

let%expect_test "empty tuple" =
  {| (P_tuple_with_ellipsis ()) |} |-> compile;
  [%expect {| P_unit |}]

let%expect_test "tuple with ellipses" =
  {|
    (P_tuple_with_ellipsis
      (((ellipsis false) (pattern (PATTERN1)))
       ((ellipsis false) (pattern (PATTERN2)))
       ((ellipsis true)  (pattern (PATTERN3)))))
  |}
  |-> compile;
  [%expect {|
    (P_list (Cons (PATTERN1) (P_list (Cons (PATTERN2) (PATTERN3))))) |}]

let%expect_test "tuple without ellipses" =
  {|
    (P_tuple_with_ellipsis
      (((ellipsis false) (pattern (PATTERN1)))
       ((ellipsis false) (pattern (PATTERN2)))
       ((ellipsis false) (pattern (PATTERN3)))))
  |}
  |-> compile;
  [%expect {|
    (P_tuple ((PATTERN1) (PATTERN2) (PATTERN3))) |}]