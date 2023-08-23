open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
open Unit_test_helpers
module Location = Simple_utils.Location

(* Upon exported declaration 'export', attribute "public" must be added *)
include Flag.No_arg ()

let compile ~raise:_ =
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_export d -> d_attr ~loc (Attribute.{ key = "public"; value = None }, d)
    | d -> make_d ~loc d
  in
  let rec no_visibility
      :  (declaration, expr, ty_expr, pattern, mod_expr, sig_expr) declaration_content_
      -> bool
    =
   fun e ->
    match e with
    | D_attr (Attribute.{ key = "public" | "private"; value = _ }, _) | D_export _ ->
      false
    | D_attr (_, d) -> no_visibility (Location.unwrap d.fp)
    | _ -> true
  in
  let program_entry
      : (program_entry, declaration, instruction) program_entry_ -> program_entry
    =
   fun e ->
    match e with
    | PE_declaration d when no_visibility (Location.unwrap d.fp) ->
      let loc = Location.generated in
      pe_declaration (d_attr ~loc (Attribute.{ key = "private"; value = None }, d))
    | PE_export t -> pe_attr Attribute.{ key = "private"; value = None } t
    | e -> make_pe e
  in
  Fold { idle_fold with declaration; program_entry }


let decompile ~raise:_ =
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_attr (Attribute.{ key = "public"; value = None }, d) -> d_export ~loc d
    | d -> make_d ~loc d
  in
  Fold { idle_fold with declaration }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_export _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__

let%expect_test _ =
  Declaration.(
    {| (D_export (DECLARATION1)) |} |-> compile;
    [%expect {| (D_attr (((key public)) (DECLARATION1))) |}];
    {| (D_attr (((key public)) (DECLARATION1))) |} |-> decompile;
    [%expect {|
      (D_export (DECLARATION1))
    |}])
