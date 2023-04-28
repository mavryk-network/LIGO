open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
open Unit_test_helpers
module Location = Simple_utils.Location

(* Upon exported declaration 'export', attribute "public" must be added *)

let compile =
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_export d -> d_attr ~loc (Attribute.{ key = "public"; value = None }, d)
    | d -> make_d ~loc d
  in
  `Cata { idle_cata_pass with declaration }


let decompile =
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_attr (Attribute.{ key = "public"; value = None }, d) -> d_export ~loc d
    | d -> make_d ~loc d
  in
  `Cata { idle_cata_pass with declaration }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_export _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise ~syntax:_ =
  morph ~name:__MODULE__ ~compile ~decompile ~reduction_check:(reduction ~raise)


let%expect_test _ =
  Declaration.(
    {|
      (D_export (DECLARATION1))
    |} |-> pass ~raise;
    [%expect {|
      (D_attr (((key public)) (DECLARATION1)))
    |}];
    {|
      (D_attr (((key public)) (DECLARATION1)))
    |} <-| pass ~raise;
    [%expect {|
      (D_export (DECLARATION1))
    |}])
