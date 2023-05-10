(**
  The goal of this pass is to do :
  {[
    P_App        (P_ctor lbl1) (P_App      (P_ctor lbl2)  p)
    |->
    P_variant            lbl1  (P_variant          lbl2   p)
  ]}

  Here we do the same transformation as [ Constructor_application.pass ]
  does for expressions : 
  {[
    E_ctor_app            (E_constr lbl)          [p1; p2; p3; p4]
    |->
    E_applied_constructor.          lbl  (E_Tuple [p1; p2; p3; p4])
  ]}

  {i Note :}
    For expression, there is an additional
    [ Standalone_constructor_removal.pass  ] which transforms
    the standalone [E_constr] (i.e. not wrapped in a [E_ctor_app])
    into [E_Constructor.]
  
    For patterns, there are no standalone [P_ctor] so there is no need
    for such a pass. And the absence of [P_ctor] is checked directly
    in this pass' reduction check.
*)

open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

let compile ~syntax =
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_app (c, args) ->
      (match get_p c with
      | P_ctor c
        when Label.(equal c (of_string "Unit"))
             && Option.is_none args
             && Syntax_types.equal syntax PascaLIGO ->
        (* should not be necessary in principle *)
        p_literal ~loc Literal_unit
      | P_ctor constructor -> p_variant ~loc constructor args
      | _ -> failwith "impossible: parsing invariant")
    | p -> make_p ~loc p
  in
  `Cata { idle_cata_pass with pattern }


let decompile =
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_variant (constructor, p_opt) -> p_app ~loc (p_ctor ~loc constructor) p_opt
    | p -> make_p ~loc p
  in
  `Cata { idle_cata_pass with pattern }


let reduction ~raise =
  { Iter.defaults with
    pattern =
      (function
      | { wrap_content = P_app ({ fp = { wrap_content = P_ctor _; _ } }, _); _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise ~syntax =
  morph
    ~name:__MODULE__
    ~compile:(compile ~syntax)
    ~decompile
    ~reduction_check:(reduction ~raise)
