Set Implicit Arguments.
From Coq Require Import String List.

(* Naive rows representation *)

Module Rows.

    (* Looks like non epmty list *)
    Inductive t (A:Set) : Set := 
    | T_single : string * A -> t A
    | T_rows : string * A -> t A -> t A
    .

    Definition single {A} p : t A := T_single p.
    Definition rows {A} p b : t A := T_rows p b.

End Rows.

(**
 * TODO:
 * - Define catamorphisms and defaulted catamorphisms
 *)