Set Implicit Arguments.

From Coq Require Import String.

From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import rows.

Module Types.

    Inductive type_classifier : Set :=
    | C_poly : type_classifier
    | C_mono : type_classifier
    .

    Inductive t : forall (A:type_classifier), Set := 
    (* Variables and basic types *)
    | Ty_exist {A} : string -> t A
    | Ty_variable {A} : string -> t A
    | Ty_unit {A} : t A 
    (* Function, universal quantification and application *)
    | Ty_arrow {A} : t A -> t A -> t A
    | Ty_forall : string -> Kinds.t -> t C_poly -> t C_poly
    | Ty_lambda {A} : string -> Kinds.t -> t A -> t A
    | Ty_apply {A} : t A -> t A -> t A
    (* Algebraic type construction *)
    | Ty_product {A} : Rows.t (t A) -> t A
    | Ty_sum {A} : Rows.t (t A) -> t A
    .

    Definition exist {A} s : t A := Ty_exist s.
    Definition var {A} s : t A := Ty_variable s.
    Definition unit {A} : t A := Ty_unit.
    (* To be continued *)

End Types.

(**
 * TODO:
 * - Define catamorphisms and defaulted catamorphisms
 *)