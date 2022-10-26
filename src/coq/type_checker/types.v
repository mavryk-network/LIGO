Set Implicit Arguments.

From Coq Require Import String.

From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import rows.

Module Types.

    Inductive type_classifier : Set :=
    | C_poly : type_classifier
    | C_mono : type_classifier
    .

    Inductive t : type_classifier -> Set := 
    (* Variables and basic types *)
    | Ty_exist {A} : string -> t A
    | Ty_variable {A} : string -> t A
    | Ty_one {A} : t A
    (* Function, universal quantification and application *)
    | Ty_arrow {A} : t A -> t A -> t A
    | Ty_for_all : string -> Kinds.t -> t C_poly -> t C_poly
    | Ty_lambda {A} : string -> Kinds.t -> t A -> t A
    | Ty_apply {A} : t A -> t A -> t A
    (* Algebraic type construction *)
    | Ty_product {A} : Rows.t (t A) -> t A
    | Ty_sum {A} : Rows.t (t A) -> t A
    .

    Definition exist {A} s : t A := Ty_exist s.
    Definition var {A} s : t A := Ty_variable s.
    Definition one {A} : t A := Ty_one.
    Definition arrow {A} l r : t A := Ty_arrow l r.
    Definition for_all s k l : t C_poly := Ty_for_all s k l.
    Definition lambda {A} s k l : t A := Ty_lambda s k l.
    Definition apply {A} l r : t A := Ty_apply l r.
    Definition product {A} r : t A := Ty_product r.
    Definition sum {A} r : t A := Ty_sum r.

    Definition fold {A B} (v:t A) {exist} {var} {one} {arrow} {for_all} {lambda} {apply} {product} {sum} : B :=
        match v with
        | Ty_exist s => exist s
        | Ty_variable v => var v
        | Ty_one => one tt
        | @Ty_arrow A l r => @arrow A l r
        | Ty_for_all s k l => for_all s k l
        | @Ty_lambda A s k l => @lambda A s k l
        | @Ty_apply A l r => @apply A l r
        | @Ty_product A r => @product A r
        | @Ty_sum A r => @sum A r
        end.

End Types.

(**
 * TODO:
 * - Define defaulted catamorphisms
 *)