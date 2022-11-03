Set Implicit Arguments.

From Coq Require Import String.

From ligo_coq_type_checker Require Import kinds.

Module Types.

    Inductive sort : Set :=
        | S_type: sort
        .

    Inductive type_classifier : Set :=
        | C_poly : type_classifier
        | C_mono : type_classifier
        .

    Inductive t : sort -> type_classifier -> Set := 
        (* Variables and basic types *)
        | Ty_exist {A} : string -> t S_type A
        | Ty_variable {A} : string -> t S_type A
        | Ty_one {A} : t S_type A
        (* Function, universal quantification and application *)
        | Ty_arrow {A} : t S_type A -> t S_type A -> t S_type A
        | Ty_for_all : string -> Kinds.t -> t S_type C_poly -> t S_type C_poly
        | Ty_lambda {A} : string -> Kinds.t -> t S_type A -> t S_type A
        | Ty_apply {A} : t S_type A -> t S_type A -> t S_type A
        (* Algebraic type construction *)
        | Ty_product {A} : t_row A -> t S_type A
        | Ty_sum {A} : t_row A -> t S_type A

    with t_row : type_classifier -> Set := 
        (* Row type *)    
        | R_single {A} : string -> t S_type A -> t_row A
        | R_multi {A} : string -> t S_type A -> t_row A -> t_row A.

    Definition t_type : type_classifier -> Set := t S_type.

    Definition exist {A} s : t_type A := Ty_exist s.
    Definition var {A} s : t_type A := Ty_variable s.
    Definition one {A} : t_type A := Ty_one.
    Definition arrow {A} l r : t_type A := Ty_arrow l r.
    Definition for_all s k l : t_type C_poly := Ty_for_all s k l.
    Definition lambda {A} s k l : t_type A := Ty_lambda s k l.
    Definition apply {A} l r : t_type A := Ty_apply l r.
    Definition product {A} r : t_type A := Ty_product r.
    Definition sum {A} r : t_type A := Ty_sum r.

    Definition single {A} s v : t_row A := R_single s v.
    Definition row {A} s v r : t_row A := R_multi s v r.

    Definition fold {R A B} (v:t R A) {exist} {var} {one} {arrow} {for_all} {lambda} {apply} {product} {sum} : B :=
        match v with
        | Ty_exist s => exist s
        | Ty_variable v => var v
        | Ty_one => one tt
        | @Ty_arrow A l r => arrow A l r
        | Ty_for_all s k l => for_all s k l
        | @Ty_lambda A s k l => lambda A s k l
        | @Ty_apply A l r => apply A l r
        | @Ty_product A r => product A r
        | @Ty_sum A r => sum A r
        end.

    Definition fold_row {A B} (v:t_row A) {single} {row} : B :=
        match v with        
        | @R_single A s v => single A s v
        | @R_multi A s v r => row A s v r
        end.

End Types.

(**
 * TODO:
 * - Define defaulted catamorphisms
 *)