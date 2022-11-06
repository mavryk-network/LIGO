Set Implicit Arguments.

From Coq Require Import List String.

From ligo_coq_type_checker Require Import kinds.

Import ListNotations.

Module Types.

    Inductive sort : Set :=
        | S_type: sort
        .

    Inductive classifier : Set :=
        | C_poly : classifier
        | C_mono : classifier
        .

    Inductive t : sort -> classifier -> Set := 
        (* Variables and basic types *)
        | Ty_exist {C} : string -> t S_type C
        | Ty_variable {C} : string -> t S_type C
        | Ty_one {C} : t S_type C
        (* Function, universal quantification and application *)
        | Ty_arrow {C} : t S_type C -> t S_type C -> t S_type C
        | Ty_for_all : string -> Kinds.t -> t S_type C_poly -> t S_type C_poly
        | Ty_lambda {C} : string -> Kinds.t -> t S_type C -> t S_type C
        | Ty_apply {C} : t S_type C -> t S_type C -> t S_type C
        (* Algebraic type construction *)
        | Ty_product {C} : t_row C -> t S_type C
        | Ty_sum {C} : t_row C -> t S_type C

    with t_row : classifier -> Set := 
        (* Row type *)    
        | R_empty {C} : t_row C
        | R_multi {C} : string -> t S_type C -> t_row C -> t_row C.

    Definition t_type : classifier -> Set := t S_type.

    Definition exist {C} s : t_type C := Ty_exist s.
    Definition var {C} s : t_type C := Ty_variable s.
    Definition one {C} : t_type C := Ty_one.
    Definition arrow {C} l r : t_type C := Ty_arrow l r.
    Definition for_all s k l : t_type C_poly := Ty_for_all s k l.
    Definition lambda {C} s k l : t_type C := Ty_lambda s k l.
    Definition apply {C} l r : t_type C := Ty_apply l r.
    Definition product {C} r : t_type C := Ty_product r.
    Definition sum {C} r : t_type C := Ty_sum r.

    Definition empty {C} : t_row C := R_empty.
    Definition row {C} s v r : t_row C := R_multi s v r.

    Definition fold {S C}
        {B : sort -> classifier -> Type} (v:t S C) 
        {exist: forall (C:classifier), string -> B S_type C} 
        {var: forall (C:classifier), string -> B S_type C} 
        {one: forall (C:classifier), B S_type C} 
        {arrow: forall (C:classifier), t S_type C -> t S_type C -> B S_type C} 
        {for_all: string -> Kinds.t -> t S_type C_poly -> B S_type C_poly}
        {lambda: forall (C:classifier), string -> Kinds.t -> t S_type C -> B S_type C} 
        {apply: forall (C:classifier), t S_type C -> t S_type C -> B S_type C} 
        {product: forall (C:classifier), t_row C -> B S_type C} 
        {sum: forall (C:classifier), t_row C -> B S_type C} 
        : B S C :=
        match v with
        | @Ty_exist C s => exist C s
        | @Ty_variable C v => var C v
        | @Ty_one C => one C
        | @Ty_arrow C l r => arrow C l r
        | Ty_for_all s k l => for_all s k l
        | @Ty_lambda C s k l => lambda C s k l
        | @Ty_apply C l r => apply C l r
        | @Ty_product C r => product C r
        | @Ty_sum C r => sum C r
        end.

    Definition fold_row {C} {B : classifier -> Type} (v:t_row C) 
        {empty: forall (C : classifier), B C} 
        {row: forall (C : classifier), string -> t S_type C -> t_row C -> B C} 
        : B C :=
        match v with        
        | @R_empty C => empty C 
        | @R_multi C s v r => row C s v r
        end.

    Fixpoint domain {C} (v:t_row C) : list string :=
        fold_row v
            (empty:=fun _ => [])
            (row:=fun _ s _ r => s :: (domain r)).

End Types.
