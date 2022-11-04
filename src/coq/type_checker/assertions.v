Set Implicit Arguments.
From Coq Require Import String List.

Import ListNotations.

From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import types.

Module Assertions.

    Inductive t : Set := 
    | Co_type_variable : string -> Types.t Types.S_type Types.C_poly -> t
    | Co_kind_variable : string -> Kinds.t -> t
    | Co_kind_bound_variable : string -> Kinds.t -> Types.t Types.S_type Types.C_poly -> t
    | Co_exist_marker : string -> t
    | Co_kind_inferable : string -> Kinds.t -> t
    | Co_kind_bound_inferable : string -> Kinds.t -> Types.t Types.S_type Types.C_mono -> t
    .

    Definition type_variable s r : t := Co_type_variable s r.
    Definition kind_variable s r : t := Co_kind_variable s r.
    Definition kind_bound_variable s k r : t := Co_kind_bound_inferable s k r.
    Definition exist_marker s : t := Co_exist_marker s.
    Definition kind_inferable s k : t := Co_kind_inferable s k.
    Definition kind_bound_inferable s k r : t := Co_kind_bound_inferable s k r.

    Definition fold {A} (a:t) {type_variable} {kind_variable} {kind_bound_variable} 
                              {exist_marker} {kind_inferable} {kind_bound_inferable} : A :=
        match a with
        | Co_type_variable s t => type_variable s t
        | Co_kind_variable s t => kind_variable s t
        | Co_kind_bound_variable s k t => kind_bound_variable s k t
        | Co_exist_marker s => exist_marker s
        | Co_kind_inferable s k => kind_inferable s k
        | Co_kind_bound_inferable s k t => kind_bound_inferable s k t
        end.

    Definition domain (a:t) : list string :=        
        fold a 
            (type_variable := fun n _ => [n]) 
            (kind_variable := fun n _ => [n]) 
            (kind_bound_variable := fun n _ _ => [n])
            (exist_marker := fun _ => [])
            (kind_inferable := fun n _ => [n])
            (kind_bound_inferable := fun n _ _ => [n])
        .

    Definition Get_kind (a:t) (v:string) : option Kinds.t :=
        fold a
            (type_variable := fun _ _ => None) 
            (kind_variable := fun n k =>  if string_dec n v then Some k else None)
            (kind_bound_variable := fun n k _ => if string_dec n v then Some k else None)
            (exist_marker := fun _ => None)
            (kind_inferable := fun n k => if string_dec n v then Some k else None)
            (kind_bound_inferable := fun n k _ => if string_dec n v then Some k else None)
        .


End Assertions.

(**
 * TODO:
 * - Define defaulted catamorphisms
 *)