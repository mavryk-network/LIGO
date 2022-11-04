Set Implicit Arguments.
From Coq Require Import String List.

From ligo_coq_type_checker Require Import assertions.
From ligo_coq_type_checker Require Import kinds.

Import ListNotations.

Module Context.

    Inductive t : Set := 
    | Co_empty : t
    | Co_assertion : t -> Assertions.t -> t
    .
    
    Definition empty : t := Co_empty.
    Definition assertion c a : t := Co_assertion c a.
    
    Definition fold {A} (c:t) {empty} {assertion} : A := 
        match c with
        | Co_empty => empty tt
        | Co_assertion c a => assertion c a
        end.

    (**
     * TODO:
     * - Define defaulted catamorphisms
     *)

    Fixpoint domain (c:t) : list string :=
        fold c 
            (empty  := fun _ => []) 
            (assertion := fun c a => domain c ++ Assertions.domain a).

    Definition In_domain (c:t) (v:string) : Prop :=
        In v (domain c).

    Fixpoint Find_kind (c:t) (v:string) : option Kinds.t := 
        fold c
            (empty  := fun _ => None) 
            (assertion := fun c a => 
                match Assertions.Get_kind a v with
                | Some v => Some v
                | None => Find_kind c v
                end
            ).

End Context.

Module Context_Properties.

    Lemma in_empty_domain : 
        forall (v:string), 
        Context.In_domain Context.empty v = False.
    Proof.
        simpl. 
        reflexivity.
    Qed.

    (* Really basic lemma *)
    Lemma in_non_empty_domain : 
        forall (v:string), forall (c:Context.t),
        Context.domain c = [v] -> Context.In_domain c v.
    Proof.
        intros.
        unfold Context.In_domain.
        rewrite H.
        intuition. (* i.e. simpl. left. reflexivity. *)
    Qed.

End Context_Properties.
