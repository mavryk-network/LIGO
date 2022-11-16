Set Implicit Arguments.
From Coq Require Import String List.

From ligo_coq_type_checker Require Import options.
From ligo_coq_type_checker Require Import assertions.
From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import types.

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

    Fixpoint Concat (c:t) (v:t) : t :=
        fold v
            (empty := fun _ => c)
            (assertion := fun v a' => assertion (Concat c v) a').    

    Definition Add_first (c:t) (a:Assertions.t) : t :=
        Concat (assertion empty a) c.

    Fixpoint domain (c:t) : list string :=
        fold c 
            (empty  := fun _ => []) 
            (assertion := fun c a => domain c ++ Assertions.domain a).

    Definition In_domain (c:t) (v:string) : Prop :=
        In v (domain c).

    Fixpoint Find_assertion (c:t) (v:string) : option Assertions.t := 
        fold c
            (empty  := fun _ => None) 
            (assertion := fun c a => 
                Options.fold (Assertions.Get_type a v)
                    (some := fun _ => Some a)
                    (none := fun _ => Find_assertion c v)
            ).

    Definition Find_kind (c:t) (v:string) : option Kinds.t := 
        Options.bind (Find_assertion c v) (fun a => Assertions.Get_kind a v).

    Definition Find_type (c:t) (v:string) : option (Types.t_type Types.C_poly) := 
        Options.bind (Find_assertion c v) (fun a => Assertions.Get_type a v).

    Fixpoint Split (c:t) (v:string) : option (t * Assertions.t * t) :=
        fold c
            (empty := fun _ => None) 
            (assertion := fun c a => 
                Options.fold (Find_assertion c v)
                    (some := fun a => Some (c,a,empty))
                    (none := fun _ => 
                        let+ (c,a',c') := Split c v in 
                        (c,a',Add_first c' a)
                    )
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
