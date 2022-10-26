Set Implicit Arguments.
From Coq Require Import String List.

From ligo_coq_type_checker Require Import assertions.

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

End Context.

Module Context_Rules.

    Fixpoint Contains (c:Context.t) (a:Assertions.t) : Prop :=
        Context.fold c 
            (empty := fun _ => False)
            (assertion := fun c a' => a' = a \/ Contains c a).

    Definition Check_assertion (a:Assertions.t) (c:Context.t) : Prop :=
        Assertions.fold a
            (type_variable := fun n _ => not (Context.In_domain c n))
            (kind_variable := fun n _ => not (Context.In_domain c n)) 
            (kind_bound_variable := fun n _ _ => not (Context.In_domain c n))
            (exist_marker := fun n => not (Contains c (Assertions.exist_marker n) \/ Context.In_domain c n))
            (kind_inferable := fun n _ => not (Context.In_domain c n))
            (kind_bound_inferable := fun n _ _ => not (Context.In_domain c n)).

    (*  Gamma ctx *)
    Fixpoint Check (c:Context.t) : Prop :=
        Context.fold c 
            (empty := fun _ => True)
            (assertion := fun c a => Check_assertion a c \/ Check c).

End Context_Rules.
