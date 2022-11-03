Set Implicit Arguments.
From Coq Require Import String List.

From ligo_coq_type_checker Require Import assertions.
From ligo_coq_type_checker Require Import context.
From ligo_coq_type_checker Require Import types.
From ligo_coq_type_checker Require Import kinds.

Import ListNotations.

Module Context_Rules.

    Fixpoint Contains (c:Context.t) (a:Assertions.t) : Prop :=
        Context.fold c 
            (empty := fun _ => False)
            (assertion := fun c a' => a' = a \/ Contains c a).

    Definition Check_assertion (a:Assertions.t) (c:Context.t) : Prop :=
        (* Note: premisses related to kind checking are missing *)
        Assertions.fold a
            (type_variable := fun n _ => not (Context.In_domain c n))
            (kind_variable := fun n _ => not (Context.In_domain c n)) 
            (kind_bound_variable := fun n _ _ => not (Context.In_domain c n))
            (exist_marker := fun n => not (Contains c (Assertions.exist_marker n) \/ Context.In_domain c n))
            (kind_inferable := fun n _ => not (Context.In_domain c n))
            (kind_bound_inferable := fun n _ _ => not (Context.In_domain c n)).

    Fixpoint Check (c:Context.t) : Prop :=
        Context.fold c 
            (empty := fun _ => True)
            (assertion := fun c a => Check_assertion a c \/ Check c).

End Context_Rules.
