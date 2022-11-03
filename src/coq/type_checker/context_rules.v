Set Implicit Arguments.
From Coq Require Import String List.

From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import types.
From ligo_coq_type_checker Require Import types_rules.
From ligo_coq_type_checker Require Import assertions.
From ligo_coq_type_checker Require Import context.

Import ListNotations.

Module Context_Rules.

    Fixpoint Contains (c:Context.t) (a:Assertions.t) : Prop :=
        Context.fold c 
            (empty := fun _ => False)
            (assertion := fun c a' => a' = a \/ Contains c a).

    Definition Check_assertion (a:Assertions.t) (c:Context.t) : Prop :=
        Assertions.fold a
            (type_variable := fun n t => 
                not (Context.In_domain c n) /\ Types_Rules.Check_kind c t Kinds.kind
            )
            (kind_variable := fun n _ => 
                not (Context.In_domain c n)
            ) 
            (kind_bound_variable := fun n k t => 
                not (Context.In_domain c n) /\ Types_Rules.Check_kind c t k
            )
            (exist_marker := fun n => 
                not (Contains c (Assertions.exist_marker n) \/ Context.In_domain c n)
            )
            (kind_inferable := fun n _ => 
                not (Context.In_domain c n)
            )
            (kind_bound_inferable := fun n k t => 
                not (Context.In_domain c n) /\ Types_Rules.Check_kind c t k
            ).

    Fixpoint Check (c:Context.t) : Prop :=
        Context.fold c 
            (empty := fun _ => 
                True
            )
            (assertion := fun c a => 
                Check_assertion a c \/ Check c
            ).

End Context_Rules.
