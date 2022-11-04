Set Implicit Arguments.
From Coq Require Import String List.

From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import types.
From ligo_coq_type_checker Require Import kinds_rules.
From ligo_coq_type_checker Require Import assertions.
From ligo_coq_type_checker Require Import context.

Import ListNotations.

Module Context_Rules.

    Fixpoint Contains (c:Context.t) (a:Assertions.t) : Prop :=
        Context.fold c             
            (empty := fun _ => False)
            (assertion := fun c a' => a' = a \/ Contains c a).

    (* G |- c ctx *)
    Definition Check_assertion (a:Assertions.t) (c:Context.t) : Prop :=
        Assertions.fold a
            (*  
            x not in domain(G)    G |- A :: *   
            ---------------------------------
            G |- (x : A) ctx
            *)
            (type_variable := fun n t => 
                not (Context.In_domain c n) /\ Kinds_Rules.Check_kind c t Kinds.kind
            )
            (*  
            a not in domain(G)
            ------------------
            G |- (a :: k) ctx
            *)
            (kind_variable := fun n _ => 
                not (Context.In_domain c n)
            ) 
            (*  
            a not in domain(G)    G |- A :: *   
            ---------------------------------
            G |- (a :: k = A) ctx
            *)
            (kind_bound_variable := fun n k t => 
                not (Context.In_domain c n) /\ Kinds_Rules.Check_kind c t k
            )
            (*  
            a not in domain(G)    |> a^ not in G
            ------------------------------------
            G |- |> a^ ctx
            *)
            (exist_marker := fun n => 
                not (Contains c (Assertions.exist_marker n) \/ Context.In_domain c n)
            )
            (*  
            a^ not in domain(G)
            -------------------
            G |- (a^ :: k) ctx
            *)
            (kind_inferable := fun n _ => 
                not (Context.In_domain c n)
            )
            (*  
            a^ not in domain(G)    G |- t :: k
            ----------------------------------
            G |- (a^ :: k = t) ctx
            *)
            (kind_bound_inferable := fun n k t => 
                not (Context.In_domain c n) /\ Kinds_Rules.Check_kind c t k
            ).

    (* G ctx *)
    Fixpoint Check (c:Context.t) : Prop :=
        Context.fold c 
            (*  

            --------------
            . ctx
            *)            
            (empty := fun a => 
                True
            )
            (*  
            G ctx  G |- A ctx 
            -----------------
            G, A ctx
            *)            
            (assertion := fun c a => 
                Check_assertion a c \/ Check c
            ).

End Context_Rules.
