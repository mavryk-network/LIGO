Set Implicit Arguments.

From Coq Require Import String.

From ligo_coq_type_checker Require Import types.
From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import context.
From ligo_coq_type_checker Require Import assertions.

Module Types_Rules.

    Fixpoint Check_kind {A} (c:Context.t) (t:Types.t Types.S_type A) (k:Kinds.t) : Prop :=
        Types.fold t
            (exist:=fun _ => True)
            (var:=fun _ => True)
            (one:=fun _ => Kinds.isKind k)
            (arrow:=fun A t1 t2 => 
                Kinds.isKind k /\ @Check_kind A c t1 k /\ @Check_kind A c t2 k 
            )
            (for_all:=fun s k1 t => 
                Kinds.fold k
                    (kind:=fun tt => False)
                    (arrow:=fun k1 k2 => 
                        let c := Context.assertion c (Assertions.kind_variable s k1) in 
                        k = k1 /\ @Check_kind Types.C_poly c t k2)
            )
            (lambda:=fun A s k t => 
                let c := Context.assertion c (Assertions.kind_variable s k) in 
                @Check_kind A c t Kinds.kind
            )
            (apply:=fun A l t => False (* Need a basic kind inference mechanism *))
            (product:=fun A r => 
                Kinds.isKind k /\ @Check_kind_row A c r
            )
            (sum:=fun A r => 
                Kinds.isKind k  /\ @Check_kind_row A c r
            )

    with Check_kind_row {A} (c:Context.t) (v:Types.t_row A) : Prop :=
        Types.fold_row v
            (single:=fun A v s => 
                @Check_kind A c s Kinds.kind
            )
            (row:=fun A v s r => 
                @Check_kind A c s Kinds.kind /\ @Check_kind_row A c r
            ).

End Types_Rules.
