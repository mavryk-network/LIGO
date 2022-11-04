Set Implicit Arguments.

From Coq Require Import List String.

From ligo_coq_type_checker Require Import types.
From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import context.
From ligo_coq_type_checker Require Import assertions.
From ligo_coq_type_checker Require Import options.

Module Kinds_Rules.

    (* Basic inference used for applications *)
    Fixpoint Infer_kind {A} (c:Context.t) (t:Types.t Types.S_type A) : option Kinds.t :=
        Types.fold t
            (exist:=fun n => Context.Find_kind c n)
            (var:=fun n => Context.Find_kind c n)
            (one:=fun _ => Some Kinds.kind)
            (arrow:=fun A t1 t2 => Some Kinds.kind)
            (for_all:=fun _ _ _ => Some Kinds.kind)
            (lambda:=fun A s k1 t => 
                let c := Context.assertion c (Assertions.kind_variable s k1) in 
                match @Infer_kind A c t with
                | Some k2 => Some (Kinds.arrow k1 k2)
                | None => None
                end
            )
            (apply:=fun A l t => 
                Options.bind (@Infer_kind A c l) (fun v => 
                    Kinds.fold v
                        (kind:=fun _ => None)
                        (arrow:=fun k1 k2 => Some k2)
                )
            )
            (product:=fun A r => Some Kinds.kind)
            (sum:=fun A r => Some Kinds.kind).

    (* G |- A :: k *)
    Fixpoint Check_kind {A} (c:Context.t) (t:Types.t Types.S_type A) (k:Kinds.t) : Prop :=
        Types.fold t
            (*  
            a^ :: k in G
            ------------
            G |- a^ :: k
            *)            
            (exist:=fun n => Options.Is_Some (Context.Find_kind c n)) 
            (*  
            a :: k in G     a :: k = A in G
            -----------     ---------------
            G |- a :: k .   G |- a :: k
            *)            
            (var:=fun n => Options.Is_Some (Context.Find_kind c n)) 
            (*  

            -----------
            G |- 1 :: *
            *)            
            (one:=fun _ => Kinds.Is_Kind k)
            (*  
            G |- t1 :: *     G |- t2 :: *
            -----------------------------
            G |- t1 -> t2 :: *
            *)            
            (arrow:=fun A t1 t2 => 
                Kinds.Is_Kind k /\ @Check_kind A c t1 k /\ @Check_kind A c t2 k 
            )
            (*  
            G, s::k |- t :: *
            ------------------------
            G |- forall(s::k).t :: *
            *)            
            (for_all:=fun s k t => 
                let c := Context.assertion c (Assertions.kind_variable s k) in 
                @Check_kind Types.C_poly c t Kinds.kind
            )
            (*  
            G, s::k1 |- t :: k2
            --------------------------------
            G |- lambda(s::k1).t :: k1 -> k2
            *)            
            (lambda:=fun A s k1 t => 
                Kinds.fold k
                    (kind:=fun tt => False)
                    (arrow:=fun k1' k2 => 
                        let c := Context.assertion c (Assertions.kind_variable s k1) in 
                        k1 = k1' /\ @Check_kind A c t k2)
            )
            (*  
            G |- t1 :: k1 -> k2    G |- t2 :: k1
            ------------------------------------
            G |- t1 t2 :: k2
            *)            
            (apply:=fun A t1 t2 => 
                let prop := Options.map (fun v => 
                    Kinds.fold v
                        (kind:=fun _ => False)
                        (arrow:=fun k1 k2 => 
                            k = k2 /\ @Check_kind A c t1 (Kinds.arrow k1 k2) /\ @Check_kind A c t2 k1
                        )
                    ) (@Infer_kind A c t1) in 
                Options.fold prop (some:=id) (none:=fun tt => False)
            )
            (*  
            G |- t row
            --------------
            G |- Pi t :: *
            *)            
            (product:=fun A r => 
                Kinds.Is_Kind k /\ @Check_kind_row A c r
            )
            (*  
            G |- t row
            -----------------
            G |- Sigma t :: *
            *)            
            (sum:=fun A r => 
                Kinds.Is_Kind k /\ @Check_kind_row A c r
            )

    (* G |- t row *)
    with Check_kind_row {A} (c:Context.t) (v:Types.t_row A) : Prop :=
        Types.fold_row v
            (*  

            ----------
            G |- . row
            *)                        
            (empty:=fun _ => 
                True
            )
            (*  
            G |- s :: *    l not in domain(t)    G |- t row
            -----------------------------------------------
            G |- v : s :: t row
            *)            
            (row:=fun A v s r => 
                @Check_kind A c s Kinds.kind /\ not (In v (Types.domain r)) /\ @Check_kind_row A c r
            ).

End Kinds_Rules.
