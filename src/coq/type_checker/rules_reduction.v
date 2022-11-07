Set Implicit Arguments.

From Coq Require Import List String.

From ligo_coq_type_checker Require Import options.
From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import types.
From ligo_coq_type_checker Require Import assertions.
From ligo_coq_type_checker Require Import context.

Module Reduction_Rules.

    (* G |- A --> B *)
    Fixpoint Reduce_type {C:Types.classifier} (c:Context.t) (t:Types.t Types.S_type C) : bool * Types.t Types.S_type C :=
        Types.fold (B:=fun S C => prod bool (Types.t S C))
            t
            (*  

            ------------
            G |- a --> a
            *)            
            (exist:=fun n =>
                (false, Types.exist n)
            )
            (*  
                            
            ------------    ------------------------
            G |- v --> v    G, v :: k = A |- v --> v
            *)            
            (var:=fun n => 
                match Context.Find_type c n with
                | Some t => (true, t)
                | None => (false, Types.var n)
                end
            )
            (*  

            ------------
            G |- 1 --> 1
            *)            
            (one:=fun _ => 
                (false, Types.one)
            )
            (*  
            G |- t1 --> t1'                G |- t2 --> t2'
            ---------------------------    ---------------------------    
            G |- t1 -> t2 --> t1' -> t2    G |- t1 -> t2 --> t1 -> t2'
            *)            
            (arrow:=fun C t1 t2 => 
                let (m1, t1') := @Reduce_type C c t1 in
                let (m2, t2') := if m1 then (m1, t2) else @Reduce_type C c t2 in
                (m2, @Types.arrow C t1' t2')
            )
            (*  
            G |- t --> t'
            ---------------------------------------
            G |- forall(s::k).t --> forall(s::k).t'
            *)            
            (for_all:=fun s k t => 
                let (m, t') := Reduce_type c t in
                (m, Types.for_all s k t')
            )
            (*  
            G |- t --> t'
            ---------------------------------------
            G |- lambda(s::k).t --> lambda(s::k).t'
            *)            
            (lambda:=fun C s k t => 
                let (m, t') := @Reduce_type C c t in
                (m, @Types.lambda C s k t')
            )
            (*  
            G |- t1 --> t1'          G |- t2 --> t2'
            ---------------------    ---------------------    
            G |- t1 t2 --> t1' t2    G |- t1 t2 --> t1 t2'
            *)            
            (apply:=fun C t1 t2 => 
                let (m1, t1') := @Reduce_type C c t1 in
                let (m2, t2') := if m1 then (m1, t2) else @Reduce_type C c t2 in
                (m2, @Types.apply C t1' t2')            
            )
            (*  
            *)            
            (product:=fun C r => 
                let (m, l) := @Reduce_row C c r in
                (m, @Types.product C l)
            )
            (*  
            *)            
            (sum:=fun C r => 
                let (m, l) := @Reduce_row C c r in
                (m, @Types.sum C l)
            )

    (* G |- r --> r' *)
    with Reduce_row {C} (c:Context.t) (v:Types.t_row C) : bool * Types.t_row C :=
        Types.fold_row v
            (*  

            ------------
            G |- . --> .
            *)                        
            (empty:=fun _ => (false, Types.empty))
            (*  
            G |- t --> t'                      G |- r -> r'
            -------------------------------    -------------------------------
            G |- v : t :: r --> v : t' :: r    G |- v : t :: r --> v : t :: r'
            *)            
            (row:=fun C v t r => 
                let (m1, t') := @Reduce_type C c t in
                let (m2, r') := if m1 then (m1, r) else @Reduce_row C c r in
                (m2, @Types.row C v t' r')
            ).

    Definition Reduce {C:Types.classifier} (c:Context.t) (t:Types.t Types.S_type C) : Types.t Types.S_type C :=
        snd (@Reduce_type C c t).

End Reduction_Rules.
