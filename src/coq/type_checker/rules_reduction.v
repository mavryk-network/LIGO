Set Implicit Arguments.

From Coq Require Import List String.

From ligo_coq_type_checker Require Import options.
From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import types.
From ligo_coq_type_checker Require Import assertions.
From ligo_coq_type_checker Require Import context.

Module Reduction_Rules.

    (* G |- A ~~> B *)
    Fixpoint Reduce_type {C:Types.classifier} (c:Context.t) (t:Types.t_type C) : option (bool * Types.t_type C) :=
        Types.fold (B:=fun S C => option (prod bool (Types.t S C)))
            t
            (*  

            ------------
            G |- a ~~> a
            *)            
            (exist:=fun n =>
                let+ _ := Context.Find_kind c n in
                (false, Types.exist n)
            )
            (*  
                            
            ------------    ------------------------
            G |- v ~~> v    G, v :: k = A |- v ~~> A
            *)            
            (var:=fun n => 
                let+ _ := Context.Find_kind c n in
                    Options.fold (Context.Find_type c n)
                    (some:=fun t => (true, t))
                    (none:=fun _ => (false, Types.var n))                    
            )
            (*  

            ------------
            G |- 1 ~~> 1
            *)            
            (one:=fun _ => 
                Some (false, Types.one)
            )
            (*  
            G |- t1 ~~> t1'                G |- t2 ~~> t2'
            ---------------------------    ---------------------------    
            G |- t1 -> t2 ~~> t1' -> t2    G |- t1 -> t2 ~~> t1 -> t2'
            *)            
            (arrow:=fun C t1 t2 => 
                let* (m,t1) := @Reduce_type C c t1 in
                let+ (m,t2) := if m then Some (m,t2) else @Reduce_type C c t2 in
                (m, @Types.arrow C t1 t2)
            )

            (*  
            G |- t ~~> t'
            ---------------------------------------
            G |- forall(s::k).t ~~> forall(s::k).t'
            *)            
            (for_all:=fun s k t => 
                let+ (m, t) := Reduce_type c t in
                (m, Types.for_all s k t)
            )
            (*  
            G |- t ~~> t'
            ---------------------------------------
            G |- lambda(s::k).t ~~> lambda(s::k).t'
            *)            
            (lambda:=fun C s k t => 
                let+ (m, t) := @Reduce_type C c t in
                (m, @Types.lambda C s k t)
            )
            (*  
            G |- t1 ~~> t1'          G |- t2 ~~> t2'
            ---------------------    ---------------------    
            G |- t1 t2 ~~> t1' t2    G |- t1 t2 ~~> t1 t2'
            *)            
            (apply:=fun C t1 t2 => 
                let* (m, t1) := @Reduce_type C c t1 in
                let+ (m, t2) := if m then Some (m, t2) else @Reduce_type C c t2 in
                (m, @Types.apply C t1 t2)            
            )
            (*  
            G |- r ~~> r'
            -------------------
            G |- Pi r ~~> Pi r'
            *)            
            (product:=fun C r => 
                let+ (m,l) := @Reduce_row C c r in
                (m, @Types.product C l)
            )
            (*  
            G |- r ~~> r'
            ---------------------
            G |- Sum r ~~> Sum r'
            *)            
            (sum:=fun C r => 
                let+ (m, l) := @Reduce_row C c r in
                (m, @Types.sum C l)
            )

    (* G |- r ~~> r' *)
    with Reduce_row {C} (c:Context.t) (v:Types.t_row C) :option (bool * Types.t_row C) :=
        Types.fold_row v
            (*  

            ------------
            G |- . ~~> .
            *)                        
            (empty:=fun _ => Some (false, Types.empty))
            (*  
            G |- t ~~> t'                      G |- r -> r'
            -------------------------------    -------------------------------
            G |- v : t :: r ~~> v : t' :: r    G |- v : t :: r ~~> v : t :: r'
            *)            
            (row:=fun C v t r => 
                let* (m1, t') := @Reduce_type C c t in
                let+ (m2, r') := if m1 then Some (m1, r) else @Reduce_row C c r in
                (m2, @Types.row C v t' r')
            ).

    Definition Reduce {C:Types.classifier} (c:Context.t) (t:Types.t Types.S_type C) : option (Types.t Types.S_type C) :=
        let+ (_,t) := @Reduce_type C c t in t.

End Reduction_Rules.
