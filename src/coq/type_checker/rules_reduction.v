Set Implicit Arguments.

From Coq Require Import List String.

From ligo_coq_type_checker Require Import options.
From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import types.
From ligo_coq_type_checker Require Import assertions.
From ligo_coq_type_checker Require Import context.

Module Reduction_Rules.

    (* G |- A --> B *)
    Fixpoint Reduce {C:Types.classifier} (c:Context.t) (t:Types.t Types.S_type C) : Types.t Types.S_type C :=
        Types.fold (B:=fun S C => Types.t S C)
            t
            (*  
            *)            
            (exist:=fun C n => Types.var "TODO") 
            (*  
            *)            
            (var:=fun C n => Types.var "TODO") 
            (*  

            ------------
            G |- 1 --> 1
            *)            
            (one:=fun _ => Types.one)
            (*  
            *)            
            (arrow:=fun C t1 t2 => Types.var "TODO")
            (*  
            *)            
            (for_all:=fun s k t => Types.var "TODO")
            (*  
            *)            
            (lambda:=fun C s k1 t => Types.var "TODO")
            (*  
            *)            
            (apply:=fun C t1 t2 => Types.var "TODO")
            (*  
            *)            
            (product:=fun C r => Types.var "TODO")
            (*  
            *)            
            (sum:=fun C r => Types.var "TODO")

    (* G |- r --> r' *)
    with Reduce_row {C} (c:Context.t) (v:Types.t_row C) : Types.t_row C :=
        Types.fold_row v
            (*  

            ------------
            G |- . --> .
            *)                        
            (empty:=fun _ => Types.empty)
            (*  
            *)            
            (row:=fun C v s r => Types.empty).

End Reduction_Rules.
