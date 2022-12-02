module I = Ast_unified
module O = Ast_core

(*
I --p1-> I --p2--> I --p3--> I --p4--> I --trivial--> O 
*)

type syntax = unit (* TODO *)

type 'a pass =
  { name : string (* useful ? *)
  ; compile : syntax -> 'a -> 'a
  ; decompile : syntax -> 'a -> 'a
  ; check_reductions : 'a -> bool (* mostly useful for debugging *)
  }

type 'a check =
  { name : string
  ; f : syntax -> 'a -> unit
  }
