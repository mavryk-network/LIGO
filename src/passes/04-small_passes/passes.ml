module I = Ast_unified

type syntax = unit (* TODO, options etc .. *)

type 'a pass =
  { name : string (* useful ? *)
  ; compile : syntax -> 'a -> 'a
  ; decompile : syntax -> 'a -> 'a
  ; check_reductions : raise:(Errors.t, unit) Simple_utils.Trace.raise -> 'a -> unit
  }

type 'a check =
  { name : string
  ; f : syntax -> 'a -> unit
  }
