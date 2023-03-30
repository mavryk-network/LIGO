module AST = Ast_core

module Of_Ast : sig
  module Waivers : sig
    type t =
      { (* Useful for Stdlib AST traversal, when declaration rhs are unwanted *)
        d_value_expr : bool
      ; d_type_expr : bool
      ; d_irrefutable_match_expr : bool
      }

    val default : t
    val of_opt : t option -> t
  end

  val program : ?waivers:Waivers.t -> AST.program -> Types.def list -> Types.def list
end

module Of_Stdlib_Ast : sig
  val program : AST.program -> Types.def list
end
