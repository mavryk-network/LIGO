module AST = Ast_core

module Of_Ast : sig
  val program : AST.program -> Types.def list -> Types.def list
end

module Of_Stdlib_Ast : sig
  val program : AST.program -> Types.def list
end
