# Stages and passes in order:

Passes operating on a stage are listed under that stage. The last pass
emits the next stage.

- Text (CameLIGO, ReasonLIGO, PascaLIGO)
  - [Preprocessing](./passes/00-preprocessing)
  - [Lexing](./passes/01-lexing)
  - [Parsing](./passes/02-parsing)
- [CST](./stages/1-cst) Concrete syntax
  - [Tree\_abstraction](./passes/04-tree_abstraction)
- [Ast\_imperative](./stages/2-ast_imperative)
  - [Self\_ast\_imperative](./passes/05-self_ast_imperative)
  - [Purification](./passes/06-purification)
- [Ast\_sugar](./stages/3-ast_sugar)
  - [Self\_ast\_sugar](./passes/07-self_ast_sugar)
  - [Desugaring](./passes/08-desugaring)
- [Ast\_core](./stages/4-ast_core)
  - [Self\_ast\_core](./passes/09-self_ast_core)
  - [Checking](./passes/10-checking)
- [Ast\_typed](./stages/5-ast_typed)
  - [Self\_ast\_typed](./passes/11-self_ast_typed)
  - [Aggregation](./passes/12-aggregation)
- [Ast\_aggregated](./stages/6-ast_aggregated)
  - [Self\_ast\_aggregated](./passes/13-self_ast_aggregated)
  - [Spilling](./passes/14-spilling)
- [Mini\_c](./stages/7-mini_c)
  - [Self\_mini\_c](./passes/15-self_mini_c)
  - [Scoping](./passes/16-scoping)
- [Ligo\_coq\_ocaml](./coq)
  - [Stacking](./passes/17-stacking)
- Michelson
  - [Self\_michelson](./passes/18-self_michelson)

See also: [Predefined](./passes/predefined) (used in a few passes)
