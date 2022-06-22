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

    The `Spilling` pass:
    - translates recursive functions to loops
    - handles some special "iterator" constants which expect their
      loop body to be given directly
    - brings the type system closer to Michelson, translating record
      types to tuple/pair types, and variant types to sum/`or` types,
      and translating expressions accordingly.
    - converts bool and option constructors and matching from generic
      variants back to special syntax, so they can be handled
      specially for Michelson later.

- [Mini\_c](./stages/7-mini_c)

  The `Mini_c` stage exists primarily for historical reasons. Once the
  `Self_mini_c` pass is ported to the Coq IR below, `Mini_c` can
  probably be removed.

  - [Self\_mini\_c](./passes/15-self_mini_c)

    This pass performs aggressive code size optimizations, mainly
    inlining and beta reduction.
  - [Scoping](./passes/16-scoping)

    The `Scoping` pass:
    - converts variables to the de Bruijn representation
    - adapts to the peculiar style of the following `Ligo_coq_ocaml`
      stage, adopted for compatibility with Coq
    - uses `Predefined.Michelson` to compile "constant" applications
      to bits of inline Michelson using `E_inline_michelson`.
- [Ligo\_coq\_ocaml](./coq)

  Two stages are defined in Coq, currently both in
  [compiler.v](./coq/compiler.v): `expr`, a de Bruijn simply-typed
  lambda calculus, and `prog`/`list instr`, a subset of Michelson
  extended with some convenient fictions.

  - [Stacking](./passes/17-stacking)

    The `Stacking` pass (which depends on the Coq compiler.v) consists
    of:

    - the de Bruijn expr -> instr compiler (compile_expr etc in
      compiler.v)
    - followed by a "strengthening" pass on the Michelson
      (strengthen_instr etc in compiler.v) for efficiency and to
      recover linearity (particularly for tickets)
    - and finally a "To_micheline" pass which converts from the Coq
      `list instr` to actual Micheline
      (`Tezos_micheline.Micheline.node`), while translating remaining
      fictions to their real equivalents.

- Michelson
  - [Self\_michelson](./passes/18-self_michelson)

    `Self_michelson` does various peephole optimizations on Michelson.

See also: [Predefined](./passes/predefined) (used in a few passes)
