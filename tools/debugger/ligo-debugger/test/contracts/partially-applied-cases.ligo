// MAVRYK: PascaLIGO. Port of partially-applied-cases.mligo (Navigation "Partially applied" golden).
// PascaLIGO multi-parameter functions are curried, so f(x) partially applies. Chained application
// f(x)(y) is not valid PascaLIGO syntax, so intermediate bindings are used where needed.
function f (const a : int; const b : int) : int is a + b

function f_uncurry (const ab : int * int; const c : int) : int is ab.0 + ab.1 + c

function op (const fn : int -> int; const a : int; const b : int) : int is fn (a) + b

function triple (const a : int; const b : int; const c : int; const d : int) : int is a + b + c + d

function simple_case (const _u : unit) : unit is
  block {
    const applied : int -> int = f (42);
    const _should_not_have_applied_args : int = applied (10)
  } with unit

function apply_vars (const _u : unit) : unit is
  block {
    const a : int = 42;
    const _applied : int -> int = f (a)
  } with unit

function tuple_is_one_arg (const _u : unit) : unit is
  block {
    const _applied : int -> int = f_uncurry ((42, 42))
  } with unit

function apply_expression (const _u : unit) : unit is
  block {
    const a : int = 100;
    const b : int = a - 42;
    const _applied : int -> int = f (a + b + a * b)
  } with unit

function apply_applied_function (const _u : unit) : unit is
  block {
    const f1 : int -> int = f (42);
    const op_f1 : int -> int -> int = op (f1);
    const _apply : int -> int = op_f1 (1000 - 7)
  } with unit

function apply_sequence (const _u : unit) : unit is
  block {
    const f1 : int -> int -> int -> int = triple (1);
    const f2 : int -> int -> int = f1 (2);
    const f3 : int -> int = f2 (3);
    const _f4 : int = f3 (4)
  } with unit

type some_foo is int -> int -> int

function type_aliases_have_children (const _u : unit) : unit is
  block {
    const t1 : int -> int -> int -> int = triple (1);
    const _type_aliased : some_foo = t1 (2)
  } with unit

[@entry] function main (const _p : unit; const _s : unit) : list (operation) * unit is
  block {
    const _1 : unit = simple_case (unit);
    const _2 : unit = apply_vars (unit);
    const _3 : unit = tuple_is_one_arg (unit);
    const _4 : unit = apply_expression (unit);
    const _5 : unit = apply_applied_function (unit);
    const _6 : unit = apply_sequence (unit);
    const _7 : unit = type_aliases_have_children (unit)
  } with ((nil : list (operation)), unit)
