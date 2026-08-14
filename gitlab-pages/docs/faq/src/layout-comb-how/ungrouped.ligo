type foo is
  record [ foo : nat ; bar : int ; baz : string ]

module Foo is {
  [@entry]
  function foo (const _f : foo; const s : unit) : list (operation) * unit is
    ((nil : list (operation)), s)

  // dummy entrypoint to avoid bug with single entrypoint :(
  [@entry]
  function dummy (const _u : unit; const s : unit) : list (operation) * unit is
    ((nil : list (operation)), s)
}

module Bar is {
  [@entry]
  function bar (const addr : address; const s : unit) : list (operation) * unit is
    block {
      const arg : foo = record [ foo = 1n; bar = 2; baz = "three" ];
      const amt : mav = 0mv;
      const dst : contract (foo) = Mavryk.get_entrypoint ("%foo", addr);
      const tx = Mavryk.transaction (arg, amt, dst);
    } with (list [tx], s)

  // dummy entrypoint to avoid bug with single entrypoint :(
  [@entry]
  function dummy (const _u : unit; const s : unit) : list (operation) * unit is
    ((nil : list (operation)), s)
}

function test_interaction (const _u : unit) : unit is
  block {
    const orig_foo = Test.originate (contract_of Foo, unit, 0mv);
    const foo_addr = Test.to_address (orig_foo.addr);
    const orig_bar = Test.originate (contract_of Bar, unit, 0mv);
    const _r = Test.transfer_exn (orig_bar.addr, Bar (foo_addr), 0mv);
  } with unit
type tree_record is
  [@layout tree]
  record [ foo : nat ; bar : int ; baz : string ]

type tree_variant is
  [@layout tree]
  | Foo of nat
  | Bar of int
  | Baz of string

type tree_tuple is
  [@layout tree] (nat * int * string)
  // the parentheses are required, else the @layout attribute will
  // attach to the first tuple field instead of the tuple type

function anon_tree_tuple (const p : [@layout tree] (nat * int * string)) : [@layout tree] (nat * int * string) is p