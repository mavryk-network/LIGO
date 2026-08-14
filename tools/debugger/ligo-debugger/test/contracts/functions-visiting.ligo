// MAVRYK: PascaLIGO. Port of functions-visiting.mligo (statement-granularity StepIn golden).
function f (const a : int) : int is
  block {
    const x  : int = a + 100;
    const x2 : int = x + 100
  } with x2

function g (const _u : unit) : int is
  block {
    const y : int = 10 + 10
  } with y

[@entry] function main (const _p : unit; const s : int) : list (operation) * int is
  block {
    const s2 : int = s + 1;
    const s3 : int = g (unit) + f (s2)
  } with ((nil : list (operation)), s3)
