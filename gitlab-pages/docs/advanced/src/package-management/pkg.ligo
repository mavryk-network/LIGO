// LIGO library for working with lists

function concat<a> (const xs : list (a); const ys : list (a)) : list (a) is
  block {
    function f (const p : a * list (a)) : list (a) is p.0 # p.1
  } with List.fold_right (f, xs, ys)

function reverse<a> (const xs : list (a)) : list (a) is
  block {
    function f (const p : list (a) * a) : list (a) is p.1 # p.0
  } with List.fold_left (f, (nil : list (a)), xs)
