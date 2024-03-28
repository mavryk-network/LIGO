#include "dir/types.mligo"

(* Three entrypoints *)
[@entry]
let increment (delta : int) (store : storage) : return = [], store + delta

[@entry]
let decrement (delta : int) (store : storage) : return = [], store - delta

[@entry]
let reset (() : unit) (_ : storage) : return = [], 0
