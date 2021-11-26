module H=Helpers
open Ast_typed
open H

let mixed = [
]


let uncurried = [
  ("or"          , e_a_raw_code "{ UNPAIR; OR }"  (t_function (t_pair (t_nat ()) (t_nat ())) (t_nat ()) ())) ;
  ("and"         , e_a_raw_code "{ UNPAIR; AND }" (t_function (t_pair (t_nat ()) (t_nat ())) (t_nat ()) ())) ;
  ("xor"         , e_a_raw_code "{ UNPAIR; XOR }" (t_function (t_pair (t_nat ()) (t_nat ())) (t_nat ()) ())) ;
  ("shift_left"  , e_a_raw_code "{ UNPAIR; LSL }" (t_function (t_pair (t_nat ()) (t_nat ())) (t_nat ()) ())) ;
  ("shift_right" , e_a_raw_code "{ UNPAIR; LSR }" (t_function (t_pair (t_nat ()) (t_nat ())) (t_nat ()) ())) ;
]
let curried = [
  ("or"          , curryfy2 (e_a_raw_code "{ UNPAIR; OR }" ) (t_nat ()) (t_nat (), t_nat ()) ) ;
  ("and"         , curryfy2 (e_a_raw_code "{ UNPAIR; AND }") (t_nat ()) (t_nat (), t_nat ()) ) ;
  ("xor"         , curryfy2 (e_a_raw_code "{ UNPAIR; XOR }") (t_nat ()) (t_nat (), t_nat ()) ) ;
  ("shift_left"  , curryfy2 (e_a_raw_code "{ UNPAIR; LSL }") (t_nat ()) (t_nat (), t_nat ()) ) ;
  ("shift_right" , curryfy2 (e_a_raw_code "{ UNPAIR; LSR }") (t_nat ()) (t_nat (), t_nat ()) ) ;
]

let module_ e ~curry = 
  make_module e "Bitwise"
  (match curry with 
    true -> mixed @ curried
  | false -> mixed @ uncurried)

