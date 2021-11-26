module H=Helpers
open Ast_typed
open H

let mixed = [
  ("pack"   , e_a_raw_code "{ PACK   }" (t_for_all (Location.wrap @@ Var.of_name "_a") () (t_function (t_variable (Var.of_name "_a") ) (t_bytes    ()) ())) ) ;
  ("unpack" , e_a_raw_code "{ UNPACK }" (t_for_all (Location.wrap @@ Var.of_name "_a") () (t_function (t_bytes ()) (t_option (t_variable (Var.of_name "_a")) ) ())) ) ;
  ("length" , e_a_raw_code "{ SIZE   }" (t_function (t_bytes ()) (t_nat ()) ())) ;
  ("size"   , e_a_raw_code "{ SIZE   }" (t_function (t_bytes ()) (t_nat ()) ())) ;
]

let michelson_slice = 
  "{ UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} }"

let slice_type = t_function (t_triplet (t_nat ()) (t_nat ()) (t_bytes ())) (t_bytes ()) ()

let uncurried = [
  ("sub"   , e_a_raw_code michelson_slice slice_type) ;
  ("concat", e_a_raw_code "{ UNPAIR ; CONCAT }" (t_function (t_pair (t_bytes ()) (t_bytes ())) (t_bytes ()) ())) ; 
]
let curried = [
  ("sub"   , curryfy3 (e_a_raw_code michelson_slice) (t_bytes ()) (t_nat (), t_nat (), t_bytes ()) ) ;
  ("concat", curryfy2 (e_a_raw_code "{ UNPAIR; CONCAT }") (t_bytes ()) ((t_bytes ()), (t_bytes ())) ) ; 
]

let module_ e ~curry = 
  make_module e "Bytes"
  (match curry with 
    true -> mixed @ curried
  | false -> mixed @ uncurried)

