module H=Helpers
open Ast_typed
open H

let michelson_slice = 
  "{ UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} }"

let slice_type = t_function (t_triplet (t_nat ()) (t_nat ()) (t_string ())) (t_string ()) ()

let mixed  = [
  ("length", e_a_raw_code "{ SIZE }" (t_function (t_string ()) (t_nat ()) ())) ;
  ("size"  , e_a_raw_code "{ SIZE }" (t_function (t_string ()) (t_nat ()) ())) ;
]

let uncurried = [
  ("slice" , e_a_raw_code michelson_slice slice_type) ; (*deprecated*)
  ("sub"   , e_a_raw_code michelson_slice slice_type) ;
  ("concat", e_a_raw_code "{ UNPAIR ; CONCAT }" (t_function (t_pair (t_string ()) (t_string ())) (t_string ()) ())) ; 
]
let curried = [
  ("slice" , curryfy3 (e_a_raw_code michelson_slice) (t_string ()) (t_nat (), t_nat (), t_string ()) ) ; (*deprecated*)
  ("sub"   , curryfy3 (e_a_raw_code michelson_slice) (t_string ()) (t_nat (), t_nat (), t_string ()) ) ;
  ("concat", curryfy2 (e_a_raw_code "{ UNPAIR; CONCAT }") (t_string ()) ((t_string ()), (t_string ())) ) ; 
]

let module_ e ~curry = 
  make_module e "String"
  (match curry with 
    true -> mixed @ curried
  | false -> mixed @ uncurried)