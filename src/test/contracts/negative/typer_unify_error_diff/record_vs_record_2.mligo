
let main (_p : int) (s : int) : operation list * int =
  let y : {foo : int ; bar : (nat * string) ; third_field : mav} = {foo = 1 ; bar = (2n, "lol") ; third_field = 42mav } in
  let x : {foo : int ; bar : (nat * nat   )}                     = y in
  //                                ^^^^^^    ^^^^^^^^^^^^^^^^^
  ([] : operation list), s
