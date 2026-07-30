
let main (_p : int) (s : int) : operation list * int =
  let  x : string * int * nat * int *       string = "foo" , 42  , 24n , 42 ,        "bar" in
  let _y : mav    * int       * mav * nat * string = x in
  //       ^^^^^^         ^^^         ^^^   
  ([] : operation list), s