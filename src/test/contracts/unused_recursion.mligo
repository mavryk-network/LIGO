let coucou = fun (_:int) : int ->
  let number = 2 in
  let id (x : int) : int = x in
  let rec foo : (int -> int) -> int = fun (foo : (int -> int)) -> let foo = foo 0 in foo in
  let rec toto : int -> int = fun (toto:int) : int -> let number = toto in number + 1 in
  toto (number)  + foo (id)

let main ((_, _) : (unit * int)) : (operation list * int) =
 ([] : operation list), coucou 0