let coucou = fun (_:int) : int ->
  let number = 2 in
  let rec toto : int -> int = fun (toto:int) : int -> let number = toto in number + 1 in
  toto (number)

let main ((_, _) : (unit * int)) : (operation list * int) =
 ([] : operation list), coucou 0