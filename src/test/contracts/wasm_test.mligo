// import Bar from 'xxx';

type parameter = int
type storage = int

type check = 
  Foo of int 
| Bar of int 
| Barx of int

let main ((_, _s):(parameter * storage)) =
  let b = [2; 3; 6] in
  let x = match b with
  | hd :: x :: y :: _  -> hd + x + y
  | _       -> 7
  in
  let y = match Some (9) with 
    Some n -> n
  | None -> 10 
  in
  let z = match (Foo 2) with 
    Foo n -> n
  | _ -> 10
  in
  let a = 
    4 + 5 +  (* 9 *)
    40 / 5 + (* 9 + 8 = 17 *)
    x +      (* 17 + 2 + 3 + 6 = 28 *)
    y +      (* 28 + 9  = 37 *)
    z        (* 37 + 2 = 39 *)
  in 
  ([]: operation list), a
