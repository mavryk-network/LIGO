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

  let a1 = 15 in
  // let a1 = List.fold_left (fun (a, i)  -> 
  //   a + i
  // ) 0 [1; 2; 3; 4; 5]
  // in

  let a2x a b = a + b in
  let a2 = a2x 2 3 in
  let a3 = 0 in
  let a3x (a, b) = a + b in
  let a3 = a3x (3, 3) in
  // let a2 = 5 in
  let a = 
    4 + 5 +  (* 9 *)
    40 / 5 + (* 9 + 8 = 17 *)
    x +      (* 17 + 2 + 3 + 6 = 28 *)
    y +      (* 28 + 9  = 37 *)
    z +      (* 37 + 2 = 39 *)
    a1 +     (* 39 + 1 + 2 + 3 + 4 + 5 = 54 *)
    a2 +     (* 44 + 5 = 59 *)
    a3       (* 59 + 6 = 65 *) 
  in 
  ([]: operation list), a
