// import Bar from 'xxx';

type parameter = int
type storage = int

type check = 
  Foo of int 
| Bar of int 
| Barx of int

let other_c a b = 
  a + b

let other_c2 (a, b) = 
  a + b

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
 
  let x3 (a, i) = a + i in
  let a1 = List.fold_left x3 0 [1; 2; 3; 4; 5]
  in 

  let a2x a b = a + b in
  let a2 = a2x 2 in
  let a2 = a2 3 in
  
  let a3x (a, b) = a + b in
  let a3 = a3x (3, 3) in
  let a4 = a2x 2 in 
  let a5 = a4 2 in
  let a6 = a4 3 in
  
  let a7 = other_c 7 in
  let a8 = a7 8 in
  let a9 = other_c2 (3, 2) in


  let a12 = [1; 2; 3] in

  let a11 = List.map (fun i -> i * 10) a12 in

  let a10 = 
    List.fold_left (fun (a, i) -> a + i) 100 a11 + 
    List.fold_left (fun (a, i) -> a + i) 100 a12 in
  

  let a13 = [3; 2; 1; 0] in
  let xz (i, a) = a - i in

  let a12 = List.fold_right xz a13 10 in

  let a13 = List.iter (fun x -> assert (x < 10)) [3;5;7;9] in

  let a14 = List.tail_opt ([]: string list) in
  let a15 = match a14 with 
    Some x -> 55
  | _ -> 44
  in
  let a16 = match List.tail_opt [2; 4] with 
    Some x -> 
      (
        match x with 
          [i] -> if i = 4 then 55 else 22
        | _ -> 33
      )
  | _ -> 44
  in
  let a17 = List.head_opt [9;7;8;9;10] in
  let a17 = match a17 with 
    Some s -> s
  | None -> 0
  in
  let a18 = List.length [3; 4; 5; 6; 7] in
  let s1 = Set.literal [20; 30; 50] in
  let m1 = Map.literal [(3, 40); (5, 50)] in
  let xx = Set.cardinal s1 + Map.size m1 in
  
  let a19: int = int xx in
  let a20 = Set.remove 30 s1 in
  // let a21 = Map.remove 50 (Map.literal [(20, 1); (50, 2); (30, 3)]) in
  let a22 = Set.cardinal a20 in 
  let a24 = Set.cardinal s1 in
  // let a23 = Map.size a21 in 
  let a23 = 0 in
  let a = 
    4 + 5 +  (* 9 *)
    40 / 5 + (* 9 + 8 = 17 *)
    x +      (* 17 + 2 + 3 + 6 = 28 *)
    y +      (* 28 + 9  = 37 *)
    z +      (* 37 + 2 = 39 *)
    a1 +     (* 39 + 1 + 2 + 3 + 4 + 5 = 54 *)
    a2 +     (* 44 + 5 = 59 *)
    a3 +     (* 59 + 6 = 65 *) 
    a5 +     (* 65 + 4 = 69 *)
    a6 +     (* 69 + 5 = 74 *) 
    a8 +     (* 74 + 15 = 89 *)
    a9 +     (* 89 + 5 = 94 *)
    a10 +    (* 94 + 266 = 360 *)
    a12 +    (* 360 + 4 = 364 *)
    a15 +     (* 364 + 44 = 408 *)
    a16 +    (* 408 + 55 = 463 *)
    a17 +     (* 463 + 9 = 472 *)
    a18 +      (* 472 + 5 = 477 *)
    a19 +    (* 477 + 5 = 482 *)
    a22 +    (* 482 + 2 = 484 *)
    a23 +     (* 484 + 2 = 486 *)
    a24 +     (*     + 3 = *)
    // 1000 
    0
  in 
  ([]: operation list), a
