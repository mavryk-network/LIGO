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
  let _ = if "ca" > "cat" then 
    log ("hi cat\n")
  else 
    log ("no cats\n")
  in
  let () = log ("wat: " ^ string_of_int 123456 ^ "\n") in
  let () = log ("foo " ^ "bar" ^ "!\n") in
  let () = log ((String.sub 0n 200n "abcd") ^ "\n") in
  let () = log "oh hai" in
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

  let a13 = List.iter (fun x -> assert (x < 10)) [3; 5; 7; 9] in
  let _ = log "\n" in
  let _ = List.iter (fun i -> log ("log test:" ^ string_of_int i ^ "\n")) [2; 0; 4; 6] in

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
  let a17 = List.head_opt [9; 7; 8; 9; 10] in
  let a17 = match a17 with 
    Some s -> s
  | None -> 0
  in
  let a18 = List.length [3; 4; 5; 6; 7] in
  let _ = log("List size:" ^ string_of_int (int(a18)) ^"\n") in
(*
           50
      30 
  20        40
    25   35     45
      
*)

  let s1 = Set.literal [45; 35; 25; 20; 40; 30; 50] in
  let m1 = Map.literal [(3, 55); (5, 50)] in
  let xx = Set.cardinal s1 + Map.size m1 in
  let a19: int = int xx in
  let a20 = Set.remove 30 s1 in
  let a21 = Map.remove 50 (Map.literal [(20, 1); (50, 2); (30, 3)]) in
  let a22 = Set.cardinal a20 in 
  let a24 = Set.cardinal s1 in
  let a23 = Map.size a21 in 
  let a25 = String.length "12" in 
  let () = if a25 = 2n then 
     log "yes 2 "
  else 
    log "not two"
  in
  let () = if (no_of_digits 5535) = 4 then 
    log "\noooh\n"
  else 
    log "\nnooo!\n"
  in
  let _ = Set.iter (fun f -> 
    log ("s1 item: " ^ string_of_int f ^ "\n")
  ) s1
  in 
  (* SOMETHING GOES WRONG HERE... *)
  let _ = Map.iter (fun (f, a) -> 
    log ("m1 item: " ^ string_of_int f ^ " = " ^ string_of_int a ^ "\n")
  ) m1
  in
  let _ = if (Set.mem 334 (Set.literal [34; 3; 33;])) then 
    log "found an item!"
  else 
    log "found no item :("
  in
  let _ = if Map.mem 5 m1 then 
    log "found an item!"
  else 
    log "found no item :("
  in
  let sum (acc, i : int * int) : int = acc + i in
  let a26 : int = Set.fold sum s1 0 in
  let a28: int = Set.fold sum (Set.literal [1; 2; 3]) 0 in
  let a27 : int = Set.fold_desc sum (Set.literal [1; 2; 3; 4; 5]) 0 in

  let _ = Set.iter (fun f -> log ("update test: " ^ string_of_int f ^ "\n")) (Set.update 5 false (Set.literal [4; 5; 6])) in
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
    a19 +    (* 477 + 9 = 486 *)
    a22 +    (* 486 + 6 = 492 *)
    a25 +    (* 492 + 2 = 494 *)
    a26 +    (* 494 + 245 = 739 *)
    a27 +    (* 739 + 15 = 754 *)
    a28 +   (* 754 + 6 = 760 *)
    a23 +     (* 484 + 2 = 762 *)
    a24 +     (*     + 7 = 769 *)
    // 1000 
    
    0
  in 
  let () =  log ("Result:" ^ string_of_int a ^ "\n") in
  ([]: operation list), 0
