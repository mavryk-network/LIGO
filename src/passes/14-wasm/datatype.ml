[@@@warning "-27"]

module W = WasmObjectFile
module A = W.Ast
module S = W.Source
module T = W.Types

open Helpers

let uni_op: (S.region -> A.instr list) -> Env.t -> A.instr list -> Env.t * A.instr list = fun op env a ->
  (* TODO: allocate a block here for the result *)
  let at        = S.no_region in
  let load      = load at in
  let const     = const at in
  let call_s    = call_s at in 
  let store     = store at in
  let local_tee_s = local_tee_s at in
  let local_get_s = local_get_s at in
  let name  = unique_name "uni_op" in
  (Env.add_local env (name, T.NumType I32Type)), 
  [
    const 4l;
    call_s "malloc";
    local_tee_s name;
  ]
  @
  a
  @
  [
    load
  ]
  @
  op at
  @
  [
    store;
    local_get_s name
  ]

let bin_op: (S.region -> A.instr list) -> Env.t -> A.instr list -> A.instr list -> Env.t * A.instr list = fun op env a b ->
  let at        = cover_region a b in
  let load      = load at in
  let const     = const at in
  let call_s    = call_s at in 
  let store     = store at in
  let local_tee_s = local_tee_s at in
  let local_get_s = local_get_s at in
  let name  = unique_name "bin_op" in
  (Env.add_local env (name, T.NumType I32Type)), 
  [
    const 4l;
    call_s "malloc";
    local_tee_s name;
  ]
  @
  a
  @
  [
    load
  ]
  @
  b
  @
  [
    load;
  ]
  @ 
  op at
  @
  [
    store;
    local_get_s name
  ]

module Int = struct

  let neg env = bin_op (fun at -> [i32_mul at]) env [const S.no_region (-1l)]

  (* Math *)
  let add = bin_op (fun at -> [i32_add at]) 
  let sub = bin_op (fun at -> [i32_sub at])
  let mul = bin_op (fun at -> [i32_mul at])
  let div = bin_op (fun at -> [i32_div at])
  (* ??? let mod_ = bin_op (fun at -> [i32_mod at]) *) 

  (* Logic *)
  let and_ = bin_op (fun at -> [i32_and at])
  let or_  = bin_op (fun at -> [i32_or  at])
  let xor  = bin_op (fun at -> [i32_xor at])
  let lsl_ = bin_op (fun at -> [i32_lsl at])
  let lsr_ = bin_op (fun at -> [i32_lsr at])

  (* Comparator *)
  let eq  = bin_op (fun at -> [i32_eq  at])
  let ne  = bin_op (fun at -> [i32_ne  at])
  let lt  = bin_op (fun at -> [i32_lt  at])
  let gt  = bin_op (fun at -> [i32_gt  at])
  let le  = bin_op (fun at -> [i32_le  at])
  let ge  = bin_op (fun at -> [i32_ge  at])

end

module String = struct 


end

module Set = struct 
  (* dumbest version of a set... *)
  (* value left right *)
end

module List = struct 
  (*
    value;
    next_value / 0 if this was the last value
  *)
  let empty = [const S.no_region 0l]
  let literal l  = []
  let iter f l   = 
    [
      (* current_item = list;
      load;
      set_x;
      loop (
        get_x; 
        call_indirect func;
        set_x + 1;
        if x == 0 then break loop        
      ) *)
    ]

(* 
  let map f l    = 
  let fold f l i =
  let fold_left f i l = 
  let fold_right f l i = *)

end

module Pair = struct 

  let create w env e1 e2 = 
    let at     = cover_region e1 e2 in
    let const  = const at in 
    let call_s = call_s at in
    let local_set_s = local_set_s at in
    let local_get_s = local_get_s at in
    let store = store at in
    let i32_add = i32_add at in
    let pair = var_to_string (Value_var.fresh ~name:"C_PAIR" ()) in
    let e =
      [const 8l; call_s "malloc"; local_set_s pair; local_get_s pair]
      @ e1
      @ [store; local_get_s pair; const 4l; i32_add]
      @ e2
      @ [store; local_get_s pair]
    in
    (w, Env.add_local env (pair, T.NumType I32Type), e)


end

module Map = struct 
  (* dumbest version of a map *)

end