[@@@warning "-27"]

module W = WasmObjectFile
module A = W.Ast
module S = W.Source
module T = W.Types
open Helpers

let uni_op : (S.region -> A.instr list) -> Env.t -> A.instr list -> Env.t * A.instr list =
 fun op env a ->
  let at = S.no_region in
  let load = load at in
  let const = const at in
  let call_s = call_s at in
  let store = store at in
  let local_tee_s = local_tee_s at in
  let local_get_s = local_get_s at in
  let i32_add = i32_add at in
  let unique_name = unique_name ~loc:Location.dummy in
  let name = unique_name "uni_op" in
  ( Env.add_local env (name, T.NumType I32Type)
  , [ const 5l
    ; call_s "malloc"
    ; local_tee_s name
    ; const 2l
    ; store
    ; local_get_s name
    ; const 1l
    ; i32_add
    ]
    @ a
    @ [ load ]
    @ op at
    @ [ store; local_get_s name ] )


let bin_op
    :  (S.region -> A.instr list) -> Env.t -> A.instr list -> A.instr list
    -> Env.t * A.instr list
  =
 fun op env a b ->
  let at = cover_region a b in
  let load = load at in
  let const = const at in
  let call_s = call_s at in
  let store = store at in
  let local_tee_s = local_tee_s at in
  let local_get_s = local_get_s at in
  let store8 = store8 at in
  let i32_add = i32_add at in
  let name = unique_name ~loc:Location.dummy "bin_op" in
  ( Env.add_local env (name, T.NumType I32Type)
  , [ const 5l
    ; call_s "malloc"
    ; local_tee_s name
    ; const 2l (* int tag *)
    ; store8
    ; local_get_s name
    ; const 1l
    ; i32_add
    ]
    @ a
    @ [ const 1l; i32_add; load ]
    @ b
    @ [ const 1l; i32_add; load ]
    @ op at
    @ [ store; local_get_s name ] )


let compare_bin_op
    :  (S.region -> A.instr list) -> Env.t -> A.instr list -> A.instr list
    -> Env.t * A.instr list
  =
 fun op env a b ->
  let at = cover_region a b in
  let load = load at in
  let const = const at in
  let i32_add = i32_add at in
  env, a @ [ const 1l; i32_add; load ] @ b @ [ const 1l; i32_add; load ] @ op at


module Int = struct
  let neg env = bin_op (fun at -> [ i32_mul at ]) env [ const S.no_region (-1l) ]

  (* Math *)
  let add = bin_op (fun at -> [ i32_add at ])
  let sub = bin_op (fun at -> [ i32_sub at ])
  let mul = bin_op (fun at -> [ i32_mul at ])
  let div = bin_op (fun at -> [ i32_div at ])
  (* ??? let mod_ = bin_op (fun at -> [i32_mod at]) *)

  (* Logic *)
  let and_ = bin_op (fun at -> [ i32_and at ])
  let or_ = bin_op (fun at -> [ i32_or at ])
  let xor = bin_op (fun at -> [ i32_xor at ])
  let lsl_ = bin_op (fun at -> [ i32_lsl at ])
  let lsr_ = bin_op (fun at -> [ i32_lsr at ])
end
