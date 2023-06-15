module type I = sig
  type storage
  type dyn_param
  
  (* generated from dyn_param : this type is transparent to the user *)
  type dyn_storage (* =
    { storage : storage
    ; dynamic_entries = (nat, (dyn_param -> storage -> operation list * storage)) big_map
    } *)
  
  (* conditions on dynamic_entries update : generated if not present *)
  val condition_on_set_double : storage -> bool
  val condition_on_set_zero_if_gt : storage -> bool

  (* generated using Dynamic_entrypoints_helpers module in std_lib *)
  [@entry] val set_double : (unit -> storage -> operation list * storage) -> dyn_storage -> operation list * dyn_storage (* setter *)
  [@entry] val set_zero_if_gt : (int -> storage -> operation list * storage) -> dyn_storage -> operation list * dyn_storage (* setter *)
  [@entry] val double : unit -> dyn_storage -> operation list * dyn_storage (* caller *)
  [@entry] val zero_if_gt : int -> dyn_storage -> operation list * dyn_storage (* caller *)
  [@entry] val replace : int -> dyn_storage -> operation list * dyn_storage (* wrapped original entry *)
  [@view] val v1 : unit -> dyn_storage -> int (* wrapped original view *)

  val init_dynamic_entries :
    (unit -> storage -> operation list * storage)
    -> (int -> storage -> operation list * storage)
    -> (nat, (dyn_param -> storage -> operation list * storage)) big_map
end

module My_contract : I = struct
  type storage = int
  [@dyn_entry] type dyn_param = Double | Zero_if_gt of int
  [@entry] let replace (p: int) (_s : storage) : operation list * storage = [],p
  [@view] let v1 () (s:storage) : int = s + 1

  let condition_on_set_double = fun (_:storage) -> true

  (* TODO: generate this as well (ez) *)
  let init_dynamic_entries double_impl zero_if_gt_impl =
    (Big_map.literal
      [ (enum_double, Dynamic_entrypoints_helpers.wrap_dynamic_entry get_double double_impl)
      ; (enum_zero_if_gt, Dynamic_entrypoints_helpers.wrap_dynamic_entry get_zero_if_gt zero_if_gt_impl) ]
    : (nat, (dyn_param -> storage -> operation list * storage)) big_map)
end


(* Some test *)
let test =
  let double_impl = fun () s : operation list * int  ->
    [],s*2
  in
  let double_impl2 = fun () s : operation list * int  ->
    [], s*4
  in
  let zero_if_gt_impl : int -> int -> operation list * int = fun x s ->
    if s > x then [],0 else [],s
  in
  let dynamic_entries = My_contract.init_dynamic_entries double_impl zero_if_gt_impl in
  let init_storage = { storage = 1 ; dynamic_entries } in
  let addr,_code,size = Test.originate_module (contract_of My_contract) init_storage 0tez in
  let () = Test.log size in
  let contr = Test.to_contract(addr) in
  let _ = Test.transfer_to_contract_exn contr (Double) 1mutez in
  let () =
    let {storage ; dynamic_entries = _ } = Test.get_storage addr in
    Test.assert (storage = 2)
  in
  let _ = Test.transfer_to_contract_exn contr (Set_double double_impl2) 1mutez in
  let _ = Test.transfer_to_contract_exn contr (Double) 1mutez in
  let () =
    let {storage ; dynamic_entries = _ } = Test.get_storage addr in
    Test.assert (storage = 8)
  in
  let _ = Test.transfer_to_contract_exn contr (Replace 42) 1mutez in
  let () =
    let {storage ; dynamic_entries = _ } = Test.get_storage addr in
    Test.assert (storage = 42)
  in
  ()