
(* client stuff: defining a contract "normally" *)
module My_contract = struct
  type storage = int
  (* TODO : should we generate this type based on
  [@dyn_entry] type double = unit
  [@dyn_entry] type zero_if_gt = int 
  
  *)
  [@dyn_entry] type dyn_param = Double | Zero_if_gt of storage

  (**)
  // [@entry]
  let replace (p: int) (_s : storage) : operation list * storage = [],p
  // [@view]
  let v1 () (s:storage) : int = s + 1

  (* specialized conditions *)
  let condition_on_set_double : storage -> bool = fun _ -> true
  let condition_on_set_zero_if_gt : storage -> bool = fun s -> s > 2

(* The module is extended, after stripping any [@entry] / [@view] *)

  type dyn_storage = Dynamic_entrypoints_helpers.dyn_storage
  type dyn_storage = (storage, dyn_param -> storage -> operation list * storage) dyn_storage

  (* dyn setters *)
  [@entry] let set_double =
    Dynamic_entrypoints_helpers.setter
      condition_on_set_double
      enum_double
      get_double
  [@entry] let set_zero_if_gt =
    Dynamic_entrypoints_helpers.setter
      condition_on_set_zero_if_gt
      enum_zero_if_gt
      get_zero_if_gt
  (* forward the dyn entry *)
  [@entry] let double : unit -> dyn_storage -> operation list * dyn_storage =
    Dynamic_entrypoints_helpers.caller
      enum_double
      make_double
  [@entry] let zero_if_gt : int -> dyn_storage -> operation list * dyn_storage =
      Dynamic_entrypoints_helpers.caller
      enum_zero_if_gt
      make_zero_if_gt
  (* /forward the dyn entry *)

  (* forward the entry *)
  [@entry] let replace : int -> dyn_storage -> operation list * dyn_storage =
    Dynamic_entrypoints_helpers.forward_entry replace
  (* /forward the entry *)

  (* forward the views *)
  [@view] let v1 : unit -> dyn_storage -> int =
    Dynamic_entrypoints_helpers.forward_view v1
  (* /forward the views *)

  let init_dynamic_entries double_impl zero_if_gt_impl =
    Big_map.literal
      [ (enum_double, Dynamic_entrypoints_helpers.dyn_entry_to_main get_double double_impl)
      ; (enum_zero_if_gt, Dynamic_entrypoints_helpers.dyn_entry_to_main get_zero_if_gt zero_if_gt_impl) ]

end
(* /this module is generated, after the generation of $main, $contract, $views *)


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