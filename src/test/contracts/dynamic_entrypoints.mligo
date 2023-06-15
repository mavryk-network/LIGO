
(* client stuff: defining a contract "normally" *)
(* TODO: abstract over dyn_storage rhs "dyn_param -> storage -> operation list * storage", 
  it could be bytes as well ..
*)
module My_contract = struct
  type storage = int
  (* TODO : should we generate this type based on
  [@dyn_entry] type double = unit
  [@dyn_entry] type zero_if_gt = int 
  *)
  [@dyn_entry] type dyn_param = Double | Zero_if_gt of storage

  [@entry] let replace (p: int) (_s : storage) : operation list * storage = [],p
  [@view] let v1 () (s:storage) : int = s + 1

  (* generate this as well *)
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