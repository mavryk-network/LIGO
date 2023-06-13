
module Dynamic_entrypoints_helpers = struct

  type ('p,'s) ps_f = 'p -> 's -> operation list * 's


  type ('s, 'dyn_main) dyn_storage = {
    storage : 's
  ; dynamic_entries : (nat, 'dyn_main) big_map
  }

  (* partially apply on the first 2 arguments to get an entry lifted as a main function *)
  let dyn_entry_to_main
    (type storage dyn_main_param dyn_ep_param) =
      type entry_dyn = (dyn_ep_param, storage) ps_f in
      fun
        (getter : dyn_main_param -> dyn_ep_param option)
        (dyn_main : entry_dyn)
        p s : operation list * storage ->
          let p = Option.value_exn (-2) (getter p) in  
          dyn_main p s

  (* partially apply on the first 3 arguments to get a valid entry to set/update dynamic entries *)
  let setter
    (type storage dyn_ep_param dyn_main_param) =
      type entry_dyn = (dyn_ep_param, storage) ps_f in
      type main_dyn = (dyn_main_param ,storage) ps_f in
      type lazified_storage = (storage, main_dyn) dyn_storage in
      fun
        (condition_on_set : storage -> bool)
        (selector : nat)
        (getter : dyn_main_param -> dyn_ep_param option)
        (dyn_main : entry_dyn)
        (store : lazified_storage)
        : operation list * lazified_storage ->
          let f = dyn_entry_to_main getter dyn_main in
          if condition_on_set store.storage then (
            let dynamic_entries = Big_map.update selector (Some f) store.dynamic_entries in
            [], { store with dynamic_entries })
          else 
            failwith (-3)

  (* partially apply on the first 2 arguments to get a valid entry to a call a dynamic entries *)
  let caller
    (type storage dyn_ep_param dyn_main_param) =
      type main_dyn = (dyn_main_param, storage) ps_f in
      type lazified_storage = (storage, main_dyn) dyn_storage in 
      fun
        (selector : nat)
        (mk_param : dyn_ep_param -> dyn_main_param)
        (param : dyn_ep_param)
        (store : lazified_storage)
        : operation list * lazified_storage ->
          let f = match Big_map.find_opt selector store.dynamic_entries with Some x -> x | None -> failwith (-3) in
          let ops, storage = f (mk_param param) store.storage in
          ops, { store with storage }

  (* to be partially applied on the first argument to get a valid entry *)
  let forward_entry
    (type storage dyn_ep_param dyn_main_param) =
      type main_dyn = (dyn_main_param, storage) ps_f in
      type entry_dyn = dyn_ep_param -> storage -> operation list * storage in
      type lazified_storage = (storage, main_dyn) dyn_storage in 
      fun
        (client_entry: entry_dyn)
        (param : dyn_ep_param)
        (store: lazified_storage)
        : operation list * lazified_storage ->
          let ops, storage = client_entry param store.storage  in
          ops, { store with storage }

  let forward_view
    (type storage view_param view_return dyn_main_param) =
      type main_dyn = (dyn_main_param, storage) ps_f in
      type view_f = view_param -> storage -> view_return in
      type lazified_storage = (storage, main_dyn) dyn_storage in 
      fun
        (client_view: view_f)
        (view_param : view_param)
        (store: lazified_storage)
        : view_return ->
          client_view view_param store.storage

end



(* client stuff: defining a contract "normally" *)
module My_contract = struct
  type storage = int
  (* generated *)
  type dyn_param = Double | Zero_if_gt of storage
  (* generated *)

  (* Need to generate those ... *)
  let selector_double = 0n
  let selector_zero_if_gt = 1n
  let get_double x = match x with Double x -> Some x | _ -> None
  let get_zero_if_gt x = match x with Zero_if_gt x -> Some x | _ -> None
  let make_double x = Double x
  let make_zero_if_gt x = Zero_if_gt x
  (* Need to generate those ... *)

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
      selector_double
      get_double
  [@entry] let set_zero_if_gt =
    Dynamic_entrypoints_helpers.setter
      condition_on_set_zero_if_gt
      selector_zero_if_gt
      get_zero_if_gt
  (* forward the dyn entry *)
  [@entry] let double : unit -> dyn_storage -> operation list * dyn_storage =
    Dynamic_entrypoints_helpers.caller
      selector_double
      make_double
  [@entry] let zero_if_gt : int -> dyn_storage -> operation list * dyn_storage =
      Dynamic_entrypoints_helpers.caller
      selector_zero_if_gt
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
      [ (selector_double, Dynamic_entrypoints_helpers.dyn_entry_to_main get_double double_impl)
      ; (selector_zero_if_gt, Dynamic_entrypoints_helpers.dyn_entry_to_main get_zero_if_gt zero_if_gt_impl) ]

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