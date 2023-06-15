module type I = sig
  type storage
  type dyn_param
  
  (* generated from dyn_param *)
  type dyn_storage (* =
    { storage : storage
    ; dynamic_entries = (nat, (dyn_param -> storage -> operation list * storage)) big_map
    } *)
  
  (* generated if not present *)
  val condition_on_set_double : storage -> bool
  val condition_on_set_zero_if_gt : storage -> bool

  (* generated using Dynamic_entrypoints_helpers module in std_lib *)
  [@entry] val set_double : (unit -> storage -> operation list * storage) -> dyn_storage -> operation list * dyn_storage (* setter *)
  [@entry] val set_zero_if_gt : (int -> storage -> operation list * storage) -> dyn_storage -> operation list * dyn_storage (* setter *)
  [@entry] val double : unit -> dyn_storage -> operation list * dyn_storage (* caller *)
  [@entry] val zero_if_gt : int -> dyn_storage -> operation list * dyn_storage (* caller *)
  [@entry] val replace : int -> dyn_storage -> operation list * dyn_storage (* wrapped original entry *)
  [@view] val v1 : unit -> dyn_storage -> int (* wrapped original view *)
end

module My_contract : I = struct
  type storage = int
  [@dyn_entry] type dyn_param = Double | Zero_if_gt of int
  [@entry] let replace (p: int) (_s : storage) : operation list * storage = [],p
  [@view] let v1 () (s:storage) : int = s + 1

  let condition_on_set_double = fun (_:storage) -> true
end
