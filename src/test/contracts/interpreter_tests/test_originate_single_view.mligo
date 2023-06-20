let () = Test.unset_print_values ()

module Bar = struct
  module Foo = struct
    type storage = int
    [@entry] let add n s : operation list * storage = [], s + n
    [@entry] let sub n s : operation list * storage = [], s - n
    [@view] let get () s : int = s
  end
end

let test =
  let ta, _, _ = Test.originate_module (contract_of Bar.Foo) 0 0tez in
  let c : (Bar.Foo parameter_of) contract = Test.to_contract ta in
  let _ = Test.transfer_to_contract_exn c (Add 42) 0tez in
  ()
