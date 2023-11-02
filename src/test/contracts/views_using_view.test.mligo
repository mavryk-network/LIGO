type param =
    Basic of address * address 
  | Not_funny of address
  | Get_storage of address
  | Get_address of address
  | Super_not_funny of address

type store = Address of address | Integer of int

let proxy (p,_ : param * store) : operation list * store
  = [], (match p with
          Basic (v,a)       -> Integer (Option.unopt (Mavryk.call_view "basic" a v))
        | Get_storage v     -> Integer (Option.unopt (Mavryk.call_view "get_storage" () v))
        | Not_funny v       -> Integer (Option.unopt (Mavryk.call_view "not_funny" () v) )
        | Get_address v     -> Address (Option.unopt (Mavryk.call_view "get_address" () v))
        | Super_not_funny v -> Integer (Option.unopt (Mavryk.call_view "super_not_funny" () v)))

let test_basic =
  let addr, _contr, _size = Test.originate_from_file "./views_using_view.jsligo" "main" 
    ["basic" ; "not_funny" ; "get_storage" ; "get_address" ; "super_not_funny"] (Test.eval 999) 1mav in
  let ta, _mcontr, _size = Test.originate proxy (Integer 1) 1mav in 
  let proxy_addr = Test.to_contract ta |> Mavryk.address in
  let _ = Test.transfer proxy_addr (Test.eval (Basic (addr, addr))) 1mav in
  let s = Test.get_storage ta in
  s = (Integer 999)

let test_not_funny =
  let addr, _contr, _size = Test.originate_from_file "./views_using_view.jsligo" "main" 
    ["basic" ; "not_funny" ; "get_storage" ; "get_address" ; "super_not_funny"] (Test.eval 999) 1mav in
  let ta, _mcontr, _size = Test.originate proxy (Integer 1) 1mav in 
  let proxy_addr = Test.to_contract ta |> Mavryk.address in
  let _ = Test.transfer proxy_addr (Test.eval (Not_funny addr)) 1mav in
  let s = Test.get_storage ta in
  s = (Integer 999)

let test_get_storage =
  let addr, _contr, _size = Test.originate_from_file "./views_using_view.jsligo" "main" 
    ["basic" ; "not_funny" ; "get_storage" ; "get_address" ; "super_not_funny"] (Test.eval 999) 1mav in
  let ta, _mcontr, _size = Test.originate proxy (Integer 1) 1mav in 
  let proxy_addr = Test.to_contract ta |> Mavryk.address in
  let _ = Test.transfer proxy_addr (Test.eval (Get_storage addr)) 1mav in
  let s = Test.get_storage ta in
  s = (Integer 999)

let test_get_address =
  let addr, _contr, _size = Test.originate_from_file "./views_using_view.jsligo" "main" 
    ["basic" ; "not_funny" ; "get_storage" ; "get_address" ; "super_not_funny"] (Test.eval 999) 1mav in
  let ta, _mcontr, _size = Test.originate proxy (Integer 1) 1mav in 
  let proxy_addr = Test.to_contract ta |> Mavryk.address in
  let _ = Test.transfer proxy_addr (Test.eval (Get_address addr)) 1mav in
  let s = Test.get_storage ta in
  s = Address proxy_addr

let test_super_not_funny =
  let addr, _contr, _size = Test.originate_from_file "./views_using_view.jsligo" "main" 
    ["basic" ; "not_funny" ; "get_storage" ; "get_address" ; "super_not_funny"] (Test.eval 999) 1mav in
  let ta, _mcontr, _size = Test.originate proxy (Integer 1) 1mav in 
  let proxy_addr = Test.to_contract ta |> Mavryk.address in
  let _ = Test.transfer proxy_addr (Test.eval (Super_not_funny addr)) 1mav in
  let s = Test.get_storage ta in
  s = (Integer (999 + 999))