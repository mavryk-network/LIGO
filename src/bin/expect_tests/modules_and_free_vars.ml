open Cli_expect

let contract basename =
  "../../test/contracts/modules_and_free_vars/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "simple.mligo" ] ;
  [%expect {|
    module Tezo = struct
     const amoun = 1000000mutez
    end
    const balanc = 2000000mutez
    const size = 10
    const bal = ADD(balanc ,
    1000000mutez)
    const amt = ADD(Tezo.amoun ,
    1000000mutez)
    type parameter = sum[Decrement -> unit , Increment -> unit]
    type storage = tez
    type return = ( list (operation) * tez )
    const main = lambda (gen#1) return let gen#8 = gen#1 in  match gen#8 with
                                                              | ( action , gen#2 ) ->
                                                              ( LIST_EMPTY() , let gen#10 = action in  match
                                                                        gen#10 with
                                                                        | Decrement unit_proj#11 ->
                                                                        amt
                                                                        | Increment unit_proj#12 ->
                                                                        bal ) |}]
let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "nested_modules.mligo" ] ;
  [%expect {|
    module Tezo = struct
     module X = struct
      module Y = struct
       const amoun = 1000000mutez
      end
     end
    end
    const balanc = 2000000mutez
    const size = 10
    const bal = ADD(balanc ,
    1000000mutez)
    const amt = ADD(Tezo.X.Y.amoun ,
    1000000mutez)
    type parameter = sum[Decrement -> unit , Increment -> unit]
    type storage = tez
    type return = ( list (operation) * tez )
    const main = lambda (gen#1) return let gen#8 = gen#1 in  match gen#8 with
                                                              | ( action , gen#2 ) ->
                                                              ( LIST_EMPTY() , let gen#10 = action in  match
                                                                        gen#10 with
                                                                        | Decrement unit_proj#11 ->
                                                                        amt
                                                                        | Increment unit_proj#12 ->
                                                                        bal ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "module_with_free_vars.mligo" ] ;
  [%expect {|
    const x = 1000000mutez
    module Tezo = struct
     const amoun = x
    end
    const balanc = 2000000mutez
    const size = 10
    const bal = ADD(balanc ,
    1000000mutez)
    const amt = ADD(Tezo.amoun ,
    1000000mutez)
    type parameter = sum[Decrement -> unit , Increment -> unit]
    type storage = tez
    type return = ( list (operation) * tez )
    const main = lambda (gen#1) return let gen#8 = gen#1 in  match gen#8 with
                                                              | ( action , gen#2 ) ->
                                                              ( LIST_EMPTY() , let gen#10 = action in  match
                                                                        gen#10 with
                                                                        | Decrement unit_proj#11 ->
                                                                        amt
                                                                        | Increment unit_proj#12 ->
                                                                        bal ) |}]

let%expect_test _ =
run_ligo_good [ "print" ; "ast-typed" ; contract "nested_modules_with_free_vars.mligo" ] ;
[%expect {|
  const used = 1000000mutez
  const unused = 2000000mutez
  module Tezo = struct
   const used = used
   const unused = unused
   module X = struct
    const used = used
    const unused = unused
    module Y = struct
     const used = used
     const unused = unused
    end
   end
  end
  const used = Tezo.X.Y.used
  const unused = Tezo.X.Y.unused
  type parameter = sum[Decrement -> unit , Increment -> unit]
  type storage = tez
  type return = ( list (operation) * tez )
  const main = lambda (gen#1) return let gen#8 = gen#1 in  match gen#8 with
                                                            | ( action , gen#2 ) ->
                                                            ( LIST_EMPTY() , let gen#10 = action in  match
                                                                      gen#10 with
                                                                      | Decrement unit_proj#11 ->
                                                                      1000000mutez
                                                                      | Increment unit_proj#12 ->
                                                                      used ) |}]