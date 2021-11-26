open Cli_expect

let contract basename =
  "../../test/contracts/modules_and_free_vars/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "simple.mligo" ] ;
  [%expect {|
    module Tezo = const amoun : tez = 1000000mutez
    const balanc : tez = 2000000mutez
    const size : int = 10
    const bal : tez = ADD(balanc ,
    1000000mutez)
    const amt : tez = ADD(Tezo.amoun ,
    1000000mutez)
    type parameter = sum[Decrement -> unit , Increment -> unit]
    type storage = tez
    type return = ( list (operation) * tez )
    const main : ( sum[Decrement -> unit , Increment -> unit] * tez ) ->
    ( list (operation) * tez ) = lambda (#1) return let #5 = #1 in  match
                                                                     #5 with
                                                                     | ( action , #2 ) ->
                                                                     ( LIST_EMPTY() , let #7 = action in  match
                                                                        #7 with
                                                                        | Decrement unit_proj#8 ->
                                                                        amt
                                                                        | Increment unit_proj#9 ->
                                                                        bal ) |}]
let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "nested_modules.mligo" ] ;
  [%expect {|
    module Tezo = module X = module Y = const amoun : tez = 1000000mutez
    const balanc : tez = 2000000mutez
    const size : int = 10
    const bal : tez = ADD(balanc ,
    1000000mutez)
    const amt : tez = ADD(Tezo.X.Y.amoun ,
    1000000mutez)
    type parameter = sum[Decrement -> unit , Increment -> unit]
    type storage = tez
    type return = ( list (operation) * tez )
    const main : ( sum[Decrement -> unit , Increment -> unit] * tez ) ->
    ( list (operation) * tez ) = lambda (#1) return let #5 = #1 in  match
                                                                     #5 with
                                                                     | ( action , #2 ) ->
                                                                     ( LIST_EMPTY() , let #7 = action in  match
                                                                        #7 with
                                                                        | Decrement unit_proj#8 ->
                                                                        amt
                                                                        | Increment unit_proj#9 ->
                                                                        bal ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "module_with_free_vars.mligo" ] ;
  [%expect {|
    const x : tez = 1000000mutez
    module Tezo = const amoun : tez = x
    const balanc : tez = 2000000mutez
    const size : int = 10
    const bal : tez = ADD(balanc ,
    1000000mutez)
    const amt : tez = ADD(Tezo.amoun ,
    1000000mutez)
    type parameter = sum[Decrement -> unit , Increment -> unit]
    type storage = tez
    type return = ( list (operation) * tez )
    const main : ( sum[Decrement -> unit , Increment -> unit] * tez ) ->
    ( list (operation) * tez ) = lambda (#1) return let #5 = #1 in  match
                                                                     #5 with
                                                                     | ( action , #2 ) ->
                                                                     ( LIST_EMPTY() , let #7 = action in  match
                                                                        #7 with
                                                                        | Decrement unit_proj#8 ->
                                                                        amt
                                                                        | Increment unit_proj#9 ->
                                                                        bal ) |}]

let%expect_test _ =
run_ligo_good [ "print" ; "ast-typed" ; contract "nested_modules_with_free_vars.mligo" ] ;
[%expect {|
  const used : tez = 1000000mutez
  const unused : tez = 2000000mutez
  module Tezo = const used : tez = used
                const unused : tez = unused
                module X = const used : tez = used
                           const unused : tez = unused
                           module Y = const used : tez = used
                                      const unused : tez = unused
  const used : tez = Tezo.X.Y.used
  const unused : tez = Tezo.X.Y.unused
  type parameter = sum[Decrement -> unit , Increment -> unit]
  type storage = tez
  type return = ( list (operation) * tez )
  const main : ( sum[Decrement -> unit , Increment -> unit] * tez ) ->
  ( list (operation) * tez ) = lambda (#1) return let #5 = #1 in  match
                                                                   #5 with
                                                                   | ( action , #2 ) ->
                                                                   ( LIST_EMPTY() , let #7 = action in  match
                                                                      #7 with
                                                                      | Decrement unit_proj#8 ->
                                                                      1000000mutez
                                                                      | Increment unit_proj#9 ->
                                                                      used ) |}]