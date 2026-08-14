#import "gitlab-pages/docs/tutorials/taco-shop/src/mavryk-taco-shop-smart-contract/TacoShop.ligo" "TacoShop"

function assert_string_failure (const res : test_exec_result; const expected : string) : unit is
  block {
    const expected = Test.eval (expected);
  } with
    case res of [
      Fail (Rejected (actual, _u)) -> assert (Test.michelson_equal (actual, expected))
    | Fail (_e) -> failwith ("contract failed for an unknown reason")
    | Success (_s) -> failwith ("bad price check")
    ]

(* Auxiliary function for testing equality in maps *)
function eq_in_map (const r : TacoShop.taco_supply; const m : TacoShop.taco_shop_storage; const k : nat) : bool is
  case Map.find_opt (k, m) of [
    None -> False
  | Some (v) -> (v.current_stock = r.current_stock) and (v.max_price = r.max_price)
  ]

const test =
  block {
    (* originate the contract with a initial storage *)
    const init_storage = Map.literal (list [
        (1n, record [ current_stock = 50n ; max_price = 50mav ]) ;
        (2n, record [ current_stock = 20n ; max_price = 75mav ]) ;
      ]);
    const orig = Test.originate (contract_of TacoShop, init_storage, 0mav);

    (* Test inputs *)
    const clasico_kind : parameter_of TacoShop = Buy_taco (1n);
    const unknown_kind : parameter_of TacoShop = Buy_taco (3n);

    (* Purchasing a Taco with 1mav and checking that the stock has been updated *)
    const ok_case : test_exec_result = Test.transfer (orig.addr, clasico_kind, 1mav);
    const _ok_check =
      case ok_case of [
        Success (_s) ->
          block {
            const storage = Test.get_storage (orig.addr);
          } with assert (
                eq_in_map (record [ current_stock = 49n ; max_price = 50mav ], storage, 1n)
                and eq_in_map (record [ current_stock = 20n ; max_price = 75mav ], storage, 2n)
              )
      | Fail (_e) -> failwith ("ok test case failed")
      ];

    (* Purchasing an unregistred Taco *)
    const nok_unknown_kind = Test.transfer (orig.addr, unknown_kind, 1mav);
    const _u1 = assert_string_failure (nok_unknown_kind, "Unknown kind of taco");

    (* Attempting to Purchase a Taco with 2mav *)
    const nok_wrong_price = Test.transfer (orig.addr, clasico_kind, 2mav);
    const _u2 = assert_string_failure (nok_wrong_price, "Sorry, the taco you are trying to purchase has a different price");
  } with unit