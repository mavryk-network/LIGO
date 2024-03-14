let contract_env =
  { now           = "2020-01-01T00:00:00Z"
  ; balance       = 1tez
  ; amount        = 2tez
  ; self          = "KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b"
  ; source        = "mv1QdgAoi2FRPYuZXsbSKG8sfJ5QMZif5Fwq"
  ; sender        = "mv1QdgAoi2FRPYuZXsbSKG8sfJ5QMZif5Fwq"
  ; chain_id      = "NetXH12Aer3be93"
  ; level         = 10000
  ; voting_powers = Map.literal
      [ "mv1E97cthY1QUw8D1LuWNDiYzG8EGacuVt2K", 40
      ; "mv1QdgAoi2FRPYuZXsbSKG8sfJ5QMZif5Fwq", 60
      ]
  }

let config =
  { parameter    = "some_param"
  ; storage      = "some_storage"
  ; program      = "main.mligo"
  ; entrypoint   = "main"
  ; module_name  = "default"
  ; log_dir      = "tmp/contract.log"
  ; contract_env = contract_env
  }
