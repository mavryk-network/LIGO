// All these fields are optional. You can omit them
// and they would be asked if needed.
// The only required object here is 'config'.

let contract_env =
  { now           = "2020-01-01T00:00:00Z"
  ; balance       = 1mav
  ; amount        = 2mav
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
  { parameter    = "*parameter value*" : parameter_type
    // Note that the types of parameter and storage
    // should be monomorphized at the config resolution stage.
  ; storage      = "*storage value*" : storage_type
  ; program      = "*path to program*"
  ; module_name  = "*module name*"
  ; entrypoint   = "*entrypoint name*"
  ; log_dir      = "*log directory*"
  ; contract_env = contract_env
  }
