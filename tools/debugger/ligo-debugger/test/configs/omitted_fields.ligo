// MAVRYK: PascaLIGO. PascaLIGO port of omitted_fields.mligo (must resolve identically).
const contract_env = record [
  now     = "2020-01-01T00:00:00Z";
  balance = 1mav;
  amount  = 2mav;
  self    = "KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b";
  source  = "mv1QdgAoi2FRPYuZXsbSKG8sfJ5QMZif5Fwq"
]

const config = record [
  parameter    = "some_param";
  module_name  = "default";
  log_dir      = "tmp/contract.log";
  contract_env = contract_env
]
