// MAVRYK: PascaLIGO. PascaLIGO port of config_with_computations.mligo (must resolve identically).
// Reuses the shared common.mligo module via cross-syntax #import.
#import "common.mligo" "Common"

function mult (const a : int; const b : int) : int is a * b

const config = record [
  parameter    = (
    block {
      const a : int = mult (100, 2);
      const b : string = Common.some_string
    } with record [ a = a; b = b ]
  );
  storage      = unit;
  entrypoint   = Common.entrypoint;
  contract_env = record [ amount = Common.some_mav ]
]
