module Name = struct
  let name = "alpha"
end

module Proto = Tezos_protocol_001_PtAtLas
module Alpha_environment = Tezos_protocol_environment_001_PtAtLas
module Raw_protocol = Tezos_raw_protocol_001_PtAtLas
module Parameters = Tezos_protocol_001_PtAtLas_parameters
module Client = Tezos_client_001_PtAtLas
module Test_helpers = Tezos_001_PtAtLas_test_helpers

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult

module Alpha_error_monad = Alpha_environment.Error_monad
include Proto
