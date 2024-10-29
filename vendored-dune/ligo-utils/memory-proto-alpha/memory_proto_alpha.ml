module Proto = Mavryk_protocol_002_PtBoreas
module Alpha_environment = Mavryk_protocol_environment_002_PtBoreas
module Raw_protocol = Mavryk_raw_protocol_002_PtBoreas
module Parameters = Mavryk_protocol_002_PtBoreas_parameters
module Client = Mavryk_client_002_PtBoreas
module Test_helpers = Mavryk_002_PtBoreas_test_helpers

(* Alcotezt redirects [Format.std_formatter] and [Format.err_formatter]
   to a buffer, we need to redirect them back to [stdout] and [stderr] *)

let redirect_formatter fmt ~oc =
  Format.pp_set_formatter_output_functions fmt (output_substring oc) (fun () -> flush oc)

let () =
  redirect_formatter ~oc:stdout Format.std_formatter;
  redirect_formatter ~oc:stderr Format.err_formatter

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult

module Alpha_error_monad = Alpha_environment.Error_monad
include Proto

let protocol_str = "boreas"
let protocol_def_str = "BOREAS"
