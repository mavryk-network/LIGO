open Simple_utils.Trace
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options

let generator_to_variant ~raise s =
  if String.equal s "list"
  then `Generator_list
  else if String.equal s "random"
  then `Generator_random
  else raise.error @@ Main_errors.main_invalid_generator_name s


