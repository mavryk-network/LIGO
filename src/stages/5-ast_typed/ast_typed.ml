module Types = Types
module Environment = Environment
module PP = PP
module PP_annotated = PP_annotated
module Yojson = To_yojson
module Formatter = Formatter
module Reasons = Reasons
module Combinators = struct
  include Combinators
end
module Misc = struct
  include Misc
end
module Helpers = Helpers

include Types
include Misc
include Combinators
module Debug = Stage_common.Debug

let program_environment env program = fst (Compute_environment.program env program)
module Compare = struct include Compare end

module Typer_errors = Typer_errors
type typer_switch = Old | New
