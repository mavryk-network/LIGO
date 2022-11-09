module Path = Command.Private.Path

module For_unix = Command.Private.For_unix (struct
    module Signal = Signal
    module Thread = Caml_threads.Thread
    module Time = Time

    module Unix = struct
      include Ligo_unix
      let unsafe_getenv = Sys.getenv
    end

    module Version_util = Version_util
  end)

let run = For_unix.run
let shape = For_unix.shape

module Deprecated = struct
  let run = For_unix.deprecated_run
end

module Shape = struct
  let help_text = For_unix.help_for_shape
end
