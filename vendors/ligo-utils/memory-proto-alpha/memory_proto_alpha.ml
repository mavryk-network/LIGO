module Name = struct let name = "alpha" end (*useless ?*)

module type IT = sig
  module Protocol : sig
    module Environment : sig
      module Pervasives : sig
        type ('ok, 'error) result = Ok of 'ok | Error of 'error
      end
      open Pervasives
      module Error_monad : sig
        type error = ..
        type 'a tzresult = ('a, error list) result
        val (>>?) : 'a tzresult -> ('a -> 'b tzresult) -> 'b tzresult
      end
      val wrap_error : 'a Error_monad.tzresult -> 'b Error_monad.tzresult
    end
  end
end

module Make (P : IT) = struct
  module Alpha_environment = P.Protocol.Environment
  type alpha_error = Alpha_environment.Error_monad.error
  type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult

  module Alpha_error_monad = Alpha_environment.Error_monad
  module Proto = P
  include Proto
end


module Protocol_edo = Make (Tezos_protocol_008_PtEdo2Zk)
module Protocol_alpha = Make (Tezos_protocol_alpha)

(* module Alpha_environment = Tezos_protocol_008_PtEdo2Zk.Protocol.Environment
type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult


module Alpha_error_monad = Alpha_environment.Error_monad
module Proto = Tezos_protocol_008_PtEdo2Zk
include Proto *)
