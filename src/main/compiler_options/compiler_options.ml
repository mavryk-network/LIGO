module Protocols = Protocols

type t = {
  infer : bool ;
  libs : string list ;
  test : bool ;
  protocol_version : Protocols.t
}

let make : 
  ?infer : bool ->
  ?libs:string list ->
  ?protocol_version:Protocols.t ->
  ?test:bool -> unit -> t =
  fun 
    ?(infer = false)
    ?(libs = ([]:string list))
    ?(protocol_version=Protocols.current)
    ?(test = false) () ->
      {
        infer;
        libs ;
        protocol_version;
        test ;
      }
