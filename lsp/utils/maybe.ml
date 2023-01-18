open Linol_lwt.Jsonrpc2

module Maybe = struct
  include Option

  let ( let@ ) = bind

  let traverse : ('a -> 'b IO.t) -> 'a t -> 'b t IO.t =
   fun f -> function
    | None -> IO.return None
    | Some x ->
      IO.(
        let* t = f x in
        return (Some t))

  let sequence : 'a IO.t t -> 'a t IO.t = fun x -> traverse Fun.id x
end
