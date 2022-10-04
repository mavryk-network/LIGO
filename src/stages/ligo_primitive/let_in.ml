module type Attr = sig
  type t [@@deriving eq, compare, yojson, hash]

  include Pretty_printer.S with type t := t
end

module Make (Attr : Attr) = struct
  type ('e, 't) t =
    { let_binder : 't Binder.t
    ; rhs : 'e
    ; let_result : 'e
    ; attributes : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let pp f g ppf { let_binder; rhs; let_result; attributes = attr } =
    Format.fprintf
      ppf
      "@[<v>let %a = %a%a in@,%a@]"
      (Binder.pp g)
      let_binder
      f
      rhs
      Attr.pp
      attr
      f
      let_result


  let pp_mut f g ppf { let_binder; rhs; let_result; attributes = attr } =
    Format.fprintf
      ppf
      "@[<v>let mut %a = %a%a in@,%a@]"
      (Binder.pp g)
      let_binder
      f
      rhs
      Attr.pp
      attr
      f
      let_result


  let fold_map
      :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc
      -> ('a, 'c) t -> 'acc * ('b, 'd) t
    =
   fun f g acc { let_binder; rhs; let_result; attributes } ->
    let acc, let_binder = Binder.fold_map g acc let_binder in
    let acc, rhs = f acc rhs in
    let acc, let_result = f acc let_result in
    acc, { let_binder; rhs; let_result; attributes }
end
