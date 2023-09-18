module Location = Simple_utils.Location

module type Container = sig
  type 'a t [@@deriving eq, compare, yojson, hash, sexp]

  val iter : 'a t -> f:('a -> unit) -> unit
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'b * 'c t
  val to_list : 'a t -> (Label.t * 'a) list
end

module type Decorator = sig
  type 'a t [@@deriving eq, compare, yojson, hash, sexp]

  val get_value : 'a t -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val map_acc : 'a t -> f:('a -> 'acc * 'b) -> 'acc * 'b t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type Typed = sig
  type 't t [@@deriving eq, compare, yojson, hash, sexp]

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val pp : (Format.formatter -> 't -> unit) -> Format.formatter -> 't t -> unit
end

module type S = sig
  type 't t [@@deriving eq, compare, yojson, hash]

  module Decorator : Decorator
  module Container : Container
  module Typed : Typed

  val fold_pattern : ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a
  val fold_map_pattern : ('a -> 'b t -> 'a * 'b t) -> 'a -> 'b t -> 'a * 'b t
  val map_pattern : ('a t -> 'a t) -> 'a t -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold_map : ('a -> 'b -> 'a * 'b) -> 'a -> 'b t -> 'a * 'b t
  val binders : 'a t -> 'a Binder.t list
  val var : loc:Location.t -> 'a Binder.t -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val record_pattern : loc:Location.t -> 'ty t Container.t -> 'ty t
  val variant_pattern : loc:Location.t -> Label.t * 'ty t -> 'ty t
  val list_prefix_pattern : loc:Location.t -> 'ty t list -> 'ty t -> 'ty t
  val list_wrap_pattern : loc:Location.t -> 'ty t -> 'ty t
end

module Make (Container : Container) (Decorator : Decorator) (Typed : Typed) = struct
  type 'ty_exp list_pattern =
    | Cons of 'ty_exp t * 'ty_exp t
    | Nil

  and 'ty_exp pattern_repr =
    | P_unit
    | P_var of 'ty_exp Binder.t
    | P_list of 'ty_exp list_pattern
    | P_variant of Label.t * 'ty_exp t
    | P_tuple of 'ty_exp t Decorator.t list
    | P_record of 'ty_exp t Container.t
    | P_typed of 'ty_exp t * 'ty_exp Typed.t

  and 't t = 't pattern_repr Location.wrap [@@deriving eq, compare, yojson, hash, sexp]

  module Container = Container
  module Decorator = Decorator
  module Typed = Typed

  let var : loc:Location.t -> 'ty Binder.t -> 'ty t =
   fun ~loc b -> Location.wrap ~loc (P_var b)


  let record_pattern : loc:Location.t -> 'ty t Container.t -> 'ty t =
   fun ~loc b -> Location.wrap ~loc (P_record b)


  let variant_pattern : loc:Location.t -> Label.t * 'ty t -> 'ty t =
   fun ~loc (label, pat) -> Location.wrap ~loc (P_variant (label, pat))


  let list_prefix_pattern : loc:Location.t -> 'ty t list -> 'ty t -> 'ty t =
   fun ~loc elts tail ->
    List.fold_right elts ~init:tail ~f:(fun p q ->
        Location.wrap ~loc (P_list (Cons (p, q))))


  let list_wrap_pattern : loc:Location.t -> 'ty t -> 'ty t =
   fun ~loc elt ->
    Location.wrap ~loc @@ P_list (Cons (elt, Location.wrap ~loc @@ P_list Nil))


  let rec pp_list g ppf pl =
    let mpp = pp g in
    match pl with
    | Cons (pl, pr) -> Format.fprintf ppf "%a::%a" mpp pl mpp pr
    | Nil -> Format.fprintf ppf "[]"


  and pp type_expression ppf p =
    let open Format in
    match Location.unwrap p with
    | P_unit -> fprintf ppf "()"
    | P_var b -> fprintf ppf "%a" (Binder.pp type_expression) b
    | P_list l -> pp_list type_expression ppf l
    | P_variant (l, p) -> fprintf ppf "%a %a" Label.pp l (pp type_expression) p
    | P_tuple pl ->
      fprintf
        ppf
        "(%a)"
        Simple_utils.PP_helpers.(list_sep (Decorator.pp (pp type_expression)) (tag ","))
        pl
    | P_record lps ->
      let aux ppf (l, p) = fprintf ppf "%a = %a" Label.pp l (pp type_expression) p in
      fprintf
        ppf
        "{ %a }"
        Simple_utils.PP_helpers.(list_sep aux (tag " ; "))
        (Container.to_list lps)
    | P_typed (p, t) ->
      fprintf ppf "%a : %a" (pp type_expression) p (Typed.pp type_expression) t


  let rec iter : ('a -> unit) -> 'a t -> unit =
   fun f p ->
    match Location.unwrap p with
    | P_unit -> ()
    | P_var b -> Binder.iter f b
    | P_list (Cons (pa, pb)) ->
      iter f pa;
      iter f pb
    | P_list Nil -> ()
    | P_variant (_, p) -> iter f p
    | P_tuple lp -> List.iter ~f:(iter f) (List.map ~f:Decorator.get_value lp)
    | P_record lps -> Container.iter ~f:(iter f) lps
    | P_typed (p, _) -> iter f p


  let rec fold_pattern : ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a =
   fun f acc p ->
    match Location.unwrap p with
    | P_unit -> f acc p
    | P_var _ -> f acc p
    | P_list lp ->
      (match lp with
      | Cons (pa, pb) -> fold_pattern f (fold_pattern f acc pb) pa
      | Nil -> acc)
    | P_variant (_, p) -> fold_pattern f acc p
    | P_tuple lp ->
      List.fold_left ~f:(fold_pattern f) ~init:acc @@ List.map ~f:Decorator.get_value lp
    | P_record lps -> Container.fold ~f:(fold_pattern f) ~init:acc lps
    | P_typed (p, _) -> fold_pattern f acc p


  let rec fold_map_pattern : ('a -> 'b t -> 'a * 'c t) -> 'a -> 'b t -> 'a * 'c t =
   fun f acc p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_unit -> f acc p
    | P_var _ -> f acc p
    | P_list lp ->
      (match lp with
      | Cons (pa, pb) ->
        let acc, pa = fold_map_pattern f acc pa in
        let acc, pb = fold_map_pattern f acc pb in
        acc, Location.wrap ~loc (P_list (Cons (pa, pb)))
      | Nil -> acc, Location.wrap ~loc (P_list Nil))
    | P_variant (l, p) ->
      let acc, lp = fold_map_pattern f acc p in
      acc, Location.wrap ~loc (P_variant (l, lp))
    | P_tuple lp ->
      let acc, lp =
        List.fold_map
          ~f:(fun v -> Decorator.map_acc ~f:(fold_map_pattern f v))
          ~init:acc
          lp
      in
      acc, Location.wrap ~loc (P_tuple lp)
    | P_record lps ->
      let acc, lps = Container.fold_map ~f:(fold_map_pattern f) ~init:acc lps in
      acc, Location.wrap ~loc (P_record lps)
    | P_typed (p, t) ->
      let acc, p = fold_map_pattern f acc p in
      acc, Location.wrap ~loc (P_typed (p, t))


  let map_pattern f p = snd @@ fold_map_pattern (fun () x -> (), f x) () p

  let rec fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a =
   fun f acc p ->
    match Location.unwrap p with
    | P_unit -> acc
    | P_var b -> Binder.fold f acc b
    | P_list lp ->
      (match lp with
      | Cons (pa, pb) -> fold f (fold f acc pb) pa
      | Nil -> acc)
    | P_variant (_, p) -> fold f acc p
    | P_tuple lp ->
      List.fold_left ~f:(fold f) ~init:acc @@ List.map ~f:Decorator.get_value lp
    | P_record lps -> Container.fold ~f:(fold f) ~init:acc lps
    | P_typed (p, t) -> fold f acc p


  let rec map : type a b. (a -> b) -> a t -> b t =
   fun f p ->
    let self = map f in
    let aux p =
      match p with
      | P_unit -> P_unit
      | P_var b ->
        let b' = Binder.map f b in
        P_var b'
      | P_list lp ->
        let lp =
          match lp with
          | Cons (pa, pb) ->
            let pa = self pa in
            let pb = self pb in
            (Cons (pa, pb) : b list_pattern)
          | Nil -> (Nil : b list_pattern)
        in
        P_list lp
      | P_variant (l, p) ->
        let p = self p in
        P_variant (l, p)
      | P_tuple lp ->
        let lp = List.map ~f:(Decorator.map ~f:self) lp in
        P_tuple lp
      | P_record lps ->
        let lps = Container.map ~f:self lps in
        P_record lps
      | P_typed (p, t) ->
        let p = self p in
        P_typed (p, Typed.map ~f t)
    in
    Location.map aux p


  let rec fold_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t =
   fun f acc p ->
    let self = fold_map f in
    let ret a wrap_content = a, { p with wrap_content } in
    match p.wrap_content with
    | P_unit -> ret acc P_unit
    | P_var b ->
      let acc, b = Binder.fold_map f acc b in
      ret acc @@ P_var b
    | P_list lp ->
      let acc, lp =
        match lp with
        | Cons (pa, pb) ->
          let acc, pa = self acc pa in
          let acc, pb = self acc pb in
          acc, (Cons (pa, pb) : 'b list_pattern)
        | Nil -> acc, (Nil : 'b list_pattern)
      in
      ret acc @@ P_list lp
    | P_variant (l, p) ->
      let acc, p = self acc p in
      ret acc @@ P_variant (l, p)
    | P_tuple lp ->
      let acc, lp =
        List.fold_map ~f:(fun v -> Decorator.map_acc ~f:(self v)) ~init:acc lp
      in
      ret acc @@ P_tuple lp
    | P_record lps ->
      let acc, lps = Container.fold_map ~f:self ~init:acc lps in
      ret acc @@ P_record lps
    | P_typed (p, t) ->
      let acc, p = self acc p in
      ret acc @@ P_typed (p, t)


  let binders t =
    fold_pattern
      (fun binders t ->
        match Location.unwrap t with
        | P_var binder -> binder :: binders
        | _ -> binders)
      []
      t
end

module Id = struct
  type 'a t = 'a [@@deriving eq, compare, yojson, hash, sexp]

  let map x ~f = f x
  let pp pp_t ppf t = Format.fprintf ppf "%a" pp_t t
end

module Void = struct
  type 'a t = | [@@deriving eq, compare, yojson, hash, sexp]

  let map (x : _ t) ~f:_ =
    match x with
    | _ -> .


  let pp _ _ (x : _ t) =
    match x with
    | _ -> .
end

module Non_linear_pattern =
  Make
    (Label.Assoc)
    (struct
      type 'a t = 'a [@@deriving eq, compare, yojson, hash, sexp]

      let get_value x = x
      let map x ~f = f x
      let map_acc x ~f = f x
      let pp ppf x = ppf x
    end)
    (Id)

module Linear_pattern =
  Make
    (Record)
    (struct
      type 'a t = 'a [@@deriving eq, compare, yojson, hash, sexp]

      let get_value x = x
      let map x ~f = f x
      let map_acc x ~f = f x
      let pp ppf x = ppf x
    end)
    (Void)

module Linear_pattern_with_ellipsis = struct
  include
    Make
      (Record)
      (struct
        type 'a t = 'a * bool [@@deriving eq, compare, yojson, hash, sexp]

        let get_value (x, _) = x
        let map (x, d) ~f = f x, d

        let map_acc (x, d) ~f =
          let acc, fx = f x in
          acc, (fx, d)


        let pp ppfx ppf = function
          | x, false -> ppfx ppf x
          | x, true -> Format.fprintf ppf "...%a" ppfx x
      end)
      (Id)

  let get_list_of_tuple_pattern (v : 'a t Decorator.t list) =
    let open Simple_utils.Option in
    let open Let_syntax in
    let rec aux v =
      match v with
      | [] -> None
      | [ (pattern, b) ] -> return ([], (pattern, b))
      | (pattern, false) :: tl ->
        let%bind init, last = aux tl in
        return (pattern :: init, last)
      | _ -> None
    in
    aux v


  let get_rest_list_of_tuple_pattern (v : 'a t Decorator.t list) =
    let open Simple_utils.Option in
    let open Let_syntax in
    match%bind get_list_of_tuple_pattern v with
    | init, (last, true) -> return (init, last)
    | _ -> None


  let get_expr_list_of_tuple_pattern (v : 'a t Decorator.t list) =
    let open Simple_utils.Option in
    let open Let_syntax in
    match%bind get_list_of_tuple_pattern v with
    | init, (last, false) -> return (init, last)
    | _ -> None


  let get_list_of_pattern (v : 'a t) =
    let open Simple_utils.Option in
    let open Let_syntax in
    match Location.unwrap v with
    | P_tuple v -> return @@ get_list_of_tuple_pattern v
    | _ -> None
end
