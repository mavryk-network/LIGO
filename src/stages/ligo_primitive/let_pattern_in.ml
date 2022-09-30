type attributes = string list
(* must be Attr .. *)
  [@@deriving eq,compare,yojson,hash]

let pp_attributes ppf attributes =
  let attr =
    List.map ~f:(fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat
  in Format.fprintf ppf "%s" attr

type ('e, 't) t = {
    let_pattern: 't Pattern.t ;
    rhs       : 'e ;
    let_result: 'e ;
    attributes: attributes ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp f g ppf = fun {let_pattern; rhs; let_result; attributes=attr} ->
  Format.fprintf ppf "@[<v>let %a = %a%a in@,%a@]"
    (Pattern.pp g) let_pattern
    f rhs
    pp_attributes attr
    f let_result

let fold_map :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) t -> 'acc * ('b,'d) t
= fun f g acc {let_pattern; rhs; let_result; attributes} ->
  let acc,let_pattern = Pattern.fold_map g acc let_pattern in
  let acc,rhs        = f acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{let_pattern; rhs; let_result; attributes})
