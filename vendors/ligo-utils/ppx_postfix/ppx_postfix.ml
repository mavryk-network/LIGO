open Ppxlib
open Simple_utils

let postfix (s : string) : string option =
  let s = s |> String.to_seq |> Stdlib.List.of_seq in
  let s = List.rev s in
  let rec g acc xs = match xs with
    | [] -> None
    | '_' :: _xs -> Some acc
    | a :: xs -> g (a :: acc) xs in
  g [] s |> Stdlib.Option.map Stdlib.List.to_seq |> Stdlib.Option.map String.of_seq

let rec extract_ident = function
  Lident id -> id
| Ldot (_, id) -> id
| Lapply (_, lid) -> extract_ident lid

let map_exprs = object
  inherit Ast_traverse.map as super

  method! value_binding expr =
    let loc = expr.pvb_loc in
    let expr = super#value_binding expr in
    if List.mem (List.map ~f:(fun {attr_name;_} -> attr_name.txt) expr.pvb_attributes) "pure" ~equal:String.equal then
      expr else
    match expr.pvb_pat with
    | {ppat_desc = Ppat_var sl;_} ->
       (match postfix sl.txt with
        | Some "r" ->
           let expr' = [%expr fun ~(raise:(_ Trace.raise)) -> [%e expr.pvb_expr] ] in
           { expr with pvb_expr = expr' }
        | Some "w" ->
           let expr' = [%expr fun ~(add_warning:(_ -> unit)) -> [%e expr.pvb_expr] ] in
           { expr with pvb_expr = expr' }
        | Some "rw" ->
           let expr' = [%expr fun ~(raise:_ Trace.raise) ~(add_warning:(_ -> unit)) -> [%e expr.pvb_expr] ] in
           { expr with pvb_expr = expr' }
        | _ -> expr)
    | {ppat_desc = Ppat_constraint ({ppat_desc = Ppat_var sl;_} as l, ({ptyp_desc = Ptyp_poly (bv,ty);_} as oty));_} ->
       (match postfix sl.txt with
        | Some "r" ->
           let ty' = [%type: raise:(_ Trace.raise) -> [%t ty]] in
           let oty' = { oty with ptyp_desc =  Ptyp_poly (bv, ty') } in
           let pat' = { expr.pvb_pat with ppat_desc = Ppat_constraint (l,oty') } in
           let expr' = [%expr fun ~(raise:(_ Trace.raise)) -> [%e expr.pvb_expr] ] in
           { expr with pvb_expr = expr' ; pvb_pat = pat' }
        | Some "w" ->
           let ty' = [%type: add_warning:(_ -> unit) -> [%t ty]] in
           let oty' = { oty with ptyp_desc =  Ptyp_poly (bv, ty') } in
           let pat' = { expr.pvb_pat with ppat_desc = Ppat_constraint (l,oty') } in
           let expr' = [%expr fun ~(add_warning:(_ -> unit)) -> [%e expr.pvb_expr] ] in
           { expr with pvb_expr = expr' ; pvb_pat = pat' }
        | Some "rw" ->
           let ty' = [%type: raise:(_ Trace.raise) -> add_warning:(_ -> unit) -> [%t ty]] in
           let oty' = { oty with ptyp_desc =  Ptyp_poly (bv, ty') } in
           let pat' = { expr.pvb_pat with ppat_desc = Ppat_constraint (l,oty') } in
           let expr' = [%expr fun ~(raise:_ Trace.raise) ~(add_warning:(_ -> unit)) -> [%e expr.pvb_expr] ] in
           { expr with pvb_expr = expr' ; pvb_pat = pat' }
        | _ -> expr)
    | _ -> expr

  method! expression_desc expr =
    let expr = super#expression_desc expr in
    match expr with
    | Pexp_apply (n, b) ->
       (match n.pexp_desc with
        | Pexp_ident id ->
           let id_id = extract_ident id.txt in
           (match postfix id_id with
            | Some "r" ->
               let loc = id.loc in
               let expr' = [%expr [%e n] ~raise] in
               Pexp_apply (expr', b)
            | Some "w" ->
               let loc = id.loc in
               let expr' = [%expr [%e n] ~add_warning] in
               Pexp_apply (expr', b)
            | Some "rw" ->
               let loc = id.loc in
               let expr' = [%expr [%e n] ~raise ~add_warning] in
               Pexp_apply (expr', b)
            | _ -> expr)
        | _ -> expr)
  | _ -> expr
end

let () =
  Ppxlib.Driver.register_transformation
    "ppx_postfix"
    ~impl:map_exprs#structure
    ~intf:map_exprs#signature
