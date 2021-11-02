open Ppxlib
open Simple_utils
open Ast_builder.Default

(* let map (s : string) : string option =
 *   let s = s |> String.to_seq |> Stdlib.List.of_seq in
 *   let s = List.rev s in
 *   let rec g acc xs = match xs with
 *     | [] -> None
 *     | '_' :: _xs -> Some acc
 *     | a :: xs -> g (a :: acc) xs in
 *   g [] s |> Stdlib.Option.map Stdlib.List.to_seq |> Stdlib.Option.map String.of_seq
 * 
 * let rec extract_ident = function
 *   Lident id -> id
 * | Ldot (_, id) -> id
 * | Lapply (_, lid) -> extract_ident lid
 * 
 * let extract_payload = function
 *   | PStr [{ pstr_desc =
 *               Pstr_eval ({ pexp_desc =
 *                              Pexp_tuple [
 *                                  { pexp_desc = Pexp_ident id ; _ } ;
 *                                  { pexp_desc = Pexp_tuple l ; _ }
 *                                ]
 *                   ; _ }, _)
 *           ; _ }] ->
 *      let f = function
 *        | { pexp_desc = Pexp_constant (Pconst_string (id, _, _)) ; _ } -> Some id
 *        | _ -> None in
 *      let l = List.filter_map l ~f in
 *      Some (extract_ident id.txt, l)
 *   | _ -> None
 * 
 * let replace_exprs id new_id =
 *   let replace l =
 *     let l = Str.global_replace (Str.regexp_string id) new_id l in
 *     let l = Str.global_replace (Str.regexp_string (String.uppercase_ascii id)) (String.uppercase_ascii new_id) l in
 *     l in
 *   object
 *     inherit Ast_traverse.map as super
 * 
 * 
 *     method! pattern_desc pat =
 *       let pat = super#pattern_desc pat in
 *       match pat with
 *       | Ppat_var v -> Ppat_var {v with txt = replace v.txt }
 *       | _ -> pat
 * 
 *     method! longident lid =
 *       let lid = super#longident lid in
 *       let rec aux = function
 *         | Lident l -> Lident (replace l)
 *         | Ldot (lid, label) -> Ldot (aux lid, replace label)
 *         | Lapply (lid, lid') -> Lapply (aux lid, aux lid') in
 *       aux lid
 *   end *)

let has_pp xs =
  let aux = function
      { pstr_desc = Pstr_value (_, xs) } ->
       let aux = function
           { pvb_pat = { ppat_desc = Ppat_var { txt } } } -> String.equal txt "pp"
         | _ -> false in
       List.exists ~f:aux xs
     | _ -> false in
  List.exists ~f:aux xs

let map_exprs = object
  inherit Ast_traverse.map as super

  method! structure = fun decls ->
    let aux = fun { pstr_desc ; pstr_loc } -> match pstr_desc with
        Pstr_module { pmb_name = { txt = Some ml } ; pmb_expr = { pmod_desc = Pmod_structure sis } } as pstr_desc ->
         let pstr_desc = super#structure_item_desc pstr_desc in
         let loc = pstr_loc in
         if has_pp sis then
           let value_binding = value_binding ~loc ~pat:(ppat_var ~loc @@ { txt = "pp_" ^ (String.lowercase_ascii ml) ; loc })
                                ~expr:(pexp_ident ~loc { txt = Ldot (lident ml, "pp") ; loc }) in
           let new_desc = Pstr_value (Nonrecursive, [value_binding]) in
           [ { pstr_desc ; pstr_loc } ; { pstr_loc ; pstr_desc = new_desc } ]
         else
           [ { pstr_desc ; pstr_loc } ]
      | pstr_desc ->
         let pstr_desc = super#structure_item_desc pstr_desc in
         [{ pstr_desc ; pstr_loc }] in
    List.concat_map ~f:aux decls

end

let () =
  Ppxlib.Driver.register_transformation
    "ppx_map"
    ~impl:map_exprs#structure
    ~intf:map_exprs#signature
