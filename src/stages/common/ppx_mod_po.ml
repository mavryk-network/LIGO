open Ppxlib
open Simple_utils
open Ast_builder.Default

type post = { has_it : structure -> bool ;
              gen_it : loc:location -> string -> structure }

let post_t =
  let has_it xs =
    let aux = function
        { pstr_desc = Pstr_type (_, xs) } ->
         let aux = function
             { ptype_name = { txt } } -> String.equal txt "t" in
         List.exists ~f:aux xs
      | _ -> false in
    List.exists ~f:aux xs in
  let gen_it ~loc mname =
    let t = ptyp_constr ~loc { txt = Ldot (lident mname, "t") ; loc } [] in
    let type_declaration = type_declaration ~loc ~name:{ txt = String.lowercase_ascii mname ; loc } ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public ~manifest:(Some t) in
    let new_desc = Pstr_type (Nonrecursive, [type_declaration]) in
    [ { pstr_desc = new_desc ; pstr_loc = loc } ] in
  { has_it ; gen_it }

let post_pp =
  let has_it xs =
    let aux = function
        { pstr_desc = Pstr_value (_, xs) } ->
         let aux = function
             { pvb_pat = { ppat_desc = Ppat_var { txt } } } -> String.equal txt "pp"
           | _ -> false in
         List.exists ~f:aux xs
      | _ -> false in
    List.exists ~f:aux xs in
  let gen_it ~loc mname =
    let value_binding = value_binding ~loc ~pat:(ppat_var ~loc @@ { txt = "pp_" ^ (String.lowercase_ascii mname) ; loc })
                          ~expr:(pexp_ident ~loc { txt = Ldot (lident mname, "pp") ; loc }) in
    let new_desc = Pstr_value (Nonrecursive, [value_binding]) in
    [ { pstr_desc = new_desc ; pstr_loc = loc } ] in
  { has_it ; gen_it }

let posts = [ post_t ; post_pp ]

let map_exprs = object
  inherit Ast_traverse.map as super

  method! structure = fun decls ->
    let aux = fun { pstr_desc ; pstr_loc } -> match pstr_desc with
        Pstr_module { pmb_name = { txt = Some ml } ; pmb_expr = { pmod_desc = Pmod_structure sis } } as pstr_desc ->
         let pstr_desc = super#structure_item_desc pstr_desc in
         let loc = pstr_loc in
         let aux r { has_it ; gen_it } =
           if has_it sis then
             r @ gen_it ~loc ml
           else
             r in
         List.fold ~init:[{ pstr_desc ; pstr_loc }] ~f:aux posts
      | pstr_desc ->
         let pstr_desc = super#structure_item_desc pstr_desc in
         [{ pstr_desc ; pstr_loc }] in
    List.concat_map ~f:aux decls
end

let () =
  Ppxlib.Driver.register_transformation
    "ppx_mod_po"
    ~impl:map_exprs#structure
    ~intf:map_exprs#signature
