open Ppxlib
open Simple_utils
open Ast_builder.Default

(* Some helpers for destructing AST *)

let rec extract_ident = function
  Lident id -> id
| Ldot (_, id) -> id
| Lapply (_, lid) -> extract_ident lid

let extract_payload = function
  | PStr [{ pstr_desc =
              Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (id, _, _)) ; _ }, _)
          ; _ }] ->
     Some [id]
  | PStr [{ pstr_desc =
              Pstr_eval ({ pexp_desc = Pexp_tuple l ; _ }, _)
          ; _ }] ->
     let f = function
       | { pexp_desc = Pexp_constant (Pconst_string (id, _, _)) ; _ } -> Some id
       | _ -> None in
     let l = List.filter_map l ~f in
     Some l
  | _ -> None


let rec extract_var = function
    { ppat_desc = Ppat_var { txt } } -> Some txt
  | { ppat_desc = Ppat_constraint (pp, _) } -> extract_var pp
  | _ -> None

type post = { has_it : label list -> label option ;
              gen_it : loc:location -> name:string -> label -> structure }

let post_t1 =
  let get s =
    match String.split_on_char 't' s with
    | [""; n] -> Some (int_of_string n)
    | _ -> None in
  let reconstruct n = "t" ^ string_of_int n in
  let has_it xs =
    Option.map ~f:reconstruct @@ List.find_map xs ~f:get in
  let gen_it ~loc ~name l =
    let n = match get l with
      | Some n -> n
      | None -> failwith "impossible?" in
    let ta = List.init n ~f:(fun i -> ptyp_var ~loc @@ "a" ^ string_of_int i) in
    let t = ptyp_constr ~loc { txt = Longident.parse (name ^ ".t") ; loc } ta in
    let ta = List.map ta ~f:(fun t -> (t, (NoVariance, NoInjectivity))) in
    let type_declaration = type_declaration ~loc ~name:{ txt = String.lowercase_ascii name ; loc } ~params:ta ~cstrs:[] ~kind:Ptype_abstract ~private_:Public ~manifest:(Some t) in
    let new_desc = Pstr_type (Nonrecursive, [type_declaration]) in
    [ { pstr_desc = new_desc ; pstr_loc = loc } ] in
  { has_it ; gen_it }

(* let post_t =
 *   let has_it xs = List.mem xs "t" ~equal:String.equal in
 *   let gen_it ~loc ~name _ =
 *     let t = ptyp_constr ~loc { txt = Longident.parse (name ^ ".t") ; loc } [] in
 *     let type_declaration = type_declaration ~loc ~name:{ txt = String.lowercase_ascii name ; loc } ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public ~manifest:(Some t) in
 *     let new_desc = Pstr_type (Nonrecursive, [type_declaration]) in
 *     [ { pstr_desc = new_desc ; pstr_loc = loc } ] in
 *   { has_it ; gen_it } *)

let post_pp =
  let has_it xs =
    if List.mem xs "pp" ~equal:String.equal then
      Some "pp"
    else
      None in
  let gen_it ~loc ~name _ =
    let value_binding = value_binding ~loc ~pat:(ppat_var ~loc @@ { txt = "pp_" ^ (String.lowercase_ascii name) ; loc })
                          ~expr:(pexp_ident ~loc { txt = Longident.parse (name ^ ".pp") ; loc }) in
    let new_desc = Pstr_value (Nonrecursive, [value_binding]) in
    [ { pstr_desc = new_desc ; pstr_loc = loc } ] in
  { has_it ; gen_it }

let posts = [ (* post_t ;  *)post_t1 ; post_pp ]

let map_exprs = object
  inherit Ast_traverse.map as super

  method! structure = fun decls ->
    let aux = fun { pstr_desc ; pstr_loc } -> match pstr_desc with
        Pstr_module { pmb_name = { txt = Some name } ; pmb_attributes } as pstr_desc ->
         let pstr_desc = super#structure_item_desc pstr_desc in
         let attr = List.find_map pmb_attributes
                      ~f:(function | {attr_name; attr_payload; _} when String.equal attr_name.txt "mod_po" -> Some attr_payload
                                   | _ -> None) in
         let attr = Option.bind attr ~f:extract_payload in
         (match attr with
         | None -> [{ pstr_desc ; pstr_loc }]
         | Some l ->
            let loc = pstr_loc in
            let aux r { has_it ; gen_it } =
              match has_it l with
              | None -> r
              | Some l -> r @ gen_it ~loc ~name l in
            let structure = List.fold ~init:[] ~f:aux posts in
            let expr = pmod_structure ~loc structure in
            let include_infos = include_infos ~loc expr in
            let pstr_include = pstr_include ~loc include_infos in
            [{ pstr_desc ; pstr_loc } ; pstr_include ])
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
