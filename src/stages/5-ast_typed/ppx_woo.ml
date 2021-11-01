module SMap = struct
  include Map.Make(String)
  let to_kv_list_rev : 'a t -> (string * 'a) list = fun m -> fold (fun k v prev -> (k , v) :: prev) m []
  let to_kv_list : 'a t -> (string * 'a) list = fun m -> List.rev (to_kv_list_rev m)
end

module P = Ppxlib
module A = P.Ast_builder.Default

let counter = ref 0

module Woo = struct

  type variant = {
    constructor_declarations : type_expression list SMap.t ;
    polymorphic : bool ;
  }

  and record_field = {
    type_expression : type_expression ;
    default_value : P.expression option ;
  }

  and record = record_field SMap.t

  and type_expression =
    | T_variant of variant
    | T_record of record
    | T_core of P.core_type

  let get_t_core_opt = function
    | T_core ct -> Some ct
    | _ -> None

  let record_field ?default_value type_expression = { type_expression ; default_value }

  (* type expression =
   *   | E_expression of P.expression *)
  type label = string
  type labelled_type = label * type_expression
  type labelled_record_field = label * record_field
  type labelled_types = label * type_expression list
  type variant_map = type_expression list SMap.t
  (* type labelled_expression = string * expression *)
  type non_recursive = bool

  type type_declaration = {
    labelled_type : labelled_type ;
    non_recursive : non_recursive ;
  }

  let type_declaration ?(non_recursive = false) labelled_type = { non_recursive ; labelled_type }

  (* type value_declaration = {
   *   labelled_expression : labelled_expression ;
   *   non_recursive : non_recursive ;
   * }
   *
   * let value_declaration ?(non_recursive = true) labelled_expression = { non_recursive ; labelled_expression } *)

  type type_declarations = type_declaration list

  let c_decls : (string * type_expression list) list -> type_expression list SMap.t = fun cds ->
    List.fold_left (fun old (k , v) -> SMap.add k v old) SMap.empty cds

  let r_decls : labelled_record_field list -> record = fun rds ->
    List.fold_left (fun old (k , v) -> SMap.add k v old) SMap.empty rds

end

module W = Woo

module type PARAMS = sig
  val location : P.location
end

module Make(Params : PARAMS) = struct

  let loc = Params.location
  let failwith str = P.Location.raise_errorf ~loc str


  module Parse = struct

    let get_default_value_opt : P.attribute -> P.expression option = fun attr ->
      let P.{ attr_payload = pl ; attr_name = name ; _ } = attr in
      let (>>?) = Option.bind in
      (if name.txt = "default" then Some () else None) >>? fun () ->
      (match pl with
       | P.PStr items -> Some items
       | _ -> None
      ) >>? fun items ->
      (match items with
      | [ default ] -> Some (default)
      | _ -> None) >>? fun (default) ->
      (match default.pstr_desc with
       | Pstr_eval (expr , _) -> (
           Some expr
         )
       | _ -> None)

    let labelled_record_field : P.label_declaration  -> W.labelled_record_field = fun ld ->
      let label = ld.pld_name.txt in
      let ty = ld.pld_type in
      let attributes = ld.pld_attributes in
      let default_value = Base.List.find_map ~f:get_default_value_opt attributes in
      (label , W.record_field ?default_value (W.T_core ty))

    let record : P.label_declaration list -> W.type_expression = fun lds ->
      let record_fields = List.map labelled_record_field lds in
      T_record ( W.r_decls record_fields )

    let type_declaration ?non_recursive : P.type_declaration -> W.type_declaration = fun td ->
      let label = td.ptype_name.txt in
      let body =
        match td.ptype_kind with
        | P.Ptype_variant cds ->  (
            let aux : P.constructor_declaration -> (string * W.type_expression list) = fun cd ->
              let constructor = cd.pcd_name.txt in
              let args =
                match cd.pcd_args with
                | P.Pcstr_tuple tys -> List.map (fun ty -> W.T_core ty) tys
                | P.Pcstr_record lds -> [ record lds ]
              in
              (constructor , args)
            in
            let polymorphic = false in
            let constructor_declarations = W.c_decls @@ List.map aux cds in
            W.T_variant { polymorphic ; constructor_declarations }
          )
        | P.Ptype_record lds -> record lds
        | P.Ptype_abstract -> (
            match td.ptype_manifest with
            | Some ct -> W.T_core ct
            | None -> failwith "parse_type_declaration: unknown case PType_abstract and no manifest"
          )
        | P.Ptype_open -> failwith "parse_type_declaration: unknown case PType_open"
      in
      W.type_declaration ?non_recursive ( label , body )

    let type_declarations ?non_recursive : P.type_declaration list -> W.type_declarations = fun tds ->
      List.map (type_declaration ?non_recursive) tds

    let non_recursive : P.rec_flag -> W.non_recursive = fun rf ->
      match rf with
      | P.Nonrecursive -> true
      | P.Recursive -> false

  end

  module Generate = struct

    let lident label = A.Located.lident ~loc label

    let e_true = P.([%expr true])
    let e_false = P.([%expr false])
    let e_bool b = if b then e_true else e_false
    let e_unit = P.([%expr ()])
    let e_var x = A.evar ~loc x
    let e_string str = A.estring ~loc str
    let e_var_n n = e_var (Int.to_string n)
    let p_var x = A.pvar ~loc x
    let e_record lst = A.pexp_record ~loc (List.map (fun (var , expr) -> (lident var , expr)) lst) None
    let e_fun ?(label = P.Nolabel) ?default var body =
      A.pexp_fun ~loc label default (p_var var) body
    let e_named_fun var body =
      let label = P.Labelled var in
      e_fun ~label var body
    let e_option_fun var ~default body =
      let label = P.Optional var in
      e_fun ~label ~default var body
    let t_unit = P.([%type: unit])
    let t_tuple lst = A.ptyp_tuple ~loc lst
    let e_property ~record ~(label:string) = A.pexp_field ~loc record (lident label)

    let label_to_variable : W.label -> string = fun str ->
      let s = String.lowercase_ascii str in
      if P.Keyword.is_keyword s
      then s ^ "_"
      else s

    (* Not fit for variant declarations *)
    let abstract_type_declaration ?(params = []) ?(private_ = P.Public) name body =
      let manifest = Some body in
      let name = Location.mkloc name loc in
      let declaration = A.type_declaration ~loc ~params ~name ~cstrs:[] ~kind:P.Ptype_abstract ~private_ ~manifest in
      let declarations = A.pstr_type ~loc Nonrecursive [ declaration ] in
      declarations

    let declaration ~name ~body =
      let pat = p_var name in
      let expr = body in
      let declaration = A.value_binding ~loc ~expr ~pat in
      let declarations = A.pstr_value ~loc Nonrecursive [ declaration ] in
      declarations

    let p_constructor ~polymorphic ?pattern label =
      if polymorphic
      then A.ppat_variant ~loc label pattern
      else A.ppat_construct ~loc (A.Located.lident ~loc label) pattern

    let e_constructor ~polymorphic ?pattern label =
      if polymorphic
      then A.pexp_variant ~loc label pattern
      else A.pexp_construct ~loc (A.Located.lident ~loc label) pattern

    let extract_core_type = function
      | W.T_variant _ -> failwith "ez doesn't support inline variants"
      | W.T_record _ -> failwith "ez doesn't support inline records"
      | W.T_core ct -> ct

    module Matching = struct

      let matching ~namer case : W.variant -> W.labelled_types -> P.structure_item = fun variant (base_label , _ty) ->
        let function_name = p_var @@ namer @@ label_to_variable base_label in
        let W.{ constructor_declarations ; polymorphic } = variant in
        let constructor_declarations' = SMap.to_kv_list constructor_declarations in
        let cases = List.map (case ~polymorphic ~base_label) constructor_declarations' in
        let body = A.pexp_function ~loc cases in
        let declaration = A.value_binding ~loc ~expr:body ~pat:function_name in
        let declarations = A.pstr_value ~loc Nonrecursive [ declaration ] in
        declarations

      let generic_case ~polymorphic ~base_label rhs : (string * W.type_expression list) -> P.case = fun (current_label , tys) ->
        let variable_name = "_ppx_match_variable" in
        let e_variable = e_var variable_name in
        let p_variable = p_var variable_name in
        let p_tuple_name i = "_" ^ (Int.to_string i) in
        let l = List.length tys in
        let lhs =
          if l = 0 then (
            p_constructor ~polymorphic current_label
          ) else (
            let pattern =
              if polymorphic
              then p_var variable_name
              else (
                let pattern_names = Base.List.init ~f:p_tuple_name l in
                let patterns = List.map (fun name -> A.pvar ~loc name) pattern_names in
                A.ppat_tuple ~loc patterns
              )
            in
            p_constructor ~polymorphic ~pattern current_label
          )
        in
        let rhs =
          let rhs = rhs ~base_label ~current_label ~variable:e_variable in
          let local_lhs_expr =
            if l = 0 then (
              e_unit
            ) else if polymorphic then (
              e_variable
            ) else (
              let var_names = Base.List.init ~f:p_tuple_name l in
              let vars = List.map e_var var_names in
              A.pexp_tuple ~loc vars
            )
          in
          let lhs =
            let pat = p_variable in
            let expr = local_lhs_expr in
            A.value_binding ~loc ~pat ~expr
          in
          A.pexp_let ~loc P.Nonrecursive [ lhs ] rhs
        in
        A.case ~lhs ~guard:None ~rhs

      let matching_full ~namer rhs : W.variant -> P.structure_item list = fun variant ->
        let matching_single =
          let case = generic_case rhs in
          matching ~namer case
        in
        let W.{ constructor_declarations ; polymorphic = _ } = variant in
        let constructor_declarations = SMap.to_kv_list constructor_declarations in
        let matching_lst = List.map (matching_single variant) constructor_declarations in
        matching_lst

      let matching_is_full : W.variant -> P.structure_item list =
        let rhs ~base_label ~current_label ~variable:_ =
          e_bool @@ String.equal base_label current_label
        in
        let namer x = "is_" ^ x in
        matching_full ~namer rhs

      let matching_exn_full : W.variant -> P.structure_item list =
        let namer x = "get_" ^ x ^ "_exn" in
        let rhs ~base_label ~current_label ~variable =
          let failure =
            let match_exn_name = namer base_label in
            let str_constant = e_string match_exn_name in
            P.([%expr raise (Failure [%e str_constant]) ])
          in
          if String.equal base_label current_label
          then variable
          else failure
        in
        matching_full ~namer rhs

      let matching_opt_full : W.variant -> P.structure_item list =
        let namer x = "get_" ^ x ^ "_opt" in
        let rhs ~base_label ~current_label ~variable =
          if String.equal base_label current_label
          then P.([%expr Some ([%e variable])])
          else P.([%expr None ])
        in
        matching_full ~namer rhs

    end

    module Constructors = struct

      let constructor_type polymorphic : W.labelled_types -> P.structure_item list = fun lts ->
        ignore polymorphic ;
        let (label , tys) = lts in
        let body =
          match tys with
          | [] -> t_unit
          | [ single ] -> extract_core_type single
          | lst -> t_tuple @@ List.map extract_core_type lst
        in
        let name = label_to_variable label in
        [ abstract_type_declaration name body ]

      let constructor_types : W.variant -> P.structure_item list = fun v ->
        let W.{ polymorphic ; constructor_declarations } = v in
        let ltss = SMap.to_kv_list constructor_declarations in
        let itemss = List.map (constructor_type polymorphic) ltss in
        let items = List.concat itemss in
        items

      let constructor polymorphic : W.labelled_types -> P.structure_item list = fun lts ->
        let (label , tys) = lts in
        let l = List.length tys in
        let body =
          if l = 0 then (
            e_constructor ~polymorphic label
          ) else (
            e_fun "x" (e_constructor ~polymorphic label ~pattern:(e_var "x"))
          )
        in
        let name = label_to_variable label in
        [ declaration ~name ~body ]

      let constructors : W.variant -> P.structure_item list = fun v ->
        let W.{ polymorphic ; constructor_declarations } = v in
        let ltss = SMap.to_kv_list constructor_declarations in
        let itemss = List.map (constructor polymorphic) ltss in
        let items = List.concat itemss in
        items

    end

    module Properties = struct

      let property_type : W.labelled_record_field -> P.structure_item list = fun lt ->
        let (label , W.{ type_expression = ty ; default_value = _ }) = lt in
        let body = extract_core_type ty in
        let name = label_to_variable label in
        [ abstract_type_declaration name body ]

      let property_types : W.record -> P.structure_item list = fun record_declarations ->
        let ltss = SMap.to_kv_list record_declarations in
        let itemss = List.map property_type ltss in
        let items = List.concat itemss in
        items

      let property : W.labelled_record_field -> P.structure_item list = fun lts ->
        let (label , W.{ type_expression = ty ; default_value = _ }) = lts in
        ignore ty ;
        let body =
            e_fun "x" (e_property ~label ~record:(e_var "x"))
        in
        let name = label_to_variable label in
        [ declaration ~name ~body ]

      let properties : W.record -> P.structure_item list = fun property_declarations ->
        let ltss = SMap.to_kv_list property_declarations in
        let itemss = List.map property ltss in
        let items = List.concat itemss in
        items

    end

    module Make = struct

      let make name : W.record -> P.structure_item list = fun property_declarations ->
        let lts = (SMap.to_kv_list property_declarations) in
        let f : W.labelled_record_field ->P.expression ->  P.expression = fun lr expr ->
          let (label , rf) = lr in
          let W.{ default_value ; _ } = rf in
          match default_value with
          | None -> e_named_fun label expr
          | Some default -> e_option_fun ~default label expr
        in
        let init =
          let aux : W.labelled_record_field -> (string * P.expression) = fun lr ->
            let (label , _) = lr in
            (label , e_var label)
          in
          let lst = List.map aux lts in
          e_record lst
        in
        let body = Base.List.fold_right lts ~init ~f in
        [ declaration ~name ~body ]

    end

    let str : (P.rec_flag * P.type_declaration list) -> P.structure_item list = fun (rec_flag , tds) ->
      let non_recursive = Parse.non_recursive rec_flag in
      let tds' = Parse.type_declarations ~non_recursive tds in
      let aux : W.type_declaration -> P.structure_item list = fun td ->
        let W.{ labelled_type = (label , ty) ; non_recursive = _ } = td in
        match ty with
        | W.T_variant variant -> (
            let cons_types = Constructors.constructor_types variant in
            let is = Matching.matching_is_full variant in
            let exn = Matching.matching_exn_full variant in
            let opt = Matching.matching_opt_full variant in
            let cons = Constructors.constructors variant in
            []
            @ cons_types
            @ cons
            @ is
            @ exn
            @ opt
          )
        | W.T_record record -> (
            (* let prop_types = Properties.property_types record in *)
            let props = Properties.properties record in
            let maker = Make.make label record in
            []
            (* @ prop_types *)
            @ props
            @ maker
          )
        | W.T_core _ -> []
      in
      let itemss = List.map aux tds' in
      let items = List.concat itemss in
      let outf = open_out_gen [Open_append; Open_creat] 0o666 "/tmp/local.txt" in
      let s = (Format.asprintf "ITEMS %d:\n %a" (! counter) (Format.pp_print_list P.Pprintast.structure_item) items) in
      let () = counter := ! counter + 1 in
      let () = Printf.fprintf outf "%s" s in
      items

  end

end

let make_PARAMS loc : (module PARAMS)  = (module struct let location = loc end : PARAMS)

let generator generate = P.Deriving.Generator.make_noarg generate

let deriver =
  let str_type_decl =
    let generate ~loc ~path:_ input =
      let module Full = Make(val make_PARAMS loc) in
      Full.Generate.str input
    in
    generator generate
  in
  (* let sig_type_decl =
   *   let generate ~loc ~path:_ input =
   *     let module Full = Make(val make_PARAMS loc) in
   *     Full.Generate.sig input
   *   in
   *   generator generate
   * in *)
  P.Deriving.add "ez"
    ~str_type_decl
    (* ~sig_type_decl *)
