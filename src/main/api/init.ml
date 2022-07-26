open Api_helpers
open Tezos_utils.Micheline
open Simple_utils.Trace

module Compile   = Ligo_compile
module Utils     = Simple_utils.Utils
module Helpers   = Ligo_compile.Helpers
module Location  = Simple_utils.Location

(* DEBUG PRINT *)
(* TODO : Should we keep this function somewhere ? Could be handy *)

let print_micheline_node (node : Micheline_parser.node) =
  let convert_locations node = Micheline.map_node (fun _ -> {Micheline_printer.comment = None}) (fun x -> x) node in
  Format.fprintf Format.err_formatter "Micheline node :\n%a\n\n" Micheline_printer.print_expr @@ convert_locations node

module Micheline_debug_print = struct

  let rec print_node ppf node =
    let open Micheline in
    match node with
    | Int _ -> Format.fprintf ppf "Int"
    | String _ -> Format.fprintf ppf "String"
    | Bytes _ -> Format.fprintf ppf "Bytes"
    | Prim (_, p, nodes, annots) -> Format.fprintf ppf "Prim <%s>@,{@[<v 2>@,annots:@,%a@,%a@]@,}" p print_annots annots print_nodes nodes
    (* 'l * 'p * ('l, 'p) node list * annot *)
    | Seq (_, nodes) -> Format.fprintf ppf "Seq @,{@[<v 2>@,%a@]@,}" print_nodes nodes
  and print_annots ppf annots =
    match annots with
    | [] -> Format.fprintf ppf ""
    | h :: t -> Format.fprintf ppf "%s; %a" h print_annots t
  and print_nodes ppf nodes =
    match nodes with
    | [] -> Format.fprintf ppf ""
    | h :: t -> Format.fprintf ppf "@,%a%a" print_node h print_nodes t
  let print_toplevel ppf nodes =
    Format.fprintf ppf "@[<v>%a@]" print_nodes nodes

end (* of module Micheline_debug_print *)

(* Additional combinators for Ast_core nodes *)
(* TODO : Are there combinators that could help make this submodule simpler ? *)
(* TODO : What should be put instead of [Location.dummy] ? *)

module Build_ast = struct

  let param_t_name = "parameter"
  let storage_t_name = "storage"


  let unit_ : Ast_core.expression         = {
    expression_content  = E_literal (Literal_unit);
    sugar               = None;
    location            = Location.dummy;
  }

  let type_var name : Ast_core.type_expression = {
      type_content = Ast_core.Combinators.T_variable ( Ast_core.TypeVar.of_input_var name);
      sugar        = None;
      location     = Location.dummy;
    }

  (* TODO : Pattern-matching exhaustiveness is not ensured here, we currently have to manually of all Prim types, what can we do ? *)
  let type_expr ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise) node =
    let open Micheline in
    let open Ast_core.Combinators in
    let rec aux node =
      match node with
      | Prim (_, "int",    [], _ )         -> t_int ()
      | Prim (_, "string", [], _ )         -> t_string ()
      | Prim (_, "bytes",  [], _ )         -> t_bytes ()
      | Prim (_, "nat",    [], _ )         -> t_nat ()
      | Prim (_, "bool",   [], _ )         -> t_bool ()
      | Prim (_, "list",   [n], _ )        -> t_list (aux n)
      | Prim (_, "pair",   [n1 ; n2], _ )  -> t_pair (aux n1) (aux n2)
      (* TODO : Add appropriate error types in Main_errors *)
      | Prim (_, name,     _, _ ) -> Printf.printf "Unsupported Prim() type : %s\n" name; raise.error @@ Main_errors.repl_unexpected
      | _ -> Printf.printf "Unsupported type so far\n"; raise.error @@ Main_errors.repl_unexpected
    in aux node


  let type_decl name node =
    let param_type_declaration_content = Ast_core.Declaration_type ( {
      type_binder = Ast_core.TypeVar.of_input_var name;
      type_expr = node;
      type_attr : Ast_core.type_attribute = { public = true; hidden = false }
    }) in
    let param_type_declaration : Ast_core.declaration = {
      wrap_content = param_type_declaration_content;
      location = Location.dummy
    } in
    param_type_declaration

  let main_decl : Ast_core.declaration =
    let input_type = Ast_core.Combinators.t_pair (type_var param_t_name) (type_var storage_t_name) in
    let content = Ast_core.Declaration_constant ({
      binder = {
        var  = Ast_core.ValueVar.of_input_var "main" ;
        ascr = Some input_type;
        attributes = { const_or_var = None } ; (* TODO : Is it the correct value for [main] ? *)
      };
      expr = unit_;
      (* TODO : Is it the correct value for [main] ? *)
      attr : Stage_common.Types.known_attributes = {
        inline       = false;
        no_mutation  = false;
        thunk        = false;
        view         = false;
        public       = true;
        hidden       = false;
      }
    })
    in
    {
      wrap_content = content;
      location = Location.dummy
    }

    let toplevel ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise) param storage : Ast_core.module_ =
      let param_type_decl = type_decl param_t_name (type_expr ~raise param) in
      let storage_type_decl = type_decl storage_t_name (type_expr ~raise storage) in
      [ param_type_decl ; storage_type_decl ; main_decl ]

end (* of module Build_ast  *)





module Build_cst_cameligo = struct
  open Cst_cameligo.CST

  let the_unit : the_unit = (Wrap.ghost "(", Wrap.ghost ")")
  let wrapped_the_unit = Region.wrap_ghost the_unit
  let mk_eunit : expr =
    EUnit wrapped_the_unit

  let mk_punit : pattern =
    PUnit wrapped_the_unit

  let mk_var : string -> variable = fun var_name -> Region.wrap_ghost var_name

  let mk_var_pattern : string -> var_pattern = fun var_name -> {
    variable   = mk_var var_name;
    attributes = []
  }
  let mk_pvar : string -> pattern = fun name -> PVar ( Region.wrap_ghost @@ mk_var_pattern name)

  let mk_par : 'a -> 'a par reg = fun i ->
    Region.wrap_ghost {
      lpar = Wrap.ghost "(";
      inside = i;
      rpar = Wrap.ghost ")";
    }

  let mk_ppar : pattern -> pattern = fun p ->
    PPar (mk_par p)

  let mk_ptuple : ?sep : lexeme -> pattern Utils.nseq -> pattern = fun ?(sep=",") (p, plist) ->
    let sep = Wrap.ghost sep in
    PTuple ( Region.wrap_ghost (p, List.map ~f:(fun elt -> sep, elt) plist))

  let mk_ptyped : pattern -> type_expr -> pattern =
    fun p te ->
      PTyped (
        Region.wrap_ghost {
          pattern = p;
          colon = Wrap.ghost ":";
          type_expr = te
        }
      )

  let mk_tvar type_name = TVar (Region.wrap_ghost type_name)

  let mk_let_binding
    :  ?type_params:type_params option
    -> ?args:pattern list
    -> ?rhs_type:type_expr option
    -> string
    -> expr
    -> let_binding =
    fun ?(type_params=None) ?(args=[]) ?(rhs_type=None) var_name expr -> {
      type_params = (
        Option.bind type_params ~f:(fun type_params ->
          Some ( mk_par type_params )
        )
      );
      binders = (mk_pvar var_name, args);
      rhs_type = Option.bind rhs_type ~f:(fun te ->
        Some (
          Wrap.ghost ":",
          te
        )
      );
      eq = Wrap.ghost "=";
      let_rhs = expr
    }

  let mk_let_decl
    : ?type_params:type_params option
    -> ?args:pattern list
    -> ?rhs_type:type_expr option
    -> string
    -> expr
    -> declaration
    = fun ?(type_params=None) ?(args=[]) ?(rhs_type=None) var_name expr ->
    let decl = (
      Wrap.ghost "let", (* kwd_let *)
      None, (* kwd_rec option *)
      mk_let_binding ~type_params ~args ~rhs_type var_name expr,
      [] (* attributes *)
    )
    in Let (Region.wrap_ghost decl)

  let mk_type_decl type_name type_expr =
    let type_decl = {
      kwd_type   = Wrap.ghost "type";
      name       = Region.wrap_ghost type_name;
      params     = None;
      eq         = Wrap.ghost "=";
      type_expr  = type_expr;
    }
    in TypeDecl (Region.wrap_ghost type_decl)

  let mk_tprod t1 t2 =
    TProd (Region.wrap_ghost (t1 , [ Wrap.ghost "*", t2 ]))

  let mk_list t =
    TApp ( Region.wrap_ghost ( (
      Region.wrap_ghost "list" , CArg t
    )))

  let toplevel : Cameligo.CST.type_expr -> Cameligo.CST.type_expr -> Cst_cameligo.CST.t =
    fun params_type_expr storage_type_expr ->

      let parameters_name = "parameters" in
      let storage_name = "storage" in
      let params_type_decl = mk_type_decl parameters_name params_type_expr in
      let storage_type_decl = mk_type_decl storage_name storage_type_expr in
      (* let type_d *)
      let input_pattern = mk_ptuple (mk_pvar "params", [mk_pvar "storage"]) in
      let input_type = mk_tprod (mk_tvar parameters_name) (mk_tvar storage_name) in
      let input_pattern = mk_ppar @@ mk_ptyped input_pattern input_type in
      let output_type = mk_tprod (mk_list @@ mk_tvar "operation") (mk_tvar storage_name) in
      let main_declaration = mk_let_decl ~rhs_type:(Some output_type) ~args:[input_pattern] "main" mk_eunit in

      { decl = (params_type_decl, [storage_type_decl ; main_declaration]);
        eof = Wrap.ghost "eof"
      }

end






let contract_from_michelson syntax source_file display_format () =
  format_result ~display_format (Parsing.Formatter.ppx_format) @@
  fun ~raise ->

    (* Check source file path *)
    let source_file =
      match Fpath.of_string source_file with
      | Ok fpath -> fpath
      | Error _ -> raise.error @@ Main_errors.repl_unexpected (* QUESTION : What type should I use to return file-related errors ? *)
    in

    (* Read Michelson contract *)
    let file_content       =
      match Bos.OS.File.read source_file with
      | Ok str -> str
      | Error _ -> raise.error @@ Main_errors.repl_unexpected (* QUESTION : Same here. ?*)
    in

    (* Parse Michelson contract into Micheline AST representation *)
    (* TODO : Error reporting *)
    let tokens, _error_list = Micheline_parser.tokenize file_content in
    let ast_list, _error_list = Micheline_parser.parse_toplevel tokens in

    let () = List.iter ~f:print_micheline_node ast_list in

    (* Extract parameter and storage definition nodes from AST list *)
    let param_node, storage_node =
      match ast_list with
      | Prim (_, "parameter", [param_node], _) :: Prim (_, "storage", [storage_node], _) :: _ -> param_node, storage_node
      | _ -> Printf.printf "Could not extract parameter and storage node from contract\n"; raise.error @@ Main_errors.repl_unexpected
    in

    (* DEBUG PRINT *)
    let () = print_micheline_node param_node in
    let () = print_micheline_node storage_node in

    let rec map_ty : ('la -> 'lb) -> ('pa -> 'pb) -> ('la, 'pa) Ligo_coq_ocaml.Compiler.ty -> ('lb, 'pb) Ligo_coq_ocaml.Compiler.ty =
      let open Ligo_coq_ocaml.Compiler in
      fun fl fp ty ->
        let self = map_ty fl fp in
        match ty with
        | T_base       ( l , p )                   -> T_base       ( fl l , fp p )
        | T_unit       ( l )                       -> T_unit       ( fl l )
        | T_pair       ( l , s1 , s2 , t1 , t2 )   -> T_pair       ( fl l , s1 , s2 , self t1 , self t2 )
        | T_or         ( l , s1 , s2 , t1 , t2 )   -> T_or         ( fl l , s1 , s2 , self t1 , self t2 )
        | T_func       ( l , t1 , t2 )             -> T_func       ( fl l , self t1 , self t2 )
        | T_lambda     ( l , t1 , t2 )             -> T_lambda     ( fl l , self t1 , self t2 )
        | T_option     ( l , t1 )                  -> T_option     ( fl l , self t1 )
        | T_list       ( l , t1 )                  -> T_list       ( fl l , self t1 )
        | T_set        ( l , t1 )                  -> T_set        ( fl l , self t1 )
        | T_map        ( l , t1 , t2 )             -> T_map        ( fl l , self t1 , self t2 )
        | T_big_map    ( l , t1 , t2 )             -> T_big_map    ( fl l , self t1 , self t2 )
        | T_ticket     ( l , t1 )                  -> T_ticket     ( fl l , self t1 )
        | T_contract   ( l , t1 )                  -> T_contract   ( fl l , self t1 )
        | T_bool       ( l )                       -> T_bool       ( fl l )
        | T_int        ( l )                       -> T_int        ( fl l )
        | T_nat        ( l )                       -> T_nat        ( fl l )
        | T_mutez      ( l )                       -> T_mutez      ( fl l )
        | T_string     ( l )                       -> T_string     ( fl l )
        | T_bytes      ( l )                       -> T_bytes      ( fl l )
        | T_address    ( l )                       -> T_address    ( fl l )
        | T_key_hash   ( l )                       -> T_key_hash   ( fl l )
        | T_operation  ( l )                       -> T_operation  ( fl l )                                   
    in

    let convert_meta :
         (Micheline_parser.location, (Micheline_parser.location, string) Micheline.node) Ligo_coq_ocaml.Compiler.ty
      -> (Mini_c.meta,               (Mini_c.meta,               string) Micheline.node) Ligo_coq_ocaml.Compiler.ty =
      fun t ->
        let make_dummy_meta = fun _ -> Mini_c.dummy_meta in
        let id x = x in
        map_ty
        make_dummy_meta
        (Micheline.map_node make_dummy_meta id)
        t
    in

    let convert_meta' :
          (Micheline_parser.location, string) Micheline.node
      ->  (Mini_c.meta, string) Micheline.node =
      fun t ->
        let make_dummy_meta = fun _ -> Mini_c.dummy_meta in
        let id x = x in
        Micheline.map_node make_dummy_meta id t
    in
    let () = ignore convert_meta in

    let decompile : Micheline_parser.node -> _ =
      fun node ->
      let dbg_print s t = let () = Format.eprintf s in t in
      node
      |> convert_meta'
      |> dbg_print "\nUnstacking...\n"
      |> Stacking.To_micheline.untranslate_type
      (* |> convert_meta *)
      |> dbg_print "\nUnscoping...\n"
      |> Scoping.untranslate_type
      |> (fun mini_c -> let () = Mini_c.PP.type_expression Format.err_formatter mini_c in mini_c) (* debug print *)
      |> dbg_print "\nUnspilling...\n"
      |> Decompile.Of_mini_c.decompile_type ~raise
      |> Aggregation.decompile_type ~raise
      |> Checking.untype_type_expression
      |> Desugaring.decompile_type_expression
      |> Purification.decompile_type_expression
      |> Tree_abstraction.Cameligo.decompile_type_expression
    in

    let param_type = decompile param_node in
    let storage_type = decompile storage_node in


    let () = ignore syntax in

    let cst = Build_cst_cameligo.toplevel param_type storage_type in
    (* let buffer = Compile.Utils.pretty_print cst in *)
    let buffer = Parsing.Cameligo.pretty_print cst in
    buffer
