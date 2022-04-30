open Api_helpers
open Tezos_utils.Micheline

module Compile   = Ligo_compile
module Helpers   = Ligo_compile.Helpers
module Location  = Simple_utils.Location

(* DEBUG PRINT *)
(* TODO : Should we keep this function somewhere ? Could be handy *)
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
  let type_expr ~(raise : Main_errors.all Trace.raise) node =
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
      | Prim (_, name,     _, _ ) -> Printf.printf "Unsupported Prim() type : %s\n" name; raise.raise @@ Main_errors.repl_unexpected
      | _ -> Printf.printf "Unsupported type so far\n"; raise.raise @@ Main_errors.repl_unexpected
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

    let toplevel ~(raise : Main_errors.all Trace.raise) param storage : Ast_core.module_ =
      let param_type_decl = type_decl param_t_name (type_expr ~raise param) in
      let storage_type_decl = type_decl storage_t_name (type_expr ~raise storage) in
      [ param_type_decl ; storage_type_decl ; main_decl ]

end (* of module Build_ast  *)

let contract_from_michelson syntax source_file display_format () =
  Trace.warning_with @@ fun _add_warning get_warnings ->
    format_result ~display_format (Ast_core.Formatter.module_format) get_warnings @@
    fun ~raise ->

      (* Check source file path *)
      let source_file =
        match Fpath.of_string source_file with
        | Ok fpath -> fpath
        | Error _ -> raise.raise @@ Main_errors.repl_unexpected (* QUESTION : What type should I use to return file-related errors ? *)
      in

      (* Read Michelson contract *)
      let file_content       =
        match Bos.OS.File.read source_file with
        | Ok str -> str
        | Error _ -> raise.raise @@ Main_errors.repl_unexpected (* QUESTION : Same here. ?*)
      in

      (* Parse Michelson contract into Micheline AST representation *)
      (* TODO : Error reporting *)
      let tokens, _error_list = Micheline_parser.tokenize file_content in
      let ast_list, _error_list = Micheline_parser.parse_toplevel tokens in

      (* Convert Micheline_parser.location to Micheline_print.location for further printing *)
      (* let convert_locations node = Micheline.map_node (fun _ -> {Micheline_printer.comment = None}) (fun x -> x) node in *)
      (* let ast_list = List.map ast_list ~f:convert_locations in *)

      (* Extract parameter and storage definition nodes from AST list *)
      let param_node, storage_node =
        match ast_list with
        | Prim (_, "parameter", [param_node], _) :: Prim (_, "storage", [storage_node], _) :: _ -> param_node, storage_node
        | _ -> Printf.printf "Could not extract parameter and storage node from contract\n"; raise.raise @@ Main_errors.repl_unexpected
      in

      (* DEBUG PRINT *)
      (* let () = Micheline_debug_print.print_toplevel Format.std_formatter ast_list in *)

      let _syntax = syntax in
      let ast = Build_ast.toplevel ~raise param_node storage_node in
      ast

      (* TODO : Error when decompiling [List is empty.], seems to come from [Decompile.Of_imperative.decompile] *)
      (* let dialect       = "terse" in *)
      (* let dialect       = Syntax_types.Dialect_name dialect in *)
      (* let n_syntax      = Syntax.of_string_opt ~raise ~dialect (Syntax_name syntax) None in *)
      (* let sugar         = Decompile.Of_core.decompile ast in
      let imperative    = Decompile.Of_sugar.decompile sugar in
      let buffer        = Decompile.Of_imperative.decompile ~raise imperative (Syntax_name syntax) in
      buffer
 *)
