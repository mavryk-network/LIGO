(* A pretty printer for JsLIGO *)

[@@@warning "-42"]

(* Jane Street dependency *)

module List = Core.List

(* Vendored dependencies *)

module Utils  = Simple_utils.Utils
module Region = Simple_utils.Region
module Option = Simple_utils.Option

(* Local dependencies *)

module CST = Cst_jsligo.CST
module PrettyComb = Parsing_shared.PrettyComb

(* Global openings *)

open CST
open! Region
open! PPrint


(* Utilities and local shadowings *)

type state = PrettyComb.state
(*
let prefix = PrettyComb.prefix
let (^/^)  = PrettyComb.(^/^)
*)

(* Placement *)

let default_state : state =
  object
    method indent       = 2
    method leading_vbar = PrettyComb.Only_on_new_line
  end
(*
(* Comments *)

let print_line_comment comment = string "//" ^^ string comment.value

let print_block_comment comment =
  string "/*" ^^ string comment.value ^^ string "*/"

let print_line_comment_opt ?(sep=empty) prefix = function
  Some comment -> prefix ^^ space ^^ print_line_comment comment ^^ hardline
| None -> prefix ^^ sep

let print_comment = function
  Wrap.Block comment -> print_block_comment comment
| Wrap.Line  comment -> print_line_comment  comment

let print_comments = function
  [] -> empty
| comments -> separate_map hardline print_comment comments ^^ hardline

(* Tokens *)

let token ?(sep=empty) (t : string Wrap.t) : document =
  let prefix = print_comments t#comments ^/^ string t#payload
  in print_line_comment_opt ~sep prefix t#line_comment

(* Enclosed documents *)

let print_enclosed_document
    state ?(force_hardline : bool option) (thread : document)
    break_size left right =
  let left, right = token left, token right in
  group (
    match force_hardline with
      None | Some false ->
        nest state#indent (left ^^ break break_size ^^ thread)
        ^^ break break_size ^^ right
    | Some true ->
        nest state#indent (left ^^ hardline ^^ thread)
        ^^ hardline ^^ right)

(* HIGHER-ORDER PRINTERS *)

let print_braces_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 1 left right

let print_braces state print
  ?(force_hardline : bool option) (node : 'a braces) =
  let {lbrace; inside; rbrace} = node.value in
  print_braces_like_document
    state ?force_hardline (print inside) lbrace rbrace

let print_brackets_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 0 left right

let print_brackets state print (node : 'a brackets) =
  let {lbracket; inside; rbracket} = node.value in
  print_brackets_like_document
    state ~force_hardline:false (print inside) lbracket rbracket

let print_chevrons_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 0 left right

let print_chevrons state print (node : 'a chevrons) =
  let {lchevron; inside; rchevron} = node.value in
  print_chevrons_like_document
    state ~force_hardline:false (print inside) lchevron rchevron

let print_par_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 0 left right

let print_par state print (node : 'a par) =
  let {lpar; inside; rpar} = node.value in
  print_par_like_document
    state ~force_hardline:false (print inside) lpar rpar

(* The separator [sep] here represents some extra spacing (like spaces
   or newlines) that will be printed after every separator in a
   sequence of type [Utils.nsepseq]. *)

let print_nsepseq :
  'a.document ->
  ('a -> document) -> ('a, lexeme Wrap.t) Utils.nsepseq -> document =
  fun sep print elements ->
    let hd, tl = elements in
    let rec separate_map = function
      []            -> empty
    | (sep', x)::xs -> token ~sep sep' ^^ print x ^^ separate_map xs
    in print hd ^^ separate_map tl

let print_sepseq :
  'a.document -> ('a -> document) ->
  ('a, lexeme wrap) Utils.sepseq -> document =
  fun sep print -> function
    None     -> empty
  | Some seq -> print_nsepseq sep print seq

let print_nseq : 'a.('a -> document) -> 'a Utils.nseq -> document =
  fun print (head, tail) -> separate_map (break 1) print (head::tail)

let print_nsep_or_term :
  'a.document -> ('a -> document) ->
  ('a, lexeme wrap) Utils.nsep_or_term -> document =
  fun sep print -> function
    `Sep  seq -> print_nsepseq sep print seq
  | `Term seq -> let print (item, term) = print item ^^ token term
                 in print_nseq print seq

let print_sep_or_term :
  'a.document -> ('a -> document) ->
  ('a, lexeme wrap) Utils.sep_or_term -> document =
  fun sep print -> function
    None     -> empty
  | Some seq -> print_nsep_or_term sep print seq

(* Enclosed structures *)

let is_enclosed_expr = function
  E_Par _ | E_Array _ | E_Object _ | E_Update _ | E_Do _ -> true
| _ -> false

let is_enclosed_statement = function
  S_Block _ -> true
| S_Expr  e -> is_enclosed_expr e
| _         -> false

let is_enclosed_type = function
  T_Par _ | T_Array _ | T_Object _ -> true
| _ -> false

(* UTILITIES *)

let (<@) f g x = f (g x)

let unroll_S_Attr (attr, stmt) =
  let rec aux attrs = function
    S_Attr (attr, stmt) -> aux (attr :: attrs) stmt
  | stmt                -> List.rev attrs, stmt
  in aux [attr] stmt

let unroll_I_Attr (attr, entry) =
  let rec aux attrs = function
    I_Attr (attr, entry) -> aux (attr :: attrs) entry
  | entry                -> List.rev attrs, entry
  in aux [attr] entry

(* PRINTING LITERALS *)

let print_bytes (node : (lexeme * Hex.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ string ("0x" ^ Hex.show (snd node#payload))
  in print_line_comment_opt prefix node#line_comment

let print_mutez (node : (lexeme * Int64.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ (Int64.to_string (snd node#payload) ^ "mutez" |> string)
  in print_line_comment_opt prefix node#line_comment

let print_ident (node : variable) = token node

let print_string (node : lexeme wrap) = dquotes (print_ident node)

and print_verbatim (node : lexeme wrap) = bquotes (print_ident node)

let print_int (node : (lexeme * Z.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ string (Z.to_string (snd node#payload))
  in print_line_comment_opt prefix node#line_comment

and print_nat (node : (lexeme * Z.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ string (Z.to_string (snd node#payload) ^ "n")
  in print_line_comment_opt prefix node#line_comment

(* PRINTING THE CST *)

let rec print state (node : CST.t) =
  let {statements; eof} = node in
  let prog = Utils.nseq_to_list statements
             |> List.map ~f:(print_statement_semi ~top:true state)
             |> separate_map hardline group
  in match eof#comments with
       [] -> prog
     | comments -> prog ^/^ print_comments comments

(*| Directive dir ->
    string (Directive.to_lexeme dir).Region.value *)

and print_statement ?top state = function
  S_Attr      s -> print_S_Attr ?top state s
| S_Block     s -> print_S_Block     state s
| S_Break     s -> print_S_Break     state s
| S_Continue  s -> print_S_Continue  state s
| S_Decl      s -> print_S_Decl ?top state s
| S_Directive s -> print_S_Directive state s
| S_Export    s -> print_S_Export    state s
| S_Expr      s -> print_S_Expr      state s
| S_For       s -> print_S_For       state s
| S_ForOf     s -> print_S_ForOf     state s
| S_If        s -> print_S_If        state s
| S_Return    s -> print_S_Return    state s
| S_Switch    s -> print_S_Switch    state s
| S_While     s -> print_S_While     state s

(*
| SCond      s -> group (print_cond_expr state s)
| SLet       s -> print_let ?top state s
| SNamespace s -> print_namespace ?top state s
*)

(* Decorated statements *)

and print_S_Attr ?top state (node : attribute * statement) =
  let attributes, stmt = unroll_S_Attr node in
  let thread = print_statement ?top state stmt
  in print_attributes state thread attributes

(* Blocks of statements *)

and print_S_Block state (node : statements braces) =
  print_block state node

and print_block state (node : statements braces) =
  let print = print_nseq (print_statement_semi state)
  in print_braces ~force_hardline:true state print node

and print_statement_semi ?top state (node : statement * semi option) =
  let statement, semi_opt = node in
  let thread = print_statement ?top state statement in
  thread ^^ Option.value_map semi_opt ~default:empty ~f:token

(* Break statement *)

and print_S_Break state (node : kwd_break) = token node

(* Continue statement *)

and print_S_Continue state (node : kwd_continue) = token node

(* Declarations as statements *)

and print_S_Decl ?top state = function
  D_Fun       d -> print_D_Fun       ?top state d
| D_Import    d -> print_D_Import         state d
| D_Interface d -> print_D_Interface      state d
| D_Namespace d -> print_D_Namespace      state d
| D_Type      d -> print_D_Type           state d
| D_Value     d -> print_D_Value     ?top state d

(* Function declaration *)

and print_D_Fun ?top state (node : fun_decl reg) =
  let {kwd_function; fun_name; type_vars;
       parameters; rhs_type; fun_body} = node.value in
  let thread = token kwd_function ^^ space ^^ print_ident fun_name in
  let thread = thread ^/^ print_type_vars_opt state type_vars in
  let thread = thread ^/^ print_fun_params state parameters in
  let thread = thread ^/^ print_type_annotation_opt state rhs_type
  in group (thread ^/^ print_fun_body state fun_body)

and print_fun_body state (node : statements braces) =
  print_block state node

and print_type_vars_opt state (node : type_vars option) =
  Option.value_map node ~default:empty ~f:(print_type_vars state)

and print_type_vars state (node : type_vars) =
  print_chevrons state (print_sep_or_term (break 1) print_ident) node

and print_fun_params state (node : fun_params) =
  let print = print_sep_or_term (break 1) (print_pattern state)
  in print_par state print node

and print_type_annotation_opt state (node : type_annotation option) =
  Option.value_map node ~default:empty ~f:(print_type_annotation state)

and print_type_annotation state (node : type_annotation) =
  let colon, type_expr = node in
  let rhs = print_type_expr state type_expr in
  group (nest state#indent (break 0 ^^ token colon ^^ space ^^ rhs))

(* Import declaration *)

and print_D_Import state = function
  ImportAlias i -> print_ImportAlias state i
| ImportAllAs i -> print_ImportAllAs state i
| ImportFrom  i -> print_ImportFrom  state i

and print_ImportAlias state (node : import_alias reg) =
  let {kwd_import; alias; equal; namespace_path} = node.value
  in group (token kwd_import ^^ space ^^ token alias
            ^^ space ^^ token equal ^^ space
            ^^ print_namespace_selection state namespace_path)

and print_namespace_selection state = function
  M_Path  s -> print_M_Path  state s
| M_Alias s -> print_M_Alias s

and print_M_Path state (node : namespace_name namespace_path reg) =
  print_namespace_path state print_ident node.value

and print_namespace_path :
  'a.state -> ('a -> document) -> 'a namespace_path -> document =
  fun state print node ->
    let {namespace_path; selector; property} = node in
    let thread = print_nsepseq (break 0) print_ident namespace_path
    in group (thread ^^ token selector ^^ break 0 ^^ print property)

and print_M_Alias (node : namespace_name) = print_ident node

and print_ImportAllAs state (node : import_all_as reg) =
  let {kwd_import; times; kwd_as; alias; kwd_from; file_path} = node.value
  in group (token kwd_import ^^ space ^^ token times ^^ space
            ^^ token kwd_as ^^ space ^^ token alias ^^ space
            ^^ token kwd_from ^^ space ^^ print_string file_path)

and print_ImportFrom state (node : import_from reg) =
  let {kwd_import; imported; kwd_from; file_path} = node.value in
  let print_idents = print_sep_or_term (break 1) print_ident in
  group (token kwd_import ^^ space ^^
         print_braces ~force_hardline:true state print_idents imported
         ^^ space ^^ token kwd_from ^^ space ^^ print_string file_path)

(* Interfaces *)

and print_D_Interface state (node : interface_decl reg) =
  let {kwd_interface; intf_name; intf_body} = node.value in
  group (token kwd_interface ^^ space ^^ token intf_name ^^ space
         ^^ print_intf_body state intf_body)

and print_intf_body state (node : intf_body) =
  print_braces ~force_hardline:true state (print_intf_entries state) node

and print_intf_entries state (node : intf_entries) =
  print_sep_or_term (break 1) (print_intf_entry state) node

and print_intf_entry state = function
  I_Attr  i -> print_I_Attr  state i
| I_Type  i -> print_I_Type  state i
| I_Const i -> print_I_Const state i

and print_I_Attr state (node : attribute * intf_entry) =
  let attributes, entry = unroll_I_Attr node in
  let thread = print_intf_entry state entry
  in print_attributes state thread attributes

and print_I_Type state (node : intf_type reg) =
  let {kwd_type; type_name; type_rhs} = node.value in
  let thread = token kwd_type ^^ space ^^ token type_name
  in group (print_rhs state thread type_rhs)

and print_rhs state thread (node : (equal * type_expr) option) =
  let print state (eq, type_expr) =
    let rhs = print_type_expr state type_expr in
    if is_enclosed_type type_expr
    then thread ^^ space ^^ token eq ^^ space ^^ rhs
    else thread ^^ prefix state#indent 1 (space ^^ token eq) rhs
  in Option.value_map node ~default:thread ~f:(print state)

and print_I_Const state (node : intf_const reg) =
  let {kwd_const; const_name; const_type} = node.value in
  let thread = token kwd_const ^^ space ^^ token const_name
  in group (thread ^/^ print_type_annotation state const_type)

(* Namespace declaration *)

and print_D_Namespace state (node : namespace_decl reg) =
  let {kwd_namespace; namespace_name;
       namespace_type; namespace_body} = node.value in
  let thread = token kwd_namespace ^^ space ^^ token namespace_name in
  let thread = print_namespace_type state thread namespace_type
  in group (thread ^^ print_block state namespace_body)

and print_namespace_type state thread (node: interface option) =
  Option.value_map node ~default:thread ~f:(print_interface state)

and print_interface state (node : interface) =
  let kwd_implements, e = node.value in
  token kwd_implements ^^ space ^^ print_intf_expr state e

and print_intf_expr state = function
  I_Body i -> print_I_Body state i
| I_Path i -> print_I_Path state i

and print_I_Body state (node : intf_body) = print_intf_body state node

and print_I_Path state (node : namespace_selection) =
  print_namespace_selection state node

(* Type declarations *)

and print_D_Type state (node : type_decl reg) =
  let {kwd_type; name; type_vars; eq; type_expr} = node.value in
  let thread = token kwd_type ^^ space ^^ token name in
  let thread = thread ^^ print_type_vars_opt state type_vars in
  let rhs = print_type_expr state type_expr in
  if is_enclosed_type type_expr
  then thread ^^ space ^^ token eq ^^ space ^^ rhs
  else thread ^^ prefix state#indent 1 (space ^^ token eq) rhs

(* XXX *)

(* Expressions as statements *)

and print_S_Expr state (node: attribute list * expr) =
  let attr, expr = node in
  let expr_doc   = print_expr state expr in
  if List.is_empty attr then expr_doc
  else print_attributes state attr ^/^ expr_doc

and print_for state (node: for_stmt reg) =
  let { attributes; kwd_for; lpar; initialiser;
        semi1; condition; semi2; afterthought; rpar;
        statement} = node.value in
  let par =
    Option.value_map initialiser ~default:space ~f:(print_statement state)
    ^^ token semi1
    ^^ Option.value_map condition ~default:empty
                        ~f:(fun expr -> break 1 ^^ print_expr state expr)
    ^^ token semi2
    ^^ Option.value_map afterthought ~default:empty
         ~f:(fun seq -> break 1 ^^ print_nsepseq (break 1) (print_expr state) seq)
  in
  let par = print_par_like_document state par lpar rpar
  in print_attributes state attributes ^^
  token kwd_for ^^ space ^^ par ^^ space ^^
  Option.value_map statement ~default:empty ~f:(print_statement state)

and print_for_of state (node: for_of reg) =
  let {kwd_for; lpar; index_kind; index; kwd_of;
       expr; rpar; statement; _} = node.value in
  let par =
    print_index_kind index_kind ^^ space
    ^^ token index ^^ space ^^ token kwd_of ^^ space
    ^^ print_expr state expr in
  let par = print_par_like_document state par lpar rpar
  in token kwd_for ^^ space ^^ par ^^ space ^^ print_statement state statement

and print_index_kind = function
  `Let   i -> token i
| `Const i -> token i

and print_while state (node: while_stmt reg) =
  let {kwd_while; lpar; expr; rpar; statement} = node.value in
  token kwd_while ^^ space
  ^^ print_par_like_document state (print_expr state expr) lpar rpar
  ^^ space ^^ print_statement state statement

and print_export state {value = (kwd_export, statement); _} =
  token kwd_export ^^ space ^^ print_statement state statement

and print_namespace ?top state (node: namespace_statement reg) =
  let kwd_namespace, name, interface_annotation, statements, attributes = node.value in
  let top = match top with Some true -> true | _ -> false in
  let is_private =
    List.exists ~f:(fun a -> String.equal (fst a#payload) "private")
                attributes in
  let attributes = filter_private attributes in
  let print_statements = print_nsepseq (break 1) (print_statement state) in
  group (
    (if List.is_empty attributes then empty
     else print_attributes state attributes)
    ^/^ token kwd_namespace ^^ space ^^ token name
    ^^ (if ((top && is_private) || not top) then empty
        else string "export" ^^ space)
    ^^ space
    ^^ (match interface_annotation with None -> empty | Some ia -> print_interface_annotation state ia ^^ space)
    ^^ print_braces state ~force_hardline:true print_statements statements)

and print_interface_annotation state ({value; _}: interface_annotation) =
  let kwd_implements, interface_expr = value in
  token kwd_implements ^^ space ^^ print_interface_expr state interface_expr

and print_interface_expr state = function
    IInterface ib -> print_interface_body state ib
  | IPath p -> print_nsepseq empty (fun a -> string a#payload) p.value

and print_cond_expr state {value; _} =
  let {attributes; test; ifso; ifnot; _} = value in
  let if_then =
    token value.kwd_if ^^ space ^^ print_par_expr state test ^^ space
    ^^ print_statement state ifso in
  let cond_doc =
    match ifnot with
      None -> if_then
    | Some (kwd_else, statement) ->
        if_then ^^ space ^^ token kwd_else ^^ space
        ^^ print_statement state statement in
  if List.is_empty attributes then cond_doc
  else print_attributes state attributes ^/^ cond_doc

and print_return state {value = {kwd_return; expr}; _} =
  match expr with
    Some s -> token kwd_return ^^ space ^^ print_expr state s
  | None -> token kwd_return

and filter_private (attributes: CST.attribute list) : CST.attribute list =
  List.filter ~f:(fun (v: CST.attribute) -> not (String.equal (fst v#payload) "private")) attributes

and print_let_or_const ?top state (node : (let_decl reg, const_decl reg) Either.t) : document =
  let attributes, kwd, bindings =
    Either.fold node
      ~left:(fun ({value; _} : let_decl reg) -> value.attributes,
                                                value.kwd_let,
                                                value.bindings)
      ~right:(fun ({value; _} : const_decl reg) -> value.attributes,
                                                   value.kwd_const,
                                                   value.bindings) in
  let top = match top with Some true -> true | _ -> false in
  let is_private =
    List.exists ~f:(fun a -> String.equal (fst a#payload) "private")
                attributes in
  let attributes = filter_private attributes in
  print_attributes state attributes ^/^
  (if (top && is_private || not top) then empty else string "export" ^^ space)
  ^^ token kwd ^^ space
  ^^ print_nsepseq (break 1) (print_val_binding state) bindings

and print_let ?top state (node : let_decl reg) =
  print_let_or_const ?top state (Left node)

and print_const state (node : const_decl reg) = print_let_or_const state (Right node)

and print_val_binding state {value = {binders; type_params = _; lhs_type; eq; expr}; _} =
  (* In case the RHS is a lambda function, we want to try to display
     it in the same line instead of causing a line break. For example,
     we want to see this:

     let f = (x: int) => x

     And not this:

     let f =
       (x: int) => x
  *)
  let join_lhs_with_rhs =
    function
      EFun _ -> ( ^^ )
    | expr when is_enclosed_expr expr -> ( ^^ )
    | _      -> prefix state#indent 0
  in
  join_lhs_with_rhs expr
    ((match lhs_type with
        Some (colon, type_expr) ->
          print_pattern state binders ^^ print_type_annot_rhs state colon type_expr
      | None -> print_pattern state binders)
     ^^ space ^^ token eq ^^ space)
    (print_expr state expr)

and print_switch state {value = {kwd_switch; lpar; expr; rpar; lbrace; cases; rbrace}; _} =
  token kwd_switch ^^ space ^^ print_par_like_document state (print_expr state expr) lpar rpar
  ^^ space ^^ print_braces_like_document state ~force_hardline:true (print_cases state cases) lbrace rbrace

and print_cases state (hd, tl) =
  List.fold ~f:(fun a i -> a ^^ break 0 ^^ print_case state i) ~init:(print_case state hd) tl

and print_case state node =
  let print_statements statements =
    group (
      match statements with
        Some s ->
          let app s = group (print_statement state s)
          in print_nsepseq hardline app s ^^ semi
      | None -> hardline)
  in
  let print_label_and_statements label =
    function
      Some (s, []) when is_enclosed_statement s ->
        label ^^ space ^^ group (print_statement state s) ^^ semi
    | statements ->
        hang state#indent (label ^/^ print_statements statements)
  in
  match node with
    Switch_case {kwd_case; expr; colon; statements} ->
      let label =
        token kwd_case ^^ space ^^ print_expr state expr ^^ token colon
      in print_label_and_statements label statements
  | Switch_default_case {kwd_default; colon; statements} ->
      let label = token kwd_default ^^ token colon
      in print_label_and_statements label statements

and print_expr state = function
  EFun      e -> print_fun state e
| EPar      e -> print_par_expr state e.value
| ESeq      e -> print_seq state e
| EVar      e -> print_ident e
| EModA     e -> print_module_access (print_expr state) e
| ELogic    e -> print_logic_expr state e
| EArith    e -> group (print_arith_expr state e)
| ECall     e -> print_call_expr state e
| EBytes    e -> print_bytes e
| EArray    e -> print_array state e
| EObject   e -> group (print_object_expr state e)
| EString   e -> print_string_expr e
| EProj     e -> print_projection state e
| EAssign   e -> print_assign state e
| EAnnot    e -> group (print_annot_expr state e)
| EConstr   e -> print_constr_expr state e
| EUnit     e -> token (fst e.value) ^^ token (snd e.value)
| ECodeInj  e -> print_code_inj state e
| ETernary  e -> print_ternary state e
| EContract e -> print_contract state e
| EPrefix   e -> print_prefix e
| EPostfix  e -> print_postfix e

and print_code_inj state (node: code_inj reg) =
  let {language; code} = node.value in
  let language = token language in
  let code     = print_expr state code in
  group (language ^/^ code)

and print_array state (node: (array_item, comma) Utils.sepseq brackets reg) =
  match node.value.inside with
    Some inside ->
      let print_items = print_nsepseq (break 1) (print_array_item state) in
      print_brackets_like_document state (print_items inside) node.value.lbracket node.value.rbracket
  | None ->
      print_brackets state (fun _ -> empty) node

and print_call_expr state {value; _} =
  let lambda, arguments = value in
  let lpar, arguments, rpar =
    match arguments with
    | Unit unit -> fst (unit.value), None, snd (unit.value)
    | Multiple xs -> xs.value.lpar, Some xs.value.inside, xs.value.rpar
  in
  let arguments =
    print_par_like_document state
      (Option.value_map ~default:empty ~f:(print_nsepseq (break 1) (print_expr state)) arguments)
      lpar
      rpar
  in
  print_expr state lambda ^^ arguments

and print_array_item state = function
  Expr_entry e -> print_expr state e
| Rest_entry {value = {expr; ellipsis}; _} -> token ellipsis ^^ print_expr state expr

and print_constr_expr state {value; _} =
  let constr, arg = value in
  let constr = token constr in
  (* FIXME: parentheses in ctor_expr are not saved. *)
  let left = Wrap.ghost "(" in
  let right = Wrap.ghost ")" in
  constr ^^ group (
    Option.value_map
      ~default:(string "()")
      ~f:(fun exp -> print_par_like_document state (print_expr state exp) left right)
      arg)

and print_object_property state = function
  Punned_property {value; _} ->
    print_expr state value
| Property {value = {name; colon; value; _}; _} ->
    print_expr state name ^^ token colon ^^ space ^^ print_expr state value
| Property_rest {value = {expr; ellipsis}; _} ->
    token ellipsis ^^ print_expr state expr

and print_object_expr state (node: (property, comma) Utils.nsepseq braces reg) =
  let print_properties = print_nsepseq (break 1) (print_object_property state)
  in print_braces state print_properties node

and print_string_expr = function
  String e -> print_string e
| Verbatim e -> print_verbatim e

and print_selection state = function
  FieldName {value = {dot; value}; _} -> token dot ^^ print_ident value
| Component value -> print_brackets state (print_expr state) value

and print_projection state {value = {expr; selection}; _} =
  print_expr state expr ^^ print_selection state selection

and print_infix state lhs middle rhs =
  group (lhs ^^ space ^^ prefix state#indent 1 middle rhs)

and print_assign state (a, op, b) =
  let operator = match op.value with
      Eq                           -> "="
    | Assignment_operator Times_eq -> "*="
    | Assignment_operator Div_eq   -> "/="
    | Assignment_operator Min_eq   -> "-="
    | Assignment_operator Plus_eq  -> "+="
    | Assignment_operator Mod_eq   -> "%="
  in
  print_infix state (print_expr state a) (string operator) (print_expr state b)

and print_annot_expr state {value; _} =
  let expr, kwd_as, type_expr = value in
  let lhs = print_expr state expr in
  let rhs = print_type_expr state type_expr in
  print_infix state lhs (token kwd_as) rhs

and print_ternary state {value; _} =
  print_expr state value.condition ^^
  space ^^ token value.qmark ^^ space ^^
  nest state#indent (print_expr state value.truthy) ^^
  space ^^ token value.colon ^^ space ^^
  nest state#indent (print_expr state value.falsy)

and print_prefix = function
  {value = {update_type=Increment _; variable}; _} ->
    plus ^^ plus ^^ print_ident variable
| {value = {update_type=Decrement _; variable}; _} ->
    minus ^^ minus ^^ print_ident variable

and print_postfix = function
  {value = {update_type=Increment _; variable}; _} ->
    print_ident variable ^^ plus  ^^ plus
| {value = {update_type=Decrement _; variable}; _} ->
    print_ident variable ^^ minus ^^ minus

and print_logic_expr state = function
  BoolExpr e -> print_bool_expr state e
| CompExpr e -> print_comp_expr state e

and print_bool_expr state = function
  Or   e  -> print_bin_op state e
| And  e  -> print_bin_op state e
| Not  e  -> print_un_op  state e

and print_bin_op state {value; _} =
  let {arg1; arg2; op} = value in
  let lhs = print_expr state arg1 in
  let rhs = print_expr state arg2 in
  print_infix state lhs (token op) rhs

and print_un_op state {value; _} =
  let {arg; op} = value in
  token op ^^ print_expr state arg

and print_comp_expr state = function
  Lt    e -> print_bin_op state e
| Leq   e -> print_bin_op state e
| Gt    e -> print_bin_op state e
| Geq   e -> print_bin_op state e
| Equal e -> print_bin_op state e
| Neq   e -> print_bin_op state e

and print_arith_expr state = function
  Add   e -> print_bin_op state e
| Sub   e -> print_bin_op state e
| Mult  e -> print_bin_op state e
| Div   e -> print_bin_op state e
| Mod   e -> print_bin_op state e
| Neg   e -> print_un_op  state e
| Int   e -> print_int e

and print_par_expr state (node: expr par) =
  let {lpar; inside; rpar} = node in
  print_par_like_document state (print_expr state inside) lpar rpar

and print_type_annot_rhs state colon value =
  group (nest state#indent (break 0 ^^ token colon ^^ space
                            ^^ print_type_expr state value))

(* In flat mode, we may render the arguments like so:

   let f = (x: int, y: int): int => /* */

   Otherwise we'll try to render it like so:

   let f = (
     x: int,
     y: int
   ): int => /* */
*)

and print_expr_fun state = function
  EPar node -> print_par state (print_expr_fun state) node
| ESeq {value; _} ->
    print_nsepseq (break 1) (print_expr_fun state) value
| EAnnot {value; _} ->
    let expr, kwd_as, type_expr = value in
    print_expr_fun state expr ^^ print_type_annot_rhs state kwd_as type_expr
| EUnit {value; _} -> token (fst value) ^^ token (snd value)
| c -> print_expr state c

and print_fun state {value; _} =
  let {type_params; parameters; lhs_type; arrow; body} = value in
  let type_params = print_type_params state type_params in
  let parameters = print_expr_fun state parameters in
  let annot =
    match lhs_type with
      None            -> empty
    | Some (colon, e) -> print_type_annot_rhs state colon e
  in
  match body with
  | FunctionBody fb ->
      let print_statements = print_nsepseq (break 1) (print_statement state) in
      (* If the function has only one statement we may try to display
         it inline rather than in a new one.  *)
      let force_hardline = not @@ List.is_empty @@ snd fb.value.inside in
      type_params ^^ parameters ^^ annot ^^ space ^^ token arrow ^^ space
      ^^ print_braces state ~force_hardline print_statements fb
  | ExpressionBody e ->
      prefix state#indent 1
        (type_params ^^ parameters ^^ annot ^^ space ^^ token arrow)
        (print_expr state e)

and print_seq state {value; _} =
  print_nsepseq (break 1) (print_expr state) value

and print_disc state value = print_disc_or_sum state (Either.left value)

and print_parameter {value; _} =
    string "parameter_of"
    ^^ group (nest 0 (break 1 ^^ print_nsepseq (break 1) print_ident value))

and print_type_expr state: type_expr -> document = function
  TProd      t -> print_cartesian state t
| TSum       t -> print_sum_type state t
| TObject    t -> print_object_type state t
| TApp       t -> print_type_app state t
| TFun       t -> print_fun_type state t
| TPar       t -> print_type_par state t
| TVar       t -> print_ident t
| TString    s -> print_string s
| TModA      t -> print_module_access (print_type_expr state) t
| TInt       t -> print_int t
| TDisc      t -> print_disc state t
| TParameter t -> print_parameter t

and print_module_access : type a. (a -> document) -> a module_access reg -> document
= fun f {value; _} ->
  let {module_name; selector; field} = value in
  group (print_ident module_name ^^ token selector ^^ break 0 ^^ f field)

and print_cartesian state (node: CST.cartesian) =
  let print_type_exprs = print_nsepseq (break 1) (print_type_expr state) in
  prefix state#indent 1
    (print_attributes state node.attributes)
    (print_brackets state print_type_exprs node.inside)

and print_sum_type state (node : sum_type reg) = print_disc_or_sum state (Either.right node)

and print_disc_or_sum state (value : ((obj_type, vbar) Utils.nsepseq, sum_type reg) Either.t) =
  (* We provide an extra layer of indentation so that instead of this:

     type t =
       | [
         "Alt",
         unit
         ]

     We will get:

     type t =
       | [
           "Alt",
           unit
         ]
   *)
  let variants, attributes =
    let open Simple_utils.Function in
    Either.fold value
      ~left:(fun (disc : (obj_type, vbar) Utils.nsepseq) ->
               let variants =
                 Utils.nsepseq_map
                   (nest state#indent <@ print_object_type state) disc
               in variants, [])
      (* JsLIGO's lexer always injects leading vertical bars in sum
         types, so we don't check for them here. *)
      ~right:(fun {value = {leading_vbar = _; variants; attributes}; _} ->
                let variants =
                  Utils.nsepseq_map
                    (nest state#indent <@ print_variant state) variants.value
                in variants, attributes)
  in
  let head, tail = variants in
  let padding_flat =
    let open PrettyComb in
    match state#leading_vbar with
      Avoid | Only_on_new_line -> empty
    | Always -> bar ^^ space
  in
  let padding_non_flat =
    let open PrettyComb in
    match state#leading_vbar with
      Avoid -> blank state#indent
    | Always | Only_on_new_line -> bar ^^ space
  in

  (* Do not append a vertical bar if we are in flat mode, unless we
     have attributes. The reason is that those two are different:

     type t = [@annot] | Ctor
     type t = [@annot] Ctor
  *)

  let head =
    if List.is_empty tail || not (List.is_empty attributes)
    then bar ^^ space ^^ head
    else ifflat padding_flat padding_non_flat ^^ head
  in
  let app (bar, variant) = break 1 ^^ token bar ^^ space ^^ variant in
  let thread = group (head ^^ concat_map app tail) in
  if attributes = [] then thread
  else group (print_attributes state attributes ^/^ thread)

and print_variant state (node : variant reg) =
  let {tuple; attributes; _} = node.value in
  let tuple = print_brackets state (print_variant_comp state) tuple in
  if List.is_empty attributes then tuple
  else group (print_attributes state attributes ^/^ tuple)

and print_variant_comp state (node: variant_comp) =
  let {constr; params} = node in
  let constr = print_string constr in
  match params with
    None -> constr
  | Some (comma, params) ->
      group (constr ^^ token comma ^^ break 1
             ^^ print_nsepseq (break 1) (print_type_expr state) params)

and print_attribute state (node : Attr.t wrap) =
  let cst_attr = ["entry"; "inline"; "view"; "no_mutation";
                  "private"; "public"; "hidden"; "thunk"] in
  let key, val_opt = node#payload in
  let thread =
    if List.mem cst_attr key ~equal:String.equal then
      string "@" ^^ string key
    else string "// @" ^^ string key in
  let thread = match val_opt with
                 Some Ident value ->
                   group (thread ^/^ nest state#indent (string value))
               | Some String value ->
                   group (thread ^/^
                          nest state#indent (string ("\"" ^ value ^ "\"")))
               | None -> thread
  in print_comments node#comments ^/^ thread

and print_attributes state = function
  []    -> empty
| attrs -> separate_map (break 0) (print_attribute state) attrs

and print_object_type state fields =
  group (print_ne_injection state (print_field_decl state) fields)

and print_field_decl state {value; _} =
  let {field_name; colon; field_type; attributes} = value in
  let attr = print_attributes state attributes in
  let name = if List.is_empty attributes then print_ident field_name
             else attr ^/^ print_ident field_name in
  match field_type with
    TVar v when String.equal v#payload field_name#payload -> name
  | _ -> let t_expr = print_type_expr state field_type in
         group (name ^^ token colon ^^ space ^^ group t_expr)

and print_ne_injection :
  'a.state -> ('a -> document) -> 'a ne_injection reg -> document =
  fun state print {value; _} ->
    let {compound; ne_elements; attributes; _} = value in
    let elements = print_nsepseq hardline print ne_elements in
    let inj =
      match compound with
        None -> elements
      | Some Braces (lbrace, rbrace) ->
        print_braces_like_document state elements lbrace rbrace
      | Some Brackets (lbracket, rbracket) ->
        print_brackets_like_document state elements lbracket rbracket
    in
    let inj = if List.is_empty attributes then inj
              else hardline ^^ print_attributes state attributes ^/^ inj
    in inj

and print_type_app state (node: (type_constr * type_params) reg) =
  let ctor, tuple = node.value in
  print_ident ctor ^^ print_type_tuple state tuple

and print_type_tuple state (node: type_params) =
  let {lchevron; inside; rchevron} = node.value in
  print_chevrons_like_document
    state
    (print_nsepseq (break 1) (print_type_expr state) inside)
    lchevron
    rchevron

and print_fun_type_arg state ({name; colon; type_expr} : CST.fun_type_arg) =
  group (print_ident name ^^ token colon ^^ space
         ^^ print_type_expr state type_expr)

and print_fun_type state {value; _} =
  let lhs, arrow, rhs = value in
  let lhs = print_par_like_document
             state
             (print_nsepseq (break 1) (print_fun_type_arg state) lhs.inside)
             lhs.lpar
             lhs.rpar in
  let rhs = print_type_expr state rhs in
  group (lhs ^^ space ^^ token arrow ^^ space ^^ rhs)

and print_type_par state value = print_par state (print_type_expr state) value

and print_pattern state = function
  PRest     p -> print_rest_pattern p
| PAssign   p -> print_assign_pattern state p
| PVar      v -> print_pvar state v
| PConstr   p -> print_ident p
| PDestruct p -> print_destruct state p
| PObject   p -> print_pobject state p
| PArray    p -> print_parray state p

and print_parray state (node: (pattern, comma) Utils.nsepseq brackets reg) =
  let print_patterns = print_nsepseq (break 1) (print_pattern state)
  in group (print_brackets state print_patterns node)

and print_pobject state (node: (pattern, comma) Utils.nsepseq braces reg) =
  print_braces state (print_nsepseq (break 1) (print_pattern state)) node

and print_pvar state {value; _} =
  let {variable; attributes} = value in
  let v = print_ident variable in
  if List.is_empty attributes then v
  else group (print_attributes state attributes ^/^ v)

and print_rest_pattern {value = {ellipsis; rest}; _} =
  token ellipsis ^^ print_ident rest

and print_assign_pattern state {value = {property; eq; value}; _} =
  print_ident property ^^ token eq ^^ print_expr state value

and print_destruct state {value = {property; colon; target}; _} =
  print_ident property ^^ token colon ^^ print_val_binding state target

and print_contract state {value; _} =
  (* FIXME: parentheses are not saved *)
  let left  = Wrap.ghost "("
  and right = Wrap.ghost ")" in
  string "contract_of"
  ^^ space
  ^^ print_par_like_document
    state
    (group (nest 0 (break 0 ^^ print_nsepseq (break 0) print_ident value)))
    left
    right

let print_type_expr = print_type_expr
let print_pattern   = print_pattern
let print_expr      = print_expr
let print_statement = print_statement
*)

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern
type statement = CST.statement


let print           _state _cst = PPrint.empty
let print_expr      _state _cst = PPrint.empty
let print_type_expr _state _cst = PPrint.empty
let print_pattern   _state _cst = PPrint.empty
let print_statement _state _cst = PPrint.empty
