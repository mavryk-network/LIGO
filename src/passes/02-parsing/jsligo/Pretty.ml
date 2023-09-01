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

let prefix = PrettyComb.prefix
let (^/^)  = PrettyComb.(^/^)

(* Placement *)

let default_state : state =
  object
    method indent       = 2
    method leading_vbar = PrettyComb.Only_on_new_line
  end

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

(*
let print_brackets_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 0 left right

let print_brackets state print (node : 'a brackets) =
  let {lbracket; inside; rbracket} = node.value in
  print_brackets_like_document
    state ~force_hardline:false (print inside) lbracket rbracket
*)

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

(*
let print_sepseq :
  'a.document -> ('a -> document) ->
  ('a, lexeme wrap) Utils.sepseq -> document =
  fun sep print -> function
    None     -> empty
  | Some seq -> print_nsepseq sep print seq
*)

let print_nseq : 'a.document -> ('a -> document) -> 'a Utils.nseq -> document =
  fun sep print (head, tail) -> separate_map sep print (head::tail)

let print_nsep_or_term :
  'a.document -> ('a -> document) ->
  ('a, lexeme wrap) Utils.nsep_or_term -> document =
  fun sep print -> function
    `Sep  seq -> print_nsepseq sep print seq
  | `Term seq -> let print (item, term) = print item ^^ token term
                 in print_nseq sep print seq

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

(* let (<@) f g x = f (g x) *)

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

let unroll_T_Attr (attr, t_expr) =
  let rec aux attrs = function
    T_Attr (attr, t_expr) -> aux (attr :: attrs) t_expr
  | t_expr                -> List.rev attrs, t_expr
  in aux [attr] t_expr

let unroll_P_Attr (attr, pattern) =
  let rec aux attrs = function
    P_Attr (attr, pattern) -> aux (attr :: attrs) pattern
  | pattern                -> List.rev attrs, pattern
  in aux [attr] pattern

let unroll_E_Attr (attr, expr) =
  let rec aux attrs = function
    E_Attr (attr, expr) -> aux (attr :: attrs) expr
  | expr                -> List.rev attrs, expr
  in aux [attr] expr

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
             |> List.map ~f:(print_statement_semi state)
             |> separate_map hardline group
  in match eof#comments with
       [] -> prog
     | comments -> prog ^/^ print_comments comments

and print_statement state = function
  S_Attr      s -> print_S_Attr      state s
| S_Block     s -> print_S_Block     state s
| S_Break     s -> print_S_Break     state s
| S_Continue  s -> print_S_Continue  state s
| S_Decl      s -> print_S_Decl      state s
| S_Directive s -> print_S_Directive state s
| S_Export    s -> print_S_Export    state s
| S_Expr      s -> print_S_Expr      state s
| S_For       s -> print_S_For       state s
| S_ForOf     s -> print_S_ForOf     state s
| S_If        s -> print_S_If        state s
| S_Return    s -> print_S_Return    state s
| S_Switch    s -> print_S_Switch    state s
| S_While     s -> print_S_While     state s

(* Decorated statements *)

and print_S_Attr state (node : attribute * statement) =
  let attributes, stmt = unroll_S_Attr node in
  let thread = print_statement state stmt
  in print_attributes state thread attributes

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
                   thread ^/^ nest state#indent (string value)
               | Some String value ->
                   thread ^/^
                   nest state#indent (string ("\"" ^ value ^ "\""))
               | None -> thread
  in group (print_comments node#comments ^/^ thread)

and print_attributes state thread = function
  []    -> thread
| attrs -> separate_map (break 0) (print_attribute state) attrs ^^ thread

(* Blocks of statements *)

and print_S_Block state (node : statements braces) =
  print_block state node

and print_block state (node : statements braces) =
  print_braces ~force_hardline:true state (print_statements state) node

and print_statements state (node : statements) =
  print_nseq (break 1) (print_statement_semi state) node

and print_statement_semi state (node : statement * semi option) =
  let statement, semi_opt = node in
  let thread = print_statement state statement in
  thread ^^ Option.value_map semi_opt ~default:empty ~f:token

(* Break statement *)

and print_S_Break state (node : kwd_break) = token node

(* Continue statement *)

and print_S_Continue state (node : kwd_continue) = token node

(* Declarations as statements *)

and print_S_Decl state (node : declaration) = print_declaration state node

and print_declaration state = function
  D_Fun       d -> print_D_Fun       state d
| D_Import    d -> print_D_Import    state d
| D_Interface d -> print_D_Interface state d
| D_Namespace d -> print_D_Namespace state d
| D_Type      d -> print_D_Type      state d
| D_Value     d -> print_D_Value     state d

(* Function declaration *)

and print_D_Fun state (node : fun_decl reg) =
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
  group (thread ^^
         if is_enclosed_type type_expr
         then space ^^ token eq ^^ space ^^ rhs
         else prefix state#indent 1 (space ^^ token eq) rhs)

(* Value declarations *)

and print_D_Value state (node : value_decl reg) =
  let {kind; bindings} = node.value in
  let thread = print_var_kind state kind ^^ space in
  group (thread ^^
         print_nsepseq (break 1) (print_val_binding state) bindings)

and print_var_kind state = function
  `Let   kwd_let   -> token kwd_let
| `Const kwd_const -> token kwd_const

and print_val_binding state (node : val_binding reg) =
  let {pattern; type_vars; rhs_type; eq; rhs_expr} = node.value in
  let thread = print_pattern state pattern in
  let thread = thread ^^ space ^^
  match rhs_type with
    None -> empty
  | Some (colon, type_expr) ->
      let rhs = print_type_vars_opt state type_vars
                ^^ print_type_expr state type_expr in
      nest state#indent (break 0 ^^ token colon ^^ space ^^ rhs) in
  let thread = thread ^^ space ^^ token eq ^^ space
  in prefix state#indent 1 thread (print_expr state rhs_expr)

(* Preprocessing directives *)

and print_S_Directive state (node : Directive.t) =
  string (Directive.to_lexeme node).Region.value

(* Export statements *)

and print_S_Export state (node : export_stmt reg) =
  let kwd_export, declaration = node.value in
  token kwd_export ^^ space ^^ print_declaration state declaration

(* Expressions as statements *)

and print_S_Expr state (node : expr) = print_expr state node

(* For-loops *)

and print_S_For state (node : for_stmt reg) =
  let {kwd_for; range; for_body} = node.value in
  let thread = token kwd_for ^^ space ^^ print_range_for state range in
  match for_body with
    None -> thread
  | Some stmt -> prefix state#indent 1 thread (print_statement state stmt)

and print_range_for state (node : range_for par) =
  let {lpar; inside; rpar} = node.value in
  let {initialiser; semi1; condition; semi2; afterthought} = inside in
  let par =
    Option.value_map initialiser ~default:space ~f:(print_statement state)
    ^^ token semi1
    ^^ Option.value_map condition ~default:empty
                        ~f:(fun expr -> break 1 ^^ print_expr state expr)
    ^^ token semi2
    ^^ Option.value_map afterthought ~default:empty
      ~f:(fun s -> break 1 ^^ print_nsepseq (break 1) (print_expr state) s)
  in print_par_like_document state par lpar rpar

(* For-of loops *)

and print_S_ForOf state (node: for_of_stmt reg) =
  let {kwd_for; range; for_of_body} = node.value in
  let thread = token kwd_for ^^ space ^^ print_range_for_of state range
  in group (thread ^^ space ^^ print_statement state for_of_body)

and print_range_for_of state (node : range_of par) =
  let {lpar; inside; rpar} = node.value in
  let {index_kind; index; kwd_of; expr} = inside in
  let par =
    print_var_kind state index_kind ^^ space ^^ token index
    ^^ space ^^ token kwd_of ^^ space ^^ print_expr state expr
  in print_par_like_document state par lpar rpar

(* Conditional statement *)

and print_S_If state (node : if_stmt reg) =
  let {kwd_if; test; if_so; if_not} = node.value in
  let thread = token kwd_if ^^ space ^^ print_par_expr state test in
  let thread = thread ^^ space ^^ print_statement_semi state if_so in
  let thread = match if_not with
                 None -> thread
               | Some (kwd_else, stmt) ->
                   thread ^^ space ^^ token kwd_else ^^ space
                   ^^ print_statement state stmt
  in group thread

and print_par_expr state (node : expr par) =
  let {lpar; inside; rpar} = node.value in
  print_par_like_document state (print_expr state inside) lpar rpar

(* Return statement *)

and print_S_Return state (node : return_stmt reg) =
  match node.value with
    kwd_return, None -> token kwd_return
  | kwd_return, Some expr ->
      group (token kwd_return ^^ space ^^ print_expr state expr)

and print_S_Switch state (node : switch_stmt reg) =
  let {kwd_switch; subject; cases} = node.value in
  let {lbrace; inside; rbrace} = cases.value in
  let thread = token kwd_switch ^^ space ^^ print_par_expr state subject in
  let thread = thread ^^ space ^^
               print_braces_like_document state ~force_hardline:true
                 (print_cases state inside) lbrace rbrace
  in group thread

and print_cases state = function
  AllCases c -> print_AllCases state c
| Default  c -> print_Default  state c

and print_AllCases state (node : all_cases) =
  let switch_cases, default_opt = node in
  let thread = print_switch_cases state switch_cases in
  match default_opt with
    None -> thread
  | Some default -> thread ^^ hardline ^^ print_switch_default state default

and print_switch_cases state (node : switch_case reg Utils.nseq) =
  print_nseq hardline (print_switch_case state) node

and print_switch_case state (node : switch_case reg) =
  let {kwd_case; expr; colon; case_body} = node.value in
  let thread = token kwd_case ^^ space ^^ print_expr state expr in
  let thread = thread ^^ token colon in
  print_label_and_statements state thread case_body

and print_Default state (node : switch_default reg) =
  print_switch_default state node

and print_switch_default state (node : switch_default reg) =
  let {kwd_default; colon; default_body} = node.value in
  let thread = token kwd_default ^^ token colon in
  print_label_and_statements state thread default_body

and print_label_and_statements state label = function
  None -> label
| Some ((stmt,_), []) when is_enclosed_statement stmt ->
    label ^^ space ^^ group (print_statement state stmt)
| Some stmts ->
    hang state#indent (label ^/^ print_statements state stmts)

(* While-loop *)

and print_S_While state (node : while_stmt reg) =
  let {kwd_while; invariant; while_body} = node.value in
  let thread = token kwd_while ^^ space ^^ print_par_expr state invariant
  in group (thread ^^ space ^^ print_statement state while_body)

(* EXPRESSIONS *)

and print_expr state = function
  E_Add        e -> print_E_Add        state e
| E_AddEq      e -> print_E_AddEq      state e
| E_And        e -> print_E_And        state e
| E_App        e -> print_E_App        state e
| E_Array      e -> print_E_Array      state e
| E_ArrowFun   e -> print_E_ArrowFun   state e
| E_Assign     e -> print_E_Assign     state e
| E_Attr       e -> print_E_Attr       state e
| E_BitAnd     e -> print_E_BitAnd     state e
| E_BitAndEq   e -> print_E_BitAndEq   state e
| E_BitNeg     e -> print_E_BitNeg     state e
| E_BitOr      e -> print_E_BitOr      state e
| E_BitOrEq    e -> print_E_BitOrEq    state e
| E_BitSl      e -> print_E_BitSl      state e
| E_BitSlEq    e -> print_E_BitSlEq    state e
| E_BitSr      e -> print_E_BitSr      state e
| E_BitSrEq    e -> print_E_BitSrEq    state e
| E_BitXor     e -> print_E_BitXor     state e
| E_BitXorEq   e -> print_E_BitXorEq   state e
| E_Bytes      e -> print_E_Bytes            e
| E_CodeInj    e -> print_E_CodeInj    state e
| E_ContractOf e -> print_E_ContractOf state e
| E_CtorApp    e -> print_E_CtorApp    state e
| E_Div        e -> print_E_Div        state e
| E_DivEq      e -> print_E_DivEq      state e
| E_Do         e -> print_E_Do         state e
| E_Equal      e -> print_E_Equal      state e
| E_False      e -> print_E_False            e
| E_Function   e -> print_E_Function   state e
| E_Geq        e -> print_E_Geq        state e
| E_Gt         e -> print_E_Gt         state e
| E_Int        e -> print_E_Int              e
| E_Leq        e -> print_E_Leq        state e
| E_Lt         e -> print_E_Lt         state e
| E_Match      e -> print_E_Match      state e
| E_Mult       e -> print_E_Mult       state e
| E_MultEq     e -> print_E_MultEq     state e
| E_Mutez      e -> print_E_Mutez            e
| E_NamePath   e -> print_E_NamePath   state e
| E_Nat        e -> print_E_Nat              e
| E_Neg        e -> print_E_Neg        state e
| E_Neq        e -> print_E_Neq        state e
| E_Not        e -> print_E_Not        state e
| E_Object     e -> print_E_Object     state e
| E_Or         e -> print_E_Or         state e
| E_Par        e -> print_E_Par        state e
| E_PostDecr   e -> print_E_PostDecr   state e
| E_PostIncr   e -> print_E_PostIncr   state e
| E_PreDecr    e -> print_E_PreDecr    state e
| E_PreIncr    e -> print_E_PreIncr    state e
| E_Proj       e -> print_E_Proj       state e
| E_Rem        e -> print_E_Rem        state e
| E_RemEq      e -> print_E_RemEq      state e
| E_String     e -> print_E_String           e
| E_Sub        e -> print_E_Sub        state e
| E_SubEq      e -> print_E_SubEq      state e
| E_Ternary    e -> print_E_Ternary    state e
| E_True       e -> print_E_True             e
| E_Typed      e -> print_E_Typed      state e
| E_Update     e -> print_E_Update     state e
| E_Var        e -> print_E_Var              e
| E_Verbatim   e -> print_E_Verbatim         e
| E_Xor        e -> print_E_Xor        state e

(* Addition *)

and print_E_Add state (node : plus bin_op reg) = print_bin_op state node

and print_bin_op state (node : lexeme wrap bin_op reg) =
  let {op; arg1; arg2} = node.value in
  let length = String.length op#payload + 1
  in group (print_expr state arg1 ^/^ token op ^^ space
            ^^ nest length (print_expr state arg2))

(* Add and assign *)

and print_E_AddEq state (node : plus_eq bin_op reg) =
  print_bin_op state node

(* Logical conjunction *)

and print_E_And state (node : bool_and bin_op reg) =
  print_bin_op state node

(* Function application *)

and print_E_App state (node : (expr * arguments) reg) = failwith "TODO"

(* Array expression *)

and print_E_Array state (node : expr _array) = failwith "TODO"

(* Arrow function *)

and print_E_ArrowFun state (node : arrow_fun_expr reg) = failwith "TODO"

(* Assignment *)

and print_E_Assign state (node : equal bin_op reg) =
  print_bin_op state node

(* Attributes expressions *)

and print_E_Attr state (node : attribute * expr) =
  let attributes, expr = unroll_E_Attr node in
  let thread = print_expr state expr
  in print_attributes state thread attributes

(* Bitwise conjunction *)

and print_E_BitAnd state (node : bit_and bin_op reg) =
  print_bin_op state node

(* Bitwise conjuction & Assignment *)

and print_E_BitAndEq state (node : bit_and_eq bin_op reg) =
  print_bin_op state node

(* Bitwise negation *)

and print_E_BitNeg state (node : bit_neg un_op reg) =
  print_un_op state node

(* Bitwise disjunction *)

and print_E_BitOr state (node : bit_or bin_op reg) =
  print_bin_op state node

(* Bitwise disjunction & Assignment *)

and print_E_BitOrEq state (node : bit_or_eq bin_op reg) =
  print_bin_op state node

(* Bitwise left shift *)

and print_E_BitSl state (node : bit_sl bin_op reg) =
  print_bin_op state node

(* Bitwise left shift & Assignment *)

and print_E_BitSlEq state (node : bit_sl_eq bin_op reg) =
  print_bin_op state node

(* Bitwise right shift *)

and print_E_BitSr state (node : bit_sr bin_op reg) =
  print_bin_op state node

(* Bitwise right shift & Assignmen *)

and print_E_BitSrEq state (node : bit_sr_eq bin_op reg) =
  print_bin_op state node

(* Bitwise exclusive disjunction *)

and print_E_BitXor state (node : bit_xor bin_op reg) =
  print_bin_op state node

(* Bitwise exclusive disjunction & Assignment *)

and print_E_BitXorEq state (node : bit_xor_eq bin_op reg) =
  print_bin_op state node

(* Bytes expressions *)

and print_E_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Code injection *)

and print_E_CodeInj state (node : code_inj reg) =
  let {language; code} = node.value in
  let language = token language in
  let code     = print_expr state code in
  group (language ^/^ code)

(* Contract-of expression *)

and print_E_ContractOf state (node : contract_of_expr reg) = failwith "TODO"

(* Constructor application *)

and print_E_CtorApp state (node : expr ctor_app reg) = failwith "TODO"

(* Euclidean division *)

and print_E_Div state (node : slash bin_op reg) = print_bin_op state node

(* Euclidean division & Assignment *)

and print_E_DivEq state (node : div_eq bin_op reg) = print_bin_op state node

(* Do-expression *)

and print_E_Do state (node : do_expr reg) = failwith "TODO"

(* Equality *)

and print_E_Equal state (node : equal bin_op reg) = print_bin_op state node

(* Logical falsity *)

and print_E_False (node : false_const) = failwith "TODO"

(* Function expression *)

and print_E_Function state (node : function_expr reg) = failwith "TODO"

(* Greater of equal *)

and print_E_Geq state (node : geq bin_op reg) = print_bin_op state node

(* Greater than *)

and print_E_Gt state (node : gt bin_op reg) = print_bin_op state node

(* Integers *)

and print_E_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Lower or equal than *)

and print_E_Leq state (node : leq bin_op reg) = print_bin_op state node

(* Lower than *)

and print_E_Lt state (node : lt bin_op reg) = print_bin_op state node

(* Pattern matching *)

and print_E_Match state (node : match_expr reg) = failwith "TODO"

(* Multiplication *)

and print_E_Mult state (node : times bin_op reg) =
  print_bin_op state node

(* Multiplication && Assignment *)

and print_E_MultEq state (node : times_eq bin_op reg) =
  print_bin_op state node

(* Mutez as an expression *)

and print_E_Mutez (node : (lexeme * Int64.t) wrap) =
  print_mutez node

(* Selection through nested namespaces *)

and print_E_NamePath state (node : expr namespace_path reg) =
  failwith "TODO"

(* Natural numbers in expressions *)

and print_E_Nat (node :  (lexeme * Z.t) wrap) = print_nat node

(* Arithmetic negation *)

and print_E_Neg state (node : minus un_op reg) =
  let {op; arg} = node.value in
  token op ^^ print_expr state arg

and print_un_op state (node : lexeme wrap un_op reg) =
  let {op; arg} = node.value in
  token op ^^ space ^^ print_expr state arg

(* Arithmetic difference *)

and print_E_Neq state (node : neq bin_op reg) =
  print_bin_op state node

(* Logical negation *)

and print_E_Not state (node : bool_neg un_op reg) =
  print_un_op state node

(* Objects *)

and print_E_Object state (node : expr _object) = failwith "TODO"

(* Logical disjunction *)

and print_E_Or state (node : bool_or bin_op reg) =
  print_bin_op state node

(* Parenthesised expression *)

and print_E_Par state (node : expr par) =
  print_par state (print_expr state) node

(* Post-decrementation *)

and print_E_PostDecr state (node : decrement un_op reg) =
  print_un_op state node

(* Post-incrementation *)

and print_E_PostIncr state (node : increment un_op reg) =
  print_un_op state node

(* Pre-decrementation *)

and print_E_PreDecr state (node : decrement un_op reg) =
  print_un_op state node

(* Pre-incrementation *)

and print_E_PreIncr state (node : increment un_op reg) =
  print_un_op state node

(* Projections *)

and print_E_Proj state (node : projection reg) = failwith "TODO"

(* Arithmetic remainder *)

and print_E_Rem state (node : remainder bin_op reg) =
  print_bin_op state node

(* Arithmetic remainder & Assignment *)

and print_E_RemEq state (node : rem_eq bin_op reg) =
  print_bin_op state node

(* String expression *)

and print_E_String (node : lexeme wrap) = print_string node

(* Arithmetic subtraction *)

and print_E_Sub state (node : minus bin_op reg) = print_bin_op state node

(* Subtraction & Assignment *)

and print_E_SubEq state (node : minus_eq bin_op reg) =
  print_bin_op state node

(* Ternary conditional *)

and print_E_Ternary state (node : ternary reg) = failwith "TODO"

(* Logical truth *)

and print_E_True (node : true_const) = failwith "TODO"

(* Typed expressions *)

and print_E_Typed state (node : typed_expr reg) = failwith "TODO"

(* Object functional updates *)

and print_E_Update state (node : update_expr braces) = failwith "TODO"

(* Expression variable *)

and print_E_Var (node : variable) = print_ident node

(* Verbatim string expressions *)

and print_E_Verbatim (node : lexeme wrap) = print_verbatim node

(* Logical exclusive disjunction *)

and print_E_Xor state (node : bool_xor bin_op reg) =
  print_bin_op state node

(* PATTERNS *)

and print_pattern state = function
  P_Array    p -> print_P_Array    state p
| P_Attr     p -> print_P_Attr     state p
| P_Bytes    p -> print_P_Bytes          p
| P_CtorApp  p -> print_P_CtorApp  state p
| P_False    p -> print_P_False          p
| P_Int      p -> print_P_Int            p
| P_Mutez    p -> print_P_Mutez          p
| P_NamePath p -> print_P_NamePath state p
| P_Nat      p -> print_P_Nat            p
| P_Object   p -> print_P_Object   state p
| P_String   p -> print_P_String         p
| P_True     p -> print_P_True           p
| P_Typed    p -> print_P_Typed    state p
| P_Var      p -> print_P_Var            p
| P_Verbatim p -> print_P_Verbatim       p

(* Array patterns *)

and print_P_Array state (node : pattern _array) = failwith "TODO"

(* Attributed pattern *)

and print_P_Attr state (node : attribute * pattern) =
  let attributes, pattern = unroll_P_Attr node in
  let thread = print_pattern state pattern
  in print_attributes state thread attributes

(* Bytes pattern *)

and print_P_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Pattern for constructor application *)

and print_P_CtorApp state (node : pattern ctor_app reg) = failwith "TODO"

(* Logical false pattern *)

and print_P_False (node : false_const) = failwith "TODO"

(* Integer in a pattern *)

and print_P_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Mutez in patterns *)

and print_P_Mutez (node : (lexeme * Int64.t) wrap) = print_mutez node

(* Selected pattern *)

and print_P_NamePath state (node : pattern namespace_path reg) =
  failwith "TODO"

(* Natural numbers in patterns *)

and print_P_Nat (node : (lexeme * Z.t) wrap) = print_nat node

(* Object patterns *)

and print_P_Object state (node : pattern _object) = failwith "TODO"

(* String patterns *)

and print_P_String (node : lexeme wrap) = print_string node

(* Logical truth pattern *)

and print_P_True (node : true_const) = failwith "TODO"

(* Typed patterns *)

and print_P_Typed state (node : typed_pattern reg) =
  let pattern, type_annot = node.value in
  print_pattern state pattern ^^ space ^^
  print_type_annotation state type_annot

(* Variable pattern *)

and print_P_Var (node : variable) = print_ident node

(* Verbatim string patterns *)

and print_P_Verbatim (node : lexeme wrap) = print_verbatim node

(* TYPE EXPRESSIONS *)

and print_type_expr state = function
  T_App         t -> print_T_App         state t
| T_Attr        t -> print_T_Attr        state t
| T_Array       t -> print_T_Array       state t
| T_Fun         t -> print_T_Fun         state t
| T_Int         t -> print_T_Int               t
| T_NamePath    t -> print_T_NamePath    state t
| T_Object      t -> print_T_Object      state t
| T_Par         t -> print_T_Par         state t
| T_ParameterOf t -> print_T_ParameterOf state t
| T_String      t -> print_T_String            t
| T_Union       t -> print_T_Union       state t
| T_Var         t -> print_T_Var               t
| T_Variant     t -> print_T_Variant     state t

(* Type constructor application *)

and print_T_App state (node : (type_expr * type_ctor_args) reg) =
  failwith "TODO"

(* Attributed type *)

and print_T_Attr state (node : attribute * type_expr) =
  let attributes, t_expr = unroll_T_Attr node in
  let thread = print_type_expr state t_expr
  in print_attributes state thread attributes

(* Array type *)

and print_T_Array state (node : array_type) = failwith "TODO"

(* Function type *)

and print_T_Fun state (node : fun_type) = failwith "TODO"

(* Integer type *)

and print_T_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Selection of a type *)

and print_T_NamePath state (node : type_expr namespace_path reg) =
  failwith "TODO"

(* Object type *)

and print_T_Object state (node : type_expr _object) = failwith "TODO"

(* Parenthesised type expressions *)

and print_T_Par state (node : type_expr par) =
  print_par state (print_type_expr state) node

(* Parameter-of type *)

and print_T_ParameterOf state (node : parameter_of_type reg) =
  failwith "TODO"

(* String type *)

and print_T_String (node : lexeme wrap) = print_string node

(* Union type *)

and print_T_Union state (node : union_type) = failwith "TODO"

(* Type variables *)

and print_T_Var (node : variable) = print_ident node

(* Variant type *)

and print_T_Variant state (node : variant_type) = failwith "TODO"


(* XXX *)
(*

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
                          *)

let print_type_expr = print_type_expr
let print_pattern   = print_pattern
let print_expr      = print_expr
let print_statement = print_statement

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern
type statement = CST.statement

(*
let print           _state _cst = PPrint.empty
let print_expr      _state _cst = PPrint.empty
let print_type_expr _state _cst = PPrint.empty
let print_pattern   _state _cst = PPrint.empty
let print_statement _state _cst = PPrint.empty
*)
