(* This module implements a filter on the lexical units of JsLIGO
   and produces tokens to be consumed by the parser. *)

[@@@warning "-42"]

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Utils     = Simple_utils.Utils
module Core      = LexerLib.Core
module Markup    = LexerLib.Markup
module Directive = LexerLib.Directive

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token list * Markup.t list, message) result
  end

(* Utilities *)

let (<@) = Utils.(<@)

let (let*) = Result.bind
let ok     = Result.ok

type message = string Region.reg

type token = Token.t

type lex_unit = token Core.lex_unit

(* Filtering out the markup *)

let tokens_of lex_units =
  let apply (tokens, comments) = function
    Core.Token token -> (token::tokens, comments)
  | Core.Markup (Markup.BlockCom bc as c) -> (Token.BlockCom bc :: tokens, c :: comments)
  | Core.Markup (Markup.LineCom lc as c) -> (Token.BlockCom lc :: tokens, c :: comments)
  | Core.Markup _ -> (tokens, comments)
  | Core.Directive d -> (Token.Directive d :: tokens, comments)
  in 
  List.fold_left apply ([], []) lex_units |> (fun (a, b) -> (List.rev a, List.rev b)) |> ok


(* Automatic Semicolon Insertion *)

let automatic_semicolon_insertion tokens =
  let open! Token in
  let rec inner result = function
    (Directive _ as t) :: rest ->
    inner (t :: result) rest
  | (LineCom _ as t) :: rest ->
    inner (t :: result) rest
  | (BlockCom _ as t) :: rest ->
    inner (t :: result) rest
  | (_ as semi) :: (LineCom _ as t) :: rest
  | (_ as semi) :: (BlockCom _ as t) :: rest
  | (SEMI _ as semi) :: (Let _ as t)  :: rest
  | (SEMI _ as semi) :: (Const _ as t)  :: rest
  | (SEMI _ as semi) :: (Type _ as t)  :: rest
  | (SEMI _ as semi) :: (Return _ as t)  :: rest
  | (LBRACE _ as semi) :: (Let _ as t)  :: rest
  | (LBRACE _ as semi) :: (Const _ as t)  :: rest
  | (LBRACE _ as semi) :: (Type _ as t)  :: rest
  | (LBRACE _ as semi) :: (Return _ as t)  :: rest ->
    inner (t:: semi :: result) rest
  | token :: (Const _ as t) :: rest
  | token :: (Type _ as t) :: rest
  | token :: (Return _ as t) :: rest
  | token :: (Let _ as t) :: rest ->
    let (r, _) = Token.proj_token token in
    let (r2, _) = Token.proj_token t in
    if r#stop#line < r2#start#line  then (
      inner (t :: SEMI (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop) :: token :: result) rest 
    )
    else (
      match token with 
        RBRACE _ as t -> 
        inner (t :: SEMI (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop) :: token :: result) rest 
      | _ ->
        inner (t :: token :: result) rest
    )
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in
  inner [] tokens

(* Attributes *)

let attribute_regexp = Str.regexp "@\\([a-zA-Z:0-9_]+\\)"

let collect_attributes str =
  let rec inner result str =
    try (
      let r = Str.search_forward attribute_regexp str 0 in
      let s = Str.matched_group 0 str in
      let s = String.sub s 1 (String.length s - 1) in
      let next = (String.sub str (r + String.length s) (String.length str - (r + + String.length s))) in
      inner (s :: result) next
    )
    with
    | Not_found -> result
  in
  inner [] str

let attributes tokens =
  let open! Token in
  let rec inner result = function
    LineCom c :: tl
  | BlockCom c :: tl ->
      let attributes = collect_attributes c.Region.value in
      let attributes = List.map (fun e ->
        Attr Region.{value = e; region = c.region}) attributes in
      inner (attributes @ result) tl
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in
  inner [] tokens

(* Injection of Zero-Width Spaces *)

let inject_zwsp lex_units =
  let open! Token in
  let rec aux acc = function
    [] -> List.rev acc
  | (Core.Token GT _ as gt1)::(Core.Token GT reg :: _ as units) ->
      aux (Core.Token (ZWSP reg) :: gt1 :: acc) units
  | unit::units -> aux (unit::acc) units
  in 
  ok @@ aux [] lex_units

(* DEBUG *)

(* Printing lexical units *)

let print_unit = function
  Core.Token t ->
    Printf.printf "%s\n" (Token.to_string ~offsets:true `Point t)
| Core.Markup m ->
    Printf.printf "%s\n" (Markup.to_string ~offsets:true `Point m)
| Core.Directive d ->
    Printf.printf "%s\n" (Directive.to_string ~offsets:true `Point d)

(* Printing tokens *)

let print_token token =
  Printf.printf "%s\n" (Token.to_string ~offsets:true `Point token)

(* insert vertical bar for sum type *)

let vertical_bar_insert tokens =
  let open! Token in
  let rec aux acc insert_token = function
    (VBAR _ as hd) :: tl ->
    aux (hd::acc) false tl
  | (EQ _ as hd) :: tl ->
    if insert_token then (
      List.rev_append (hd :: VBAR Region.ghost :: acc) tl
    )
    else (
      List.rev_append (hd :: acc) tl
    )
  | (RBRACKET _ as hd) :: tl -> 
    aux (hd::acc) true tl
  | hd :: tl ->
    aux (hd::acc) insert_token tl
  | [] ->
    List.rev acc
  in
  aux [] false tokens

let vertical_bar_insert tokens =
  let open! Token in
  let rec aux acc = function 
    (VBAR _ as hd) :: tl ->
      aux (vertical_bar_insert (hd::acc)) tl
  | hd :: tl -> aux (hd::acc) tl
  | [] -> List.rev acc
  in aux [] tokens

(* COMPOSING FILTERS (exported) *)

let filter tokens =
  let* tokens = Style.check tokens in
  let* tokens = inject_zwsp tokens in
  let* (tokens, comments) = tokens_of tokens in
  let tokens = 
    (attributes
    <@ automatic_semicolon_insertion
    <@ vertical_bar_insert)
    tokens
  in
  ok (tokens, comments)
