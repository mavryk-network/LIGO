(* Self-passes on the tokens for JsLIGO *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Unit   = LexerLib.Unit

(* Definition of self-passes (a.k.a. filters) *)

type item = Token.t

type items = item list

type message = string Region.reg

type filter =
  ?print_passes:Std.t ->
  add_warning:(Main_warnings.all -> unit) ->
  items ->
  (items, items * message) result

type t = filter list

(* Listing all self-passes on tokens (resulting in [filters]) *)

let filters : t = [
  ES6FUN.filter;
  ES6FUN_hook.filter;
  VBAR.filter;
  VBAR_hook.filter;
  SEMI.filter;
  SEMI_hook.filter;
  Comments.filter;
  Attributes.filter
  (* Add more to this list. *)
]
