[@@@coverage exclude_file]
module Location    = Simple_utils.Location
module Var         = Simple_utils.Var
module List        = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Int64       = Caml.Int64
open Types
open Format
open Simple_utils.PP_helpers
include Stage_common.PP

type 'a pretty_printer = Format.formatter -> 'a -> unit

let lmap_sep value sep ppf m =
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) m in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, v) = fprintf ppf "%a" value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep_t value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, v) = fprintf ppf "%a" value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

(* Prints records which only contain the consecutive fields
   0..(cardinal-1) as tuples *)
let tuple_or_record_sep value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m
let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep_t value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep_t value (tag sep_record)) m
let list_sep_d_short x = list_sep x (tag " , ")
let list_sep_d x = list_sep x (tag " ,@ ")
let kv_short value_pp ~assoc ppf (k, v) = fprintf ppf "%a%s%a" label k assoc value_pp v
let lmap_sep_short x ~sep ~assoc ppf m =
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) m in
  list_sep (kv_short x ~assoc) (tag sep) ppf lst
let lmap_sep_d x = lmap_sep x (tag " ,@ ")
let tuple_or_record_sep_expr value = tuple_or_record_sep value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " ,@ "
let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " *@ "

let type_variable ppf (t : type_variable) : unit = fprintf ppf "%a" Var.pp t
let module_variable ppf (m : module_variable) : unit = pp_print_string ppf m

open Format

let rec constraint_identifier_unicode (ci : Int64.t) =
  let digit =
    let ( - ) = Int64.sub in
    let ( / ) = Int64.div in
    let ( * ) = Int64.mul in
    match (ci - ((ci / 10L) * 10L)) with
      a when Int64.equal a 0L -> "₀"
    | a when Int64.equal a 1L -> "₁"
    | a when Int64.equal a 2L -> "₂"
    | a when Int64.equal a 3L -> "₃"
    | a when Int64.equal a 4L -> "₄"
    | a when Int64.equal a 5L -> "₅"
    | a when Int64.equal a 6L -> "₆"
    | a when Int64.equal a 7L -> "₇"
    | a when Int64.equal a 8L -> "₈"
    | a when Int64.equal a 9L -> "₉"
    | _ -> failwith (Format.asprintf "internal error: couldn't pretty-print int64: %Li (is it a negative number?)" ci)
  in
  if Int64.equal ci 0L then "" else (constraint_identifier_unicode (Int64.div ci 10L)) ^ digit

let constraint_identifier_short ppf x =
  if Int64.equal x 0L
  then Format.fprintf ppf "₀"
  else Format.fprintf ppf "%s" (constraint_identifier_unicode x)

let list_sep_d_par f ppf lst =
  match lst with
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_content -> unit =
  fun ppf tc ->
  match tc with
  | T_variable        tv -> type_variable                 ppf tv
  | T_constant        tc -> type_injection ppf tc
  | T_sum              m -> fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row) (LMap.to_kv_list_rev m.content)
  | T_record           m -> fprintf ppf "%a" (tuple_or_record_sep_type row) m.content
  | T_arrow            a -> arrow         type_expression ppf a
  | T_singleton       x  -> literal       ppf             x
  | T_abstraction     x  -> abstraction   type_expression ppf x
  | T_for_all         x  -> for_all       type_expression ppf x

and row : formatter -> row_element -> unit =
  fun ppf { associated_type ; michelson_annotation=_ ; decl_pos=_ } ->
    fprintf ppf "%a"
      type_expression associated_type

and type_injection ppf {language;injection;parameters} =
  (* fprintf ppf "[%s {| %s %a |}]" language (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters *)
  ignore language;
  fprintf ppf "%s%a" (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters


and type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool te) then
    fprintf ppf "%a" type_variable Stage_common.Constant.v_bool
  else
    fprintf ppf "%a" type_content te.type_content

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev.wrap_content


let rec expression ppf (e : expression) =
  fprintf ppf "%a"
    expression_content e.expression_content

and expression_content ppf (ec: expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "(%a)@(%a)" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "%a(%a)" label c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "%a(%a)" constant' c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "%a" (tuple_or_record_sep_expr expression) m
  | E_record_accessor ra ->
      fprintf ppf "%a.%a" expression ra.record label ra.path
  | E_record_update {record; path; update} ->
      fprintf ppf "{ %a with { %a = %a } }" expression record label path expression update
  | E_lambda {binder; result} ->
      fprintf ppf "lambda (%a) return %a" expression_variable binder
        expression result
  | E_matching {matchee; cases;} ->
      fprintf ppf "@[<v 2> match @[%a@] with@ %a@]" expression matchee (matching expression) cases
  | E_let_in {let_binder; rhs; let_result; attr = { inline; no_mutation; public=__LOC__ ; view = _} } ->
      fprintf ppf "@[<h>let %a = %a%a%a in@.%a@]" expression_variable let_binder expression
        rhs option_inline inline option_no_mutation no_mutation expression let_result
  | E_type_in   {type_binder; rhs; let_result} -> 
      fprintf ppf "@[let %a =@;<1 2>%a in@ %a@]"
        type_variable type_binder
        type_expression rhs
        expression let_result
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_type_inst {forall;type_} ->
      fprintf ppf "%a@@{%a}" expression forall type_expression type_
  | E_recursive { fun_name;fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )"
        expression_variable fun_name
        type_expression fun_type
        expression_content (E_lambda lambda)

and option_inline ppf inline =
  if inline then
    fprintf ppf "[@inline]"
  else
    fprintf ppf ""

and matching_variant_case : (_ -> expression -> unit) -> _ -> matching_content_case -> unit =
  fun f ppf {constructor=c; pattern; body} ->
  fprintf ppf "@[<v 2>| %a %a ->@ %a@]" label c expression_variable pattern f body

and matching : (formatter -> expression -> unit) -> _ -> matching_expr -> unit = fun f ppf m -> match m with
  | Match_variant {cases ; tv=_} ->
      fprintf ppf "@[%a@]" (list_sep (matching_variant_case f) (tag "@ ")) cases
  | Match_record {fields ; body ; tv = _} ->
      (* let with_annots f g ppf (a , b) = fprintf ppf "%a:%a" f a g b in *)
      let fields = LMap.map (fun (v,_) -> v) fields in
      fprintf ppf "| @[%a@] ->@ @[%a@]"
        (tuple_or_record_sep_expr expression_variable) fields
        f body
