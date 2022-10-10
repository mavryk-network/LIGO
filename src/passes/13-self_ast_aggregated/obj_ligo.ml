module Trace = Simple_utils.Trace
open Simple_utils.Trace
open Ligo_prim
module Folder = Helpers
open Ast_aggregated

type 'err ty_exp_mapper = type_expression -> unit

let rows : ('a -> unit) -> _ Rows.t -> unit =
 fun g { fields; _ } ->
  Map.iter fields ~f:(fun row_elem -> g row_elem.content.associated_type)


let rec traverse_type_expression : 'err ty_exp_mapper -> type_expression -> unit
  =
 fun f te ->
  let open Ligo_prim in
  let self = traverse_type_expression f in
  let () = f te in
  match te.type_content with
  | T_sum temap -> rows self temap
  | T_for_all x -> self x.type_
  | T_record temap -> rows self temap
  | T_arrow arr ->
    let _ = Arrow.map self arr in
    ()
  | T_tuple tuple ->
    let _ = Tuple.map self tuple in
    ()
  | T_variable _ -> ()
  | T_singleton _ -> ()
  | T_constant { parameters } ->
    let _ = List.map ~f:self parameters in
    ()


(* Adapted from lib_protocol/script_string_repr.ml *)
let check_string v =
  let rec check_printable_ascii i =
    if Int.(i < 0)
    then true
    else (
      match v.[i] with
      | '\n' | '\x20' .. '\x7E' -> check_printable_ascii (i - 1)
      | _ -> false)
  in
  check_printable_ascii (String.length v - 1)


(*
  check_obj_ligo [blacklist] [t] fails if t hold meta-ligo types or meta-ligo constants sub-terms
  [blacklist] is a list of binder and location which refer to meta-ligo terms, when
  encountering a variable matching an element of this list, it fails
*)
let check_obj_ligo ~raise ?(blacklist = []) (t : expression) : unit =
  let folder_constant () expr =
    match expr.expression_content with
    | E_variable v ->
      let b_opt =
        List.find
          ~f:(fun (x, _loc) -> Value_var.equal v (Binder.get_var x))
          blacklist
      in
      (match b_opt with
      | Some (_, loc) -> raise.error @@ Errors.expected_obj_ligo loc
      | None -> ())
    | E_constant { cons_name } when Constant.ppx_is_only_interpreter cons_name
      -> raise.error @@ Errors.expected_obj_ligo expr.location
    | E_literal (Literal_string s)
      when not (check_string @@ Ligo_string.extract s) ->
      raise.error @@ Errors.expected_obj_ligo expr.location
    | _ -> ()
  in
  let traverser_types loc expr =
    match expr.type_content with
    | T_constant { injection = Literal_types.Michelson_program; _ }
    | T_constant { injection = Literal_types.Typed_address; _ }
    | T_constant { injection = Literal_types.Mutation; _ } ->
      raise.error @@ Errors.expected_obj_ligo loc
    | _ -> ()
  in
  let folder_types () (expr : expression) =
    traverse_type_expression
      (traverser_types expr.location)
      expr.type_expression
  in
  let () = Folder.fold_expression folder_constant () t in
  let () = Folder.fold_expression folder_types () t in
  ()


(*
  check_obj_ligo [blacklist] [t] fails if t hold meta-ligo types or meta-ligo constants sub-terms
  [blacklist] is a list of binder and location which refer to meta-ligo terms, when
  encountering a variable matching an element of this list, it fails
*)
let check_obj_ligo_program ~raise ?(blacklist = []) ((ctxt, e) : program) : unit
  =
  let f decl () =
    match Location.unwrap decl with
    | D_value { binder = _; expr; attr = _ } ->
      check_obj_ligo ~raise ~blacklist expr
  in
  let () = List.fold_right ctxt ~f ~init:() in
  check_obj_ligo ~raise ~blacklist e


(*
    [purge_meta_ligo_program] [t] remove any "top-level" let-in bindings holding meta-ligo terms in [t] context
    it checks right-hand sides of context bindings (`[let <rhs_0> = ... ; let <rhs_N> = ... ]`) for meta-ligo
    constructs (primitives or types). If present, the corresponding let-in binding is purged from [t] context
    while keeping a list of all "meta-binders" along with their location (blacklist)

    e.g.

    ```
    (* blacklist = [] *)
    let x = Test.log "hello" in
    (* blacklist = [ (x,LOCATION(Test.log "hello")) ] *)
    let y = 1 in
    y + 1
    ```
    |->

    ```
    let y = 1 in
    y + 1
    ```

    when encountering the <rest>, [purge_meta_ligo] will fail on any meta-ligo constructors
    
    e.g.

    ```
    let x = Test.log "hello" in
    let y = 1 in
    x
    ```
    |-> FAIL

    of

    ```
    let y = "hello" in
    (fun _ -> 2) (Test.log y)
    ```
    | -> FAIL
    
*)
let purge_meta_ligo_program ~raise ((ctxt, e) : program) : program =
  let f (blacklist, ctxt) decl =
    match Location.unwrap decl with
    | D_value { binder; expr; attr = _ } ->
      let expr_is_meta = not (Trace.to_bool (check_obj_ligo ~blacklist expr)) in
      let blacklist =
        if expr_is_meta then (binder, expr.location) :: blacklist else blacklist
      in
      if expr_is_meta then blacklist, ctxt else blacklist, decl :: ctxt
  in
  let blacklist, ctxt = List.fold_left ctxt ~init:([], []) ~f in
  let ctxt = List.rev ctxt in
  let () = check_obj_ligo ~raise ~blacklist e in
  ctxt, e
