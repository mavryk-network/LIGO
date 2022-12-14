open Ast_unified
open Pass_type

open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* Pattern matching for JsLIGO is implemented as a 'built-in function' as
    JavaScript and TypeScript don't have native pattern matching. *)

let block_to_expr : (expr, statement) Block_fun.fun_block -> expr = function
  | ExpressionBody body -> body
  | FunctionBody stmts ->
    let last_s, stmts = Simple_utils.List.Ne.rev stmts in
    let last =
      let loc = get_s_loc last_s in
      match get_s last_s with
      | S_Instr x ->
        (match get_i x with
        | I_Return x -> Option.value_map ~default:(e_unit ~loc) ~f:Fun.id x
        (* see https://tezos-dev.slack.com/archives/GMHV0U3Q9/p1670852612146059 *)
        | I_Expr x -> x
        | _ -> e_unit ~loc)
      | _ -> e_unit ~loc
    in
    let body =
      let loc =
        List.fold
          ~init:(get_s_loc last_s)
          ~f:(fun acc x -> Location.cover acc (get_s_loc x))
          (last_s :: stmts)
      in
      match List.rev stmts with
      | [] -> last
      | hd :: tl -> e_block_with ~loc { block = hd, tl; expr = last }
    in
    body


let object_to_matching_clause ~raise : expr Object_.property -> (pattern, expr) Case.clause
  = function
  | Property (name, value) ->
    (match get_e name, get_e value with
    | E_variable ctor, E_Block_fun { parameters; lhs_type; body } ->
      ignore lhs_type;
      (* TODO: we ignore the types here.. emit a warning ?*)
      let pattern =
        let loc = Variable.get_location ctor in
        let params =
          match parameters with
          | [] -> None
          | _ -> Some (p_tuple ~loc parameters)
        in
        p_variant
          ~loc:(Variable.get_location ctor)
          (Label.of_string (Variable.to_name_exn ctor))
          params
      in
      let rhs = block_to_expr body in
      { pattern; rhs }
    | _,_ -> raise.error (invalid_case name))
  | Punned_property e | Property_rest e ->
    raise.error (unsupported_match_object_property e)


let list_to_matching_clause ~raise : expr -> (pattern, expr) Case.clause =
 fun e ->
  match get_e e with
  | E_Block_fun { parameters = [ pattern ]; lhs_type; body } ->
    ignore lhs_type;
    let rhs = block_to_expr body in
    { pattern; rhs }
  | _ -> raise.error (invalid_list_pattern_match (get_e_loc e))


let compile ~raise ~syntax =
  ignore raise;
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_Call (f, [ matchee; cases ]) ->
      (match get_e f, get_e cases with
      | E_variable v, E_Object args when Variable.is_name v "match" ->
        let cases = Simple_utils.List.Ne.map (object_to_matching_clause ~raise) args in
        e_match ~loc { expr = matchee; cases }
      | E_variable v, E_List args when Variable.is_name v "match" ->
        let cases = List.map ~f:(list_to_matching_clause ~raise) args in
        (match cases with
        | [] -> raise.error (invalid_list_pattern_match loc)
        | hd :: tl -> e_match ~loc { expr = matchee; cases = hd, tl })
      | _ -> same)
    | _ -> same
  in
  if Syntax_types.equal syntax JsLIGO
  then `Cata { idle_cata_pass with expr = pass_expr }
  else `Cata idle_cata_pass

let reduction ~raise ~syntax =
    let fail () = raise.error (wrong_reduction __MODULE__) in
    if Syntax_types.equal syntax JsLIGO
    then
      { Iter.defaults with
        expr =
          (function
          | { wrap_content = E_Call (f, _); _ }
            when Option.value_map ~default:false (get_e_variable f) ~f:(fun x ->
                     Variable.is_name x "match") -> fail ()
          | _ -> ())
      }
    else Iter.defaults
  
let decompile ~syntax = ignore syntax ; `Cata idle_cata_pass (* TODO *)

let pass ~raise ~syntax =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~syntax)
    ~decompile:(decompile ~syntax)
    ~reduction_check:(reduction ~raise ~syntax)