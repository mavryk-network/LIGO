open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* this should be called 'call_syntax' ? *)

let array_to_list ~loc (arguments : expr Array_repr.t) =
  match arguments with
  | [ Expr_entry hd; Rest_entry tl ] ->
    e_constant ~loc { cons_name = C_CONS; arguments = [ hd; tl ] }
  | _ ->
    let arguments =
      List.map arguments ~f:(function
          | Expr_entry x -> x
          | Rest_entry _ -> failwith "raise.error (array_rest_not_supported e))")
    in
    e_list ~loc arguments


let compile ~syntax =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_Call (f, [ args ]) ->
      (match get_e f, get_e args with
      | E_variable v, E_Array args when Variable.is_name v "list" ->
        array_to_list ~loc args
      
      | _ -> same)
    | _ -> same
  in
  if Option.equal Syntax_types.equal syntax (Some JsLIGO)
  then `Cata idle_cata_pass
  else `Cata { idle_cata_pass with expr = pass_expr }


let reduction ~raise =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_Call (f, _) ; _ }
        when Option.value_map ~default:false (get_e_variable f) ~f:(fun x ->
                 Variable.is_name x "list") -> fail ()
      | _ -> ())
  }

let decompile ~syntax =
  let pass_expr : _ expr_ -> expr =
    fun e ->
      let loc = Location.get_location e in
      let same = make_e ~loc e.wrap_content in
      match Location.unwrap e with
      | E_Constant {cons_name = C_CONS ; arguments = [hd;tl]} ->
        

let pass ~syntax = cata_morph ~name:__MODULE__ ~(compile:compile ~syntax) ~decompile ~reduction_check
