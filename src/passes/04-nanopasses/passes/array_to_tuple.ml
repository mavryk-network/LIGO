open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let compile ~raise =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_let_in { lhs = ({ fp = { wrap_content = P_tuple _ ; _ } }, _) as lhs; rhs; body; is_rec; type_params; rhs_type = None } ->
      let rhs = (match Location.unwrap rhs.fp with
          | E_array [] -> e_unit ~loc
          | E_array (hd :: tl) ->
            let f = function
              | Array_repr.Expr_entry e -> e
              | Rest_entry e -> raise.error @@ unsupported_rest_property e
            in
            e_tuple ~loc (List.Ne.map f (hd, tl))
          | _ -> rhs) in
      make_e ~loc @@ E_let_in { lhs; rhs; body; is_rec; type_params; rhs_type = None }
    | e -> make_e ~loc e
  in
  let declaration : (declaration, expr, ty_expr, pattern, mod_expr, sig_expr) declaration_ -> declaration =
    fun d ->
    let loc = Location.get_location d in
    match Location.unwrap d with
    | D_multi_const ds ->
      let f (d : (pattern, expr, ty_expr) Simple_decl.t) : (pattern, expr, ty_expr) Simple_decl.t =
        match d with
        | { pattern = { fp = { wrap_content = P_tuple _ ; _ } } as pattern; type_params; rhs_type = None; let_rhs } ->
          let let_rhs = (match Location.unwrap let_rhs.fp with
              | E_array [] -> e_unit ~loc
              | E_array (hd :: tl) ->
                let f = function
                  | Array_repr.Expr_entry e -> e
                  | Rest_entry e -> raise.error @@ unsupported_rest_property e
                in
                e_tuple ~loc (List.Ne.map f (hd, tl))
              | _ -> let_rhs) in
          { pattern; type_params; rhs_type = None; let_rhs }
        | _ -> d
      in
      make_d ~loc @@ D_multi_const (List.Ne.map f ds)
    | d -> make_d ~loc d
  in
  Fold { idle_fold with expr ; declaration }


let reduction ~raise:_ = Iter.defaults
  (* { Iter.defaults with *)
  (*   expr = *)
  (*     (function *)
  (*     | { wrap_content = E_array _; _ } -> raise.error (wrong_reduction __MODULE__) *)
  (*     | _ -> ()) *)
  (* } *)


let name = __MODULE__
let decompile ~raise:_ = Nothing (* TODO *)
