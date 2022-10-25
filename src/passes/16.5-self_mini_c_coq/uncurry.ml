module I = Ligo_coq_ocaml.Compiler
module Micheline = Tezos_micheline.Micheline
module Coq = Ligo_coq_ocaml

type meta = Mini_c.meta
type bynder_meta = Mini_c.binder_meta
type base_type = (meta, string) Micheline.node
type ty = (meta, base_type) I.ty
type expr = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.expr
type binds = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.binds
type args = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.args

let dummy_meta = Mini_c.dummy_meta

(* return list tuples (lambda's meta, binding's meta, variable type), body and return type *)
let rec uncurry_lambda (depth : int) (expr : expr) :
          (meta * meta * ty) list * expr * ty option =
  match expr with
  | I.E_lam (m1, I.Binds (m2, [ta], body), ret_ty) when depth > 0 ->
     let (vars, body, ret_ty') = uncurry_lambda (depth - 1) body in
     let ret_ty = Option.first_some ret_ty' (Some ret_ty) in
     ((m1, m2, ta) :: vars, body, ret_ty)
  | _ -> ([], expr, None)


(* let rec uncurry_arrow (depth : int) (type_ : ty) : ty list * ty = *)
(*   match type_ with *)
(*   | T_func (_, type1, type2) when depth > 0 -> *)
(*     let (rest, type2) = uncurry_arrow (depth - 1) type2 in *)
(*     (type1 :: rest, type2) *)
(*   | _ -> ([], type_) *)

(* TODO: ask about more than two arguments *)
let rec uncurry_app (expr : expr) : expr * expr list =
  match expr with
  | E_app (_, I.Args_cons (_, a, I.Args_cons (_, f, I.Args_nil _))) ->
    let (lamb, args') = uncurry_app f in
    (lamb, a :: args')
  | _ -> (expr, [])

let curried_depth_in_lambda (rhs : expr) : int =
  let (vars, _, _) = uncurry_lambda Int.max_value rhs in
  List.length vars

let isvar f x : bool =
  match x with
  | I.E_var (_, x) -> Coq.PeanoNat.Nat.eqb f x
  | _ -> false

(* Finding the usage of a function in an expression: we will look for
   functions which are _only_ used in applications with a certain
   fixed number of args. *)
type usage =
  | Application of int (* number of applied args *)
  | Other
  | Unused

let combine_usage (u1 : usage) (u2 : usage) : usage =
  match (u1, u2) with
  | (Application d1, Application d2) ->
    if d1 = d2
    then u1
    else Other
  | (Other, _) -> Other
  | (_, Other) -> Other
  | (Unused, u2) -> u2
  | (u1, Unused) -> u1

let usages = List.fold_left ~f:combine_usage ~init:Unused

let rec usage_in_expr (f : Coq.Datatypes.nat) (expr : expr) : usage =
  let self = usage_in_expr f in
  let self_binder = function
    | I.Binds (_, tys, body) ->
       usage_in_expr (Coq.Nat.add f (Coq.Datatypes.length tys)) body
  in
  let rec self_args = function
    | I.Args_cons (_, x, xs) -> combine_usage (self x) (self_args xs)
    | I.Args_nil (_) -> Unused
  in
  match expr with
  (* interesting cases: *)
  | I.E_var (_, x) ->
    if Coq.PeanoNat.Nat.eqb f x
    (* if we got here, f wasn't only used in applications *)
    then Other
    else Unused
  | I.E_app _ ->
    let (g, args) = uncurry_app expr in
    let g =
      if isvar f g
      (* found an application of f *)
      then Application (List.length args)
      (* else g might be something weird which contains a usage of f,
         e.g. if expr is ((if b then f else h) arg) *)
      else self g in
    usages (g :: List.map ~f:self args)

  | I.E_deref (_, x) ->
    if Coq.PeanoNat.Nat.eqb f x
    then Other
    else Unused
  | I.E_assign (_, x, e) ->
    if Coq.PeanoNat.Nat.eqb f x
    then Other
    else self e

  (* everything else is boilerplate... *)
  | I.E_literal _ | I.E_unit _ | I.E_raw_michelson _ ->
    Unused
  | I.E_lam (_, binder, _) ->
    self_binder binder
  | I.E_left (_, _t, e)  -> self e
  | I.E_right (_, _t, e) -> self e
  | I.E_failwith (_, e)  -> self e
  | I.E_pair (_, args)   -> self_args args
  | I.E_car (_, e)       -> self e
  | I.E_cdr (_, e)       -> self e

  | I.E_iter (_, v1, e1) ->
    usages [self_binder v1; self e1]
  | I.E_map (_, v1, e1) ->
    usages [self_binder v1; self e1]
  | I.E_loop_left (_, v1, _ty, e1) ->
    usages [self_binder v1; self e1]
  | I.E_fold (_, e1, e2, v1) ->
    usages [self_binder v1; self e1; self e2]
  | I.E_fold_right (_, _, e1, e2, v1) ->
    usages [self_binder v1; self e1; self e2]

  | I.E_if_bool (_, e1, e2, e3) ->
    usages [self e1; self e2; self e3]
  | I.E_if_none (_, e1, e2, v3) ->
    usages [self e1; self e2; self_binder v3]
  | I.E_if_cons (_, e1, vht, e2) ->
    usages [self e1; self e2; self_binder vht]
  | I.E_if_left (_, e1, v1, v2) ->
    usages [self e1; self_binder v1; self_binder v2]

  | I.E_let_in (_, e, binder) ->
    usages [self e; self_binder binder]
  | I.E_tuple (_, args) ->
    self_args args
  | I.E_let_tuple (_, e, vars) ->
    usages [self e; self_binder vars]
  | I.E_proj (_, e, _i, _n) ->
    self e
  | I.E_update (_, args, _i, _n) ->
    self_args args
  | I.E_inline_michelson (_, _, _, args) ->
    self_args args
  | I.E_global_constant (_, _ty, _hash, args) ->
    self_args args
  | I.E_create_contract (_, _p, _s, _code, args) -> (* TODO *)
    self_args args

  | I.E_let_mut_in (_, e, binder) ->
    usages [self e; self_binder binder]
  | I.E_for (_, e1, e2) ->
    usages [self_args e1; self_binder e2]
  | I.E_for_each (_, e1, e2) ->
    usages [self e1; self_binder e2]
  | I.E_while (_, e1, e2) ->
    usages [self e1; self e2]

let rec comb_type (ts : ty list) : ty =
  match ts with
  | x :: y :: [] -> I.T_pair (dummy_meta, None, None, x, y)
  | x :: xs -> I.T_pair (dummy_meta, None, None, x, comb_type xs)
  | [] -> I.T_unit (dummy_meta)

(* let comb_expr (es : expr list) : expr = *)
(*   I.E_tuple (dummy_meta, Helpers.args_of_list es) *)

let uncurry_rhs (depth : int) (expr : expr) : expr * ty =
  (*  λ x. λy. λ z. body => λ t. let (x, y) = t in λ z. body *)
  (* Format.printf "\nUNCURRY_RHS:\n%a" Helpers.pp_expr expr; *)
  let m = dummy_meta in
  let (vars, body, ret_ty) = uncurry_lambda depth expr in
  (* body save all binding and their order, but we need add one for new tuple
     argument right before them *)
  let body = Coq.Compiler.shift Helpers.nat_one (Coq.Datatypes.length vars) body in
  let _, _, var_tys = List.unzip3 vars in
  let arg_ty = comb_type var_tys in
  let ret_ty = match ret_ty with Some ty -> ty | None -> assert false in

  (* TODO: we can remove it by passing tuple in reverse order *)
  let _, vs = List.fold_left vars ~init:(Coq.Datatypes.O, [])
                  ~f:(fun (i, acc) (_, vm, vt) -> (S (S i),  (i, vm, vt) :: acc)) in
  let vs = List.rev vs in (* because fold_left reverse [vs] *)
  let body = Coq.Compiler.shift (Coq.Datatypes.length vars) (Coq.Datatypes.length vars) body in
  let body = List.fold_right vs ~init:body
                 ~f:(fun (vi, vm, vt) body ->
                     I.E_let_in (m, I.E_var (m, vi), I.Binds (vm, [vt], body)))
                 (* shift up all variables expect first [length vars] that is binded to arguments   *)
  in
  let binder_expr = I.E_var (m, Coq.Datatypes.O) in
  (* TODO: Investigate how order of [var_tys] and tuple arguments correspond *)
  let body = I.E_let_tuple (m, binder_expr, I.Binds (m, var_tys, body)) in
  (* Format.printf "type: %a %a\n" Helpers.pp_ty (arg_ty) Helpers.pp_ty ret_ty; *)
  let rsh = I.E_lam (Helpers.get_meta expr, I.Binds (m, [arg_ty], body), ret_ty) in
  (* Format.printf "\nOUTPUT:\n%a\n\n" Helpers.pp_expr rsh; *)
  let rsh_type = I.T_func (m, arg_ty, ret_ty) in
  (rsh, rsh_type)

(* hack to specialize map_expression to identity monad since there are
   no errors here *)
let map_expression = Helpers.map_expr
let mapi_expression = Helpers.mapi_expr

(* TODO: _depth *)
let uncurry_in_expression
    (f : Coq.Datatypes.nat) (_depth : int) : expr -> expr =
  mapi_expression ~init:f
    (fun f expr ->
       (* Format.printf "\nUNCURRY_BODY (f = %a):\n%a\n\n" *)
           (* Helpers.print_nat f Helpers.pp_expr expr; *)
       let dummy = dummy_meta in
       match expr with
       | I.E_app (m, _) ->
         let (lamb, args) = uncurry_app expr in
         if isvar f lamb
         then
           (* the interesting part... *)
           let arg_tuple = I.E_tuple (dummy, Helpers.args_of_list args) in
           I.E_app (m, Helpers.args_of_list [arg_tuple; lamb])
           (* let e' = I.E_app (m, Helpers.args_of_list [arg_tuple; lamb]) in *)
           (* Format.printf "\nCHANGE to %a\n" Helpers.pp_expr e'; e' *)
         else
           expr
       | _ -> expr)

let uncurry_expression : expr -> expr =
  map_expression
    (fun e ->
       match e with
       | I.E_let_in (m1, e1, I.Binds (m2, ts, e2)) ->
         let return e1 e1_ty e2 = I.E_let_in (m1, e1, I.Binds (m2, e1_ty, e2)) in
         let depth_in_rhs = curried_depth_in_lambda e1 in
         if depth_in_rhs > 0
         then
           match usage_in_expr Coq.Datatypes.O e2 with
           | Unused | Other -> return e1 ts e2
           | Application depth ->
             if depth_in_rhs >= depth && depth > 1
             then
               let e1, e1_ty = uncurry_rhs depth e1 in
               let e2 = uncurry_in_expression (Coq.Datatypes.O) depth e2 in
               return e1 [e1_ty] e2
             else
               return e1 ts e2
         else
           e
       | _ -> e)
