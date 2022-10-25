module Pair   = Simple_utils.Pair
module Triple = Simple_utils.Triple

module Coq = Ligo_coq_ocaml
module I = Ligo_coq_ocaml.Compiler
module Micheline = Tezos_micheline.Micheline

(* TODO: make it shared *)
type meta = Mini_c.meta
type bynder_meta = Mini_c.binder_meta
type base_type = (meta, string) Micheline.node
type ty = (meta, base_type) I.ty
type expr = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.expr
type binds = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.binds
type args = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.args

let nat_zero = Coq.Datatypes.O
let nat_one  = Coq.Datatypes.S nat_zero
let nat_two  = Coq.Datatypes.S nat_one

let rec args_to_list (xs : args) : expr list =
  match xs with
  | Args_cons (_, x, xs) -> x :: args_to_list xs
  | Args_nil  (_)        -> []

(* make [args] type from list of expression regarding inverse order *)
let rec args_of_list (xs : expr list) : args =
  match xs with
  | x :: xs -> Args_cons (Mini_c.dummy_meta, x, args_of_list xs)
  | [] -> Args_nil (Mini_c.dummy_meta)

let rec args_of_list' (xs : expr list) (metas : meta list) : args =
  match xs, metas with
  | x :: xs, m :: ms -> Args_cons (m, x, args_of_list' xs ms)
  | [], m :: [] -> Args_nil (m)
  | _, _ -> assert false

let rec nat_to_int (n : Ligo_coq_ocaml.Datatypes.nat) =
  match n with
  | O -> 0
  | S (n) -> 1 + nat_to_int (n)

let print_nat ppf (n : Coq.Datatypes.nat) =
  Format.fprintf ppf "%d" (nat_to_int n)

let rec pp_ty ppf (t : ty) : unit =
  match t with
  | I.T_base (_, _) -> Format.fprintf ppf "(T_base)"
  | I.T_unit _ -> Format.fprintf ppf "(T_unit)"
  | I.T_pair (_, _, _, lt, rt) -> Format.fprintf ppf "(T_pair %a %a)" pp_ty lt pp_ty rt
  | I.T_or (_, _, _, _, _) -> Format.fprintf ppf "(T_or)"
  | I.T_func (_, a, b) -> Format.fprintf ppf "(T_func %a %a)" pp_ty a pp_ty b
  | I.T_lambda (_, _, _) -> Format.fprintf ppf "(T_lambda)"
  | I.T_option (_, _) -> Format.fprintf ppf "(T_option)"
  | I.T_list (_, t) -> Format.fprintf ppf "(T_list %a)" pp_ty t
  | I.T_set (_, _) -> Format.fprintf ppf "(T_set)"
  | I.T_map (_, _, _) -> Format.fprintf ppf "(T_map)"
  | I.T_big_map (_, _, _) -> Format.fprintf ppf "(T_big_map)"
  | I.T_ticket (_, _) -> Format.fprintf ppf "(T_ticket)"
  | I.T_contract (_, _) -> Format.fprintf ppf "(T_contract)"
  | I.T_bool _ -> Format.fprintf ppf "(T_bool)"
  | I.T_int _ -> Format.fprintf ppf "(T_int)"
  | I.T_nat _ -> Format.fprintf ppf "(T_nat)"
  | I.T_mutez _ -> Format.fprintf ppf "(T_mutez)"
  | I.T_string _ -> Format.fprintf ppf "(T_string)"
  | I.T_bytes _ -> Format.fprintf ppf "(T_bytes)"
  | I.T_address _ -> Format.fprintf ppf "(T_address)"
  | I.T_key_hash _ -> Format.fprintf ppf "(T_key_hash)"
  | I.T_operation _ -> Format.fprintf ppf "(T_operation)"

let rec pp_expr ppf (e : (meta, 'b, 'c, 'd) I.expr) : unit =
  match e with
  | I.E_var (_, i) -> Format.fprintf ppf "(Var %a)" print_nat i
  | I.E_let_in (m, e, b) ->
     let should_inline = if m.inline_let_in then "[@inline]" else "" in
     Format.fprintf ppf "(E_let_in %s%a %a)" should_inline  pp_expr e pp_binds b

  | I.E_deref (_, i) -> Format.fprintf ppf "(E_deref %a)" print_nat i
  | I.E_let_mut_in (_, e, b) ->
     Format.fprintf ppf "(E_let_mut_in %a %a)" pp_expr e pp_binds b
  | I.E_assign (_, i, e) ->
     Format.fprintf ppf "(E_assign %a %a)" print_nat i pp_expr e
  | I.E_for (_, e1, e2) ->
     Format.fprintf ppf "(E_for %a %a)" pp_args e1 pp_binds e2
  | I.E_for_each (_, e1, e2) ->
     Format.fprintf ppf "(E_for_each %a %a)" pp_expr e1 pp_binds e2
  | I.E_while (_, e1, e2) ->
     Format.fprintf ppf "(E_while %a %a)" pp_expr e1 pp_expr e2

  | I.E_tuple (_, a) -> Format.fprintf ppf "(E_tuple %a)" pp_args a
  | I.E_let_tuple (_, e, b) -> Format.fprintf ppf "(E_let_tuple %a %a)" pp_expr e pp_binds b
  | I.E_proj (_, _, _, _) -> Format.fprintf ppf "(E_proj )"
  | I.E_update (_, _, _, _) -> Format.fprintf ppf "(E_update )"
  | I.E_app (_, a) -> Format.fprintf ppf "(E_app %a)" pp_args a
  | I.E_lam (_, b, ret_ty) -> Format.fprintf ppf "(E_lam [%a] %a)" pp_ty ret_ty pp_binds b
  | I.E_literal (_, _) -> Format.fprintf ppf "(E_literal )"
  | I.E_pair (_, a) -> Format.fprintf ppf "(E_pair %a)" pp_args a
  | I.E_car (_, e) -> Format.fprintf ppf "(E_car %a)" pp_expr e
  | I.E_cdr (_, e) -> Format.fprintf ppf "(E_cdr %a)" pp_expr e
  | I.E_unit _ -> Format.fprintf ppf "E_Unit"
  | I.E_left (_, _, _) -> Format.fprintf ppf "(E_left )"
  | I.E_right (_, _, _) -> Format.fprintf ppf "(E_right )"
  | I.E_if_left (_, e, b1, b2) -> Format.fprintf ppf "(E_if_left %a %a %a)" pp_expr e pp_binds b1 pp_binds b2
  | I.E_if_bool (_, e1, e2, e3) -> Format.fprintf ppf "(E_if_bool (COND %a) (THEN %a) (ELSE %a))" pp_expr e1 pp_expr e2 pp_expr e3
  | I.E_if_none (_, e1, e2, b3) -> Format.fprintf ppf "(E_if_none (COND %a) (NONE_PAT %a) (SOME_PAT %a))" pp_expr e1 pp_expr e2 pp_binds b3
  | I.E_if_cons (_, _, _, _) -> Format.fprintf ppf "(E_if_cons )"
  | I.E_iter (_, _, _) -> Format.fprintf ppf "(E_iter )"
  | I.E_map (_, _, _) -> Format.fprintf ppf "(E_map )"
  | I.E_loop_left (_, _, _, _) -> Format.fprintf ppf "(E_loop_left )"
  | I.E_fold (_, _, _, _) -> Format.fprintf ppf "(E_fold )"
  | I.E_fold_right (_, _, _, _, _) -> Format.fprintf ppf "(E_fold_right )"
  | I.E_failwith (_, _) -> Format.fprintf ppf "(E_failwith )"
  | I.E_raw_michelson (_, _, _, _) -> Format.fprintf ppf "(E_raw_michelson )"
  | I.E_inline_michelson (_, is_pure, mich, a) -> Format.fprintf ppf "(E_inline_michelson %b %a %a)" is_pure pp_mich mich pp_args a
  | I.E_global_constant (_, _, _, _) -> Format.fprintf ppf "(E_global_constant )"
  | I.E_create_contract (_, _, _, _, _) -> Format.fprintf ppf "(E_create_contract )"
and pp_binds ppf (b : binds) =
  match b with
  | I.Binds (_, tys, e) -> Format.fprintf ppf "(Binds [%a] %a)"
                               (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_ty) tys
                               pp_expr e
and pp_args ppf (a : args) =
  let a = args_to_list a in
  Format.fprintf ppf "(Args %a)" (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_expr) a

and pp_mich ppf prims =
  let pp_one ppf = function
  | Micheline.Prim (_, name, _, _) -> Format.fprintf ppf "%s" name
  | _ -> Format.fprintf ppf "MICH"
  in
  Format.pp_print_list ~pp_sep:Format.pp_print_space pp_one ppf prims

let get_meta (e: expr) : meta =
  match e with
  | I.E_var (m, _) -> m
  | I.E_let_in (m, _, _) -> m
  | I.E_deref (m, _) -> m
  | I.E_let_mut_in (m, _, _) -> m
  | I.E_assign (m, _, _) -> m
  | I.E_for (m, _, _) -> m
  | I.E_for_each (m, _, _) -> m
  | I.E_while (m, _, _) -> m
  | I.E_tuple (m, _) -> m
  | I.E_let_tuple (m, _, _) -> m
  | I.E_proj (m, _, _, _) -> m
  | I.E_update (m, _, _, _) -> m
  | I.E_app (m, _) -> m
  | I.E_lam (m, _, _) -> m
  | I.E_literal (m, _) -> m
  | I.E_pair (m, _) -> m
  | I.E_car (m, _) -> m
  | I.E_cdr (m, _) -> m
  | I.E_unit m -> m
  | I.E_left (m, _, _) -> m
  | I.E_right (m, _, _) -> m
  | I.E_if_left (m, _, _, _) -> m
  | I.E_if_bool (m, _, _, _) -> m
  | I.E_if_none (m, _, _, _) -> m
  | I.E_if_cons (m, _, _, _) -> m
  | I.E_iter (m, _, _) -> m
  | I.E_map (m, _, _) -> m
  | I.E_loop_left (m, _, _, _) -> m
  | I.E_fold (m, _, _, _) -> m
  | I.E_fold_right (m, _, _, _, _) -> m
  | I.E_failwith (m, _) -> m
  | I.E_raw_michelson (m, _, _, _) -> m
  | I.E_inline_michelson (m, _, _, _) -> m
  | I.E_global_constant (m, _, _, _) -> m
  | I.E_create_contract (m, _, _, _, _) -> m

let rec ty_eq (ty1 : ty) (ty2 : ty) : bool =
  let self_pair t1l t2l t1r t2r = ty_eq t1l t2l && ty_eq t1r t2r in
  let self = ty_eq in
  match ty1, ty2 with
  |  I.T_base (_, _t1), I.T_base (_, _t2) -> true
  |  I.T_unit _, I.T_unit _ -> true
  |  I.T_pair (_, _, _, tl1, tr1), I.T_pair (_, _, _, tl2, tr2) -> self_pair tl1 tl2 tr1 tr2
  |  I.T_or (_, _, _, tl1, tr1), I.T_or (_, _, _, tl2, tr2) -> self_pair tl1 tl2 tr1 tr2
  |  I.T_func (_, ta1, tb1), I.T_func (_, ta2, tb2) -> self_pair ta1 ta2 tb1 tb2
  |  I.T_lambda (_, ta1, tb1), I.T_lambda (_, ta2, tb2) -> self_pair ta1 ta2 tb1 tb2
  |  I.T_option (_, t1), I.T_option (_, t2) -> self t1 t2
  |  I.T_list (_, t1), I.T_list (_, t2) -> self t1 t2
  |  I.T_set (_, t1), I.T_set (_, t2) -> self t1 t2
  |  I.T_map (_, ta1, tb1), I.T_map (_, ta2, tb2) -> self_pair ta1 ta2 tb1 tb2
  |  I.T_big_map (_, ta1, tb1), I.T_big_map (_, ta2, tb2) -> self_pair ta1 ta2 tb1 tb2
  |  I.T_ticket (_, t1), I.T_ticket (_, t2) -> self t1 t2
  |  I.T_contract (_, t1), I.T_contract (_, t2) -> self t1 t2
  |  I.T_bool _, I.T_bool _ -> true
  |  I.T_int _, I.T_int _ -> true
  |  I.T_nat _, I.T_nat _ -> true
  |  I.T_mutez _, I.T_mutez _ -> true
  |  I.T_string _, I.T_string _ -> true
  |  I.T_bytes _, I.T_bytes _ -> true
  |  I.T_address _, I.T_address _ -> true
  |  I.T_key_hash _, I.T_key_hash _ -> true
  |  I.T_operation _, I.T_operation _ -> true
  | _, _ -> false

let all = List.fold ~init:true ~f:(&&)

(* TODO we should not use this *)
let rec expr_eq (e1 : expr) (e2 : expr) : bool =
  let nat_eq = Coq.PeanoNat.Nat.eqb in
  let self = expr_eq in
  match e1, e2 with
  |  I.E_var (_, il), I.E_var (_, ir)
     -> nat_eq il ir
  |  I.E_let_in (_, el, bl), I.E_let_in (_, er, br)
     -> all [self el er; binds_eq bl br]
  |  I.E_tuple (_, al), I.E_tuple (_, ar)
     -> args_eq al ar
  |  I.E_let_tuple (_, el, bl), I.E_let_tuple (_, er, br)
     -> all [self el er; binds_eq bl br]
  |  I.E_proj (_, el, il, nl), I.E_proj (_, er, ir, nr)
     -> all [self el er; nat_eq il ir; nat_eq nl nr]
  |  I.E_update (_, al, il, nl), I.E_update (_, ar, ir, nr)
     -> all [args_eq al ar; nat_eq il ir; nat_eq nl nr]
  |  I.E_app (_, al), I.E_app (_, ar)
     -> args_eq al ar
  |  I.E_lam (_, bl, tl), I.E_lam (_, br, tr)
     -> all [binds_eq bl br; ty_eq tl tr]
  |  I.E_literal (_, _ll), I.E_literal (_, _lr)
     -> true (* TODO *)
  |  I.E_pair (_, al), I.E_pair (_, ar)
     -> args_eq al ar
  |  I.E_car (_, al), I.E_car (_, ar)
     -> self al ar
  |  I.E_cdr (_, al), I.E_cdr (_, ar)
     -> self al ar
  |  I.E_unit _, I.E_unit _
     -> true
  |  I.E_left (_, tl, el), I.E_left (_, tr, er)
     -> all [ty_eq tl tr; self el er]
  |  I.E_right (_, tl, el), I.E_right (_, tr, er)
     -> all [ty_eq tl tr; self el er]
  |  I.E_if_left (_, el, b1l, b2l), I.E_if_left (_, er, b1r, b2r)
     -> all [self el er; binds_eq b1l b1r; binds_eq b2l b2r]
  |  I.E_if_bool (_, e1l, e2l, e3l), I.E_if_bool (_, e1r, e2r, e3r)
     -> all [self e1l e1r; self e2l e2r; self e3l e3r]
  |  I.E_if_none (_, e1l, e2l, bl), I.E_if_none (_, e1r, e2r, br)
     -> all [self e1l e1r; self e2l e2r; binds_eq bl br]
  |  I.E_if_cons (_, e1l, bl, e2l), I.E_if_cons (_, e1r, br, e2r)
     -> all [self e1l e1r; binds_eq bl br; self e2l e2r]
  |  I.E_iter (_, bl, el), I.E_iter (_, br, er)
     -> all [binds_eq bl br; self el er]
  |  I.E_map (_, bl, el), I.E_map (_, br, er)
     -> all [binds_eq bl br; self el er]
  |  I.E_loop_left (_, bl, tl, el), I.E_loop_left (_, br, tr, er)
     -> all [self el er; ty_eq tl tr; binds_eq bl br]
  |  I.E_fold (_, e1l, e2l, bl), I.E_fold (_, e1r, e2r, br)
     -> all [self e1l e1r; self e2l e2r; binds_eq bl br]
  |  I.E_fold_right (_, tl, e1l, e2l, bl), I.E_fold_right (_, tr, e1r, e2r, br)
     -> all [ty_eq tl tr; self e1l e1r; self e2l e2r; binds_eq bl br]
  |  I.E_failwith (_, el), I.E_failwith (_, er)
     -> self el er
  |  I.E_raw_michelson (_, _, _, _), I.E_raw_michelson (_, _, _, _)
     -> true
  |  I.E_inline_michelson (_, _, _, al), I.E_inline_michelson (_, _, _, ar)
     -> args_eq al ar
  |  I.E_global_constant (_, _, _, al), I.E_global_constant (_, _, _, ar)
     -> args_eq al ar
  |  I.E_create_contract (_, _, _, bl, al), I.E_create_contract (_, _, _, br, ar)
     -> all [binds_eq bl br; args_eq al ar]
  |  I.E_deref (_, il), I.E_deref (_, ir)
     -> nat_eq il ir
  |  I.E_assign (_, il, el), I.E_assign (_, ir, er)
     -> all [nat_eq il ir; self el er]
  |  I.E_let_mut_in (_, el, bl), I.E_let_mut_in (_, er, br)
     -> all [self el er; binds_eq bl br]
  |  I.E_for (_, e1l, e2l), I.E_for (_, e1r, e2r)
     -> all [args_eq e1l e1r; binds_eq e2l e2r]
  |  I.E_for_each (_, e1l, e2l), I.E_for_each (_, e1r, e2r)
     -> all [expr_eq e1l e1r; binds_eq e2l e2r]
  |  I.E_while (_, e1l, e2l), I.E_while (_, e1r, e2r)
     -> all [expr_eq e1l e1r; expr_eq e2l e2r]
  (* TODO hides bugs :( *)
  | _, _ -> false
and
  binds_eq (b1 : binds) (b2 : binds) : bool =
  match b1, b2 with
  | I.Binds (_, tys1, e1), I.Binds (_, tys2, e2) ->
     all [List.equal ty_eq tys1 tys2; expr_eq e1 e2]
and args_eq (a1 : args) (a2 : args) : bool =
  match a1, a2 with
  | I.Args_nil _, I.Args_nil _ -> true
  | I.Args_cons (_, x1, a1), I.Args_cons (_, x2, a2) -> all [expr_eq x1 x2; args_eq a1 a2]
  | _, _ -> false

type expr_mapperi = Coq.Datatypes.nat -> expr -> expr

let rec mapi_expr : expr_mapperi -> init:Coq.Datatypes.nat -> expr -> expr =
  fun f ~init e ->
  let self ?(init=init) = mapi_expr ~init f in
  let self_binds ?(init=init) = mapi_binds ~init f in
  let self_args ?(init=init) = mapi_args ~init f in
  let e' = f init e in
  match e' with
  | I.E_var _ -> e'
  | I.E_let_in (m, expr, body) -> (
      let expr' = self expr in
      let body' = self_binds body in
      I.E_let_in (m, expr', body')
  )
  | I.E_deref (m, i) ->
     I.E_deref (m, i)
  | I.E_let_mut_in (m, expr, body) ->
      let expr' = self expr in
      let body' = self_binds body in
      I.E_let_mut_in (m, expr', body')
  | I.E_assign (m, i, e) ->
     let e' = self e in
     I.E_assign (m, i, e')
  | I.E_for (m, e1, e2) ->
     let e1' = self_args e1 in
     let e2' = self_binds e2 in
     I.E_for (m, e1', e2')
  | I.E_for_each (m, e1, e2) ->
     let e1' = self e1 in
     let e2' = self_binds e2 in
     I.E_for_each (m, e1', e2')
  | I.E_while (m, e1, e2) ->
     let e1' = self e1 in
     let e2' = self e2 in
     I.E_while (m, e1', e2')
  | I.E_tuple (m, args) -> (
      let args' = self_args args in
      I.E_tuple (m, args')
  )
  | I.E_let_tuple (m, expr, body) -> (
      let expr' = self expr in
      let body' = self_binds body in
      I.E_let_tuple (m, expr', body')
  )
  | I.E_proj (m, expr, i, n) -> (
      let expr' = self expr in
      I.E_proj (m, expr', i, n)
  )
  | I.E_update (m, args, i, n) -> (
      let args' = self_args args in
      I.E_update (m, args', i, n)
  )
  | I.E_app (m, args) -> (
      let args' = self_args args in
      I.E_app (m, args')
  )
  | I.E_lam (m, body, ty) -> (
      let body' = self_binds body in
      I.E_lam (m, body', ty)
  )
  | I.E_literal _ -> e'
  | I.E_pair (m, args) -> (
      let args' = self_args args in
      I.E_pair (m, args')
  )
  | I.E_car (m, expr) -> (
      let expr' = self expr in
      I.E_car (m, expr')
  )
  | I.E_cdr (m, expr) -> (
      let expr' = self expr in
      I.E_cdr (m, expr')
  )
  | I.E_unit _ -> e'
  | I.E_left (m, ty, expr) -> (
      let expr' = self expr in
      I.E_left (m, ty, expr')
  )
  | I.E_right (m, ty, expr) -> (
      let expr' = self expr in
      I.E_right (m, ty, expr')
  )
  | I.E_if_left (m, expr, body_l, body_r) -> (
      let expr' = self expr in
      let (body_l', body_r') = Pair.map ~f:self_binds (body_l, body_r) in
      I.E_if_left (m, expr', body_l', body_r')
  )
  | I.E_if_bool (m, e1, e2, e3) -> (
      let (e1', e2', e3') = Triple.map ~f:self (e1, e2, e3) in
      I.E_if_bool (m, e1', e2', e3')
  )
  | I.E_if_none (m, c, n, s) -> (
      let (c', n') = Pair.map ~f:self (c, n) in
      let s' = self_binds s in
      I.E_if_none (m, c', n', s')
  )
  | I.E_if_cons (m, cond, c, n) -> (
      let cond' = self cond in
      let c' = self_binds c in
      let n' = self n in
      I.E_if_cons (m, cond', c', n')
  )
  | I.E_iter (m, body, expr) -> (
      let expr' = self expr in
      let body' = self_binds body in
      I.E_iter (m, body', expr')
  )
  | I.E_map (m, body, expr) -> (
      let expr' = self expr in
      let body' = self_binds body in
      I.E_map (m, body', expr')
  )
  | I.E_loop_left (m, body, ty, expr) -> (
      let expr' = self expr in
      let body' = self_binds body in
      I.E_loop_left (m, body', ty, expr')
  )
  | I.E_fold (m, init, coll, body) -> (
      let (init', coll') = Pair.map ~f:self (init, coll) in
      let body' = self_binds body in
      I.E_fold (m, init', coll', body')
  )
  | I.E_fold_right (m, ty, init, coll, body) -> (
      let (init', coll') = Pair.map ~f:self (init, coll) in
      let body' = self_binds body in
      I.E_fold_right (m, ty, init', coll', body')
  )
  | I.E_failwith (m, expr) -> (
      let expr' = self expr in
      I.E_failwith (m, expr')
  )
  | I.E_raw_michelson _ -> e'
  | I.E_inline_michelson (m, p, michs, args) -> (
      let args' = self_args args in
      I.E_inline_michelson (m, p, michs, args')
  )
  | I.E_global_constant (m, ty, hash, args) -> (
      let args' = self_args args in
      I.E_global_constant (m, ty, hash, args')
  )
  | I.E_create_contract (m, tya, tyb, body, args) -> (
      let body' = self_binds body in
      let args' = self_args args in
      I.E_create_contract (m, tya, tyb, body', args')
  )
and mapi_binds : expr_mapperi -> init:Coq.Datatypes.nat -> binds -> binds =
  fun f ~init binds ->
  match binds with
  | I.Binds (m, ty, body) -> (
      let body' = mapi_expr f body ~init:(Coq.Nat.add init (Coq.Datatypes.length ty)) in
      I.Binds (m, ty, body')
  )
and mapi_args : expr_mapperi -> init:Coq.Datatypes.nat -> args -> args =
  fun f ~init args ->
  match args with
  | I.Args_nil _ -> args
  | I.Args_cons (m, arg, args) -> (
      let arg'  = mapi_expr f arg ~init in
      let args' = mapi_args f args ~init in
      I.Args_cons (m, arg', args')
  )

type expr_mapper = expr -> expr

let map_expr : expr_mapper -> expr -> expr =
  fun f e -> mapi_expr (fun _ -> f) e ~init:nat_zero

let map_sub_level_expression : expr_mapper -> binds -> binds = fun f e ->
  match e with
  | Binds (m, tys, body) -> Binds (m, tys, map_expr f body)
