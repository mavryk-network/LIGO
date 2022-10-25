module Errors = Errors
open Errors
open Mini_c
open Simple_utils.Trace

module Coq = Ligo_coq_ocaml
module I = Ligo_coq_ocaml.Compiler
module Micheline = Tezos_micheline.Micheline
module Coq_Optimization = Ligo_coq_ocaml.Compiler
(* module Ligo_string = Simple_utils.Ligo_string *)

type meta = Mini_c.meta
type bynder_meta = Mini_c.binder_meta
type base_type = (meta, string) Micheline.node
type oty = (meta, base_type) I.ty
type expr = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.expr
type binds = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.binds
type args = (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) I.args

let eta_expand : expression -> type_expression -> type_expression -> anon_function =
  fun e in_ty out_ty ->
    let binder = Ligo_prim.Value_var.fresh () in
    let var = e_var binder in_ty in
    let app = e_application e out_ty var in
    { binder = binder ; body = app }
let get_t_function ~raise e =
  trace_option ~raise not_a_function @@ Mini_c.get_t_function e

let get_function_or_eta_expand ~raise e =
  let in_ty, out_ty = match e.type_expression.type_content with
    | T_function t -> t
    | _ -> raise.error (corner_case "contract do not have the type of a function")
  in
  match e.content with
  | E_closure f -> f
  | _ ->
    eta_expand e in_ty out_ty

(* TODO hack to specialize map_expression to identity monad *)
let map_expression = Helpers.map_expr

(* Conservative purity test: ok to treat pure things as impure, must
   not treat impure things as pure. *)

let rec is_pure : expr -> bool =
  fun e ->
  let self = is_pure in
  let self_binds = binds_is_pure in
  let self_args = args_are_pure in
  match e with
  | I.E_var _ | I.E_literal _ | I.E_raw_michelson _ | I.E_unit _ -> true

  | I.E_let_in (_, e, b) -> self e && self_binds b
  | I.E_tuple (_, a) -> self_args a
  | I.E_let_tuple (_, e, b) -> self e && self_binds b
  | I.E_proj (_, e, _, _) -> self e
  | I.E_update (_, a, _, _) -> self_args a

  | I.E_lam (_, _b, _) -> true

  | I.E_pair (_, a) -> self_args a
  | I.E_car (_, e) -> self e
  | I.E_cdr (_, e) -> self e
  | I.E_left (_, _, e) -> self e
  | I.E_right (_, _, e) -> self e

  | I.E_if_left (_, e, b1, b2) -> self e && List.for_all ~f:self_binds [b1; b2]
  | I.E_if_bool (_, e1, e2, e3) -> List.for_all ~f:self [e1; e2; e3]
  | I.E_if_none (_, e1, e2, b) -> List.for_all ~f:self [e1; e2] && self_binds b
  | I.E_if_cons (_, e1, b, e2) -> List.for_all ~f:self [e1; e2] && self_binds b

  (* we use predefined purity from previous pass *)
  | I.E_inline_michelson (_, is_pure, _, a) -> is_pure && self_args a

  (* very not pure *)
  | I.E_failwith _ -> false
  | I.E_global_constant _ | I.E_create_contract _ -> false

  (* TODO E_let_mut_in is pure when the rhs is pure and the body's
     only impurity is assign/deref of the bound mutable variable *)
  | E_let_mut_in _
  | E_assign _
  | E_deref _ ->
     false

  (* these could be pure through the exception above for
     E_let_mut_in *)
  | E_for _ | E_for_each _ -> false

  (* never pure in any important case *)
  | E_while _ -> false

  (* I'm not sure about these. Maybe can be tested better? *)
  | I.E_fold _ | I.E_fold_right _ | I.E_iter _ | I.E_app _
  | I.E_map _  | I.E_loop_left _ -> false
and binds_is_pure = function
  | I.Binds (_, _, body) -> is_pure body
and args_are_pure = function
  | I.Args_nil _ -> true
  | I.Args_cons (_, a, args) -> is_pure a && args_are_pure args

let rec occurs_count : Coq.Datatypes.nat -> expr -> int =
  fun k e ->
  let self e = occurs_count k e in
  let self_binds = function
    | I.Binds (_, tys, body) ->
       occurs_count (Coq.Nat.add (Coq.Datatypes.length tys) k) body in
  let rec self_args = function
    | I.Args_nil _ -> 0
    | I.Args_cons (_, x, xs) -> self x + self_args xs in
  match e with
  | I.E_var (_, i) -> if Coq.PeanoNat.Nat.eqb k i then 1 else 0
  | I.E_deref (_, i) -> if Coq.PeanoNat.Nat.eqb k i then 1 else 0
  | I.E_assign (_, i, e) ->
     let x = if Coq.PeanoNat.Nat.eqb k i then 1 else 0 in
     x + self e
  | I.E_let_in (_, e, b) -> self e + self_binds b
  | I.E_let_mut_in (_, e, b) -> self e + self_binds b
  | I.E_tuple (_, a) -> self_args a
  | I.E_let_tuple (_, e, b) -> self e + self_binds b
  | I.E_proj (_, e, _, _) -> self e
  | I.E_update (_, a, _, _) -> self_args a
  | I.E_app (_, a) -> self_args a
  | I.E_lam (_, b, _) -> self_binds b
  | I.E_literal (_, _) -> 0
  | I.E_pair (_, a) -> self_args a
  | I.E_car (_, e) -> self e
  | I.E_cdr (_, e) -> self e
  | I.E_unit _ -> 0
  | I.E_left (_, _, e) -> self e
  | I.E_right (_, _, e) -> self e
  | I.E_if_left (_, e, b1, b2) -> self e + self_binds b1 + self_binds b2
  | I.E_if_bool (_, e1, e2, e3) -> self e1 + self e2 + self e3
  | I.E_if_none (_, e1, e2, b) -> self e1 + self e2 + self_binds b
  | I.E_if_cons (_, e1, b, e2) -> self e1 + self_binds b + self e2
  | I.E_iter (_, b, e) -> self_binds b + self e
  | I.E_map (_, b, e) -> self_binds b + self e
  | I.E_loop_left (_, b, _, e) -> self_binds b + self e
  | I.E_fold (_, e1, e2, b) -> self e1 + self e2 + self_binds b
  | I.E_fold_right (_, _, e1, e2, b) -> self e1 + self e2 + self_binds b
  | I.E_failwith (_, e) -> self e
  | I.E_raw_michelson (_, _, _, _) -> 0
  | I.E_inline_michelson (_, _, _, a) -> self_args a
  | I.E_global_constant (_, _, _, a) -> self_args a
  | I.E_create_contract (_, _, _, b, a) -> self_binds b + self_args a
  | I.E_for (_, e1, e2) -> self_args e1 + self_binds e2
  | I.E_for_each (_, e1, e2) -> self e1 + self_binds e2
  | I.E_while (_, e1, e2) -> self e1 + self e2


(* Let "inlining" mean transforming the code:

     let x = e1 in e2

   to:

     e2[e1/x]

   (where the latter signifies substituting e1 for x in e2.)

   Things which can go wrong for inlining:

   - If `e1` is not pure, inlining may fail to preserve semantics.
   - Free variables of `e1` may be shadowed in e2, at usages of `x`. This
     is not a problem if the substitution is capture-avoiding.
   - ?
*)

let is_variable : expr -> bool = function
  | I.E_var _ -> true
  | _ -> false

let should_inline : meta -> expr -> expr ->  bool =
  fun m e1 e2 -> (* e2[x:=e1] *)
  let should_inline_here = m.inline_let_in in
  (is_pure e1 && (should_inline_here
               || (occurs_count Coq.Datatypes.O e2) <= 1
               || is_variable e1))

let inline_let : bool ref -> expr -> expr =
  fun changed e ->
  let e' = I.inline should_inline e in
  changed := !changed || not (Helpers.expr_eq e e');
  e'

let inline_lets : bool ref -> expr -> expr =
  fun changed ->
  inline_let changed

(* TODO: clarify policy of copying [meta]
   - should we save it if we change some child ?
   - should we copy [meta] if we replace node ?
 *)


(* Let "beta" mean transforming the code:

     (\x. e1) e2

   to:

     let x = e2 in e1

   Things which can go wrong for beta reduction:

   - Nothing?
*)

let beta : bool ref -> expr -> expr =
  fun changed e ->
  (* Printf.printf "perform beta"; *)
  let dummy = dummy_meta in
  match e with
  (** (λ x. y) z ↦ let x = y in z *)
  | I.E_app (_m, I.Args_cons (_, e2,
                 I.Args_cons (_, I.E_lam (_, I.Binds (_, [t], body), _ret_ty),
                 I.Args_nil _))) ->
     (changed := true;
      I.E_let_in (dummy, e2, I.Binds (dummy, [t], body)))

  (** also do CAR (PAIR x y) ↦ x, or CDR (PAIR x y) ↦ y, only if x and y are pure *)
  | I.E_inline_michelson (_, _, [Prim (_, ("CDR" | "CAR"  as const), _, _)], args) ->
     begin match args with
     | I.Args_cons (_, I.E_inline_michelson (_, _, [Prim (_, "PAIR", _, _)], args), I.Args_nil _) ->
        begin match args with
        | I.Args_cons (_, y, I.Args_cons (_, x, I.Args_nil _)) ->
           if is_pure x && is_pure y
           then (changed := true;
                 match const with "CAR" -> x | "CDR" -> y | _ -> assert false)
           else e
        | _ -> e
        end
     | _ -> e
     end

  | I.E_car (_, I.E_pair (_, args)) ->
     begin match args with
     | I.Args_cons (_, y, I.Args_cons (_, x, I.Args_nil _)) ->
        if is_pure x && is_pure y
        then (changed := true; x)
           else e
     | _ -> e
     end

  | I.E_cdr (_, I.E_pair (_, args)) ->
     begin match args with
     | I.Args_cons (_, y, I.Args_cons (_, x, I.Args_nil _)) ->
        if is_pure x && is_pure y
        then (changed := true; y)
        else e
     | _ -> e
     end


  (** (e0, e1, ...).(i) ↦ ei  (only if all ei are pure) *)
  | I.E_proj (_, I.E_tuple (_, es), i, _n) ->
    let es = Helpers.args_to_list es in
    if List.for_all ~f:is_pure es
    then (changed := true;
          List.nth_exn es (Helpers.nat_to_int i)) (* FIXME *)
    else e

  (** (let x = e1 in e2).(i) ↦ (let x = e1 in e2.(i)) *)
  | I.E_proj (_, I.E_let_in (ml, e1, I.Binds (mb, t, e2)), i, n) ->
     let e2' = I.E_proj (dummy, e2, i, n) in
     changed := true;
     I.E_let_in (ml, e1, I.Binds (mb, t, e2'))

  (** (let (x, y, ...) = e1 in e2).(i) ↦ (let (x, y, ...) = e1 in e2.(i)) *)
  | I.E_proj (_, I.E_let_tuple (ml, e1, I.Binds (mb, t, e2)), i, n) ->
     let e2' = I.E_proj (dummy, e2, i, n) in
     (changed := true;
      I.E_let_tuple (ml, e1, I.Binds (mb, t, e2')))

  (** (let x = (let y = e1 in e2) in e3) ↦ (let y = e1 in let x = e2 in e3) *)
  | I.E_let_in (mx, I.E_let_in (my, e1,
                    I.Binds (mby, [tyy], e2)),
    I.Binds (mbx, [tyx], e3)) ->
    (changed := true;
     let e3' = Coq.Compiler.shift Helpers.nat_one Helpers.nat_one e3 in
     I.E_let_in (my, e1, I.Binds (mby, [tyy],
     I.E_let_in (mx, e2, I.Binds (mbx, [tyx], e3')))))

  (** note: E_let_tuple/E_let_in and E_let_in/E_let_tuple conversions
      not implemented yet because they don't seem important (?) *)

  (** (let x = e1 in e2)@e3 ↦ let x = e1 in e2@e3  (if e1 or e3 is pure) *)
  | I.E_app (m, I.Args_cons (mc1, e3,
                I.Args_cons (mc2, I.E_let_in (ml, e1, I.Binds (mx, [tyx], e2)),
                I.Args_nil mnil))) ->
     if is_pure e1 || is_pure e3 then
      (changed := true;
       let e3' = Coq.Compiler.shift Helpers.nat_one Helpers.nat_zero e3 in
       let args = Helpers.args_of_list' [e3'; e2] [mc1; mc2; mnil] in
       I.E_let_in (ml, e1, I.Binds (mx, [tyx], I.E_app (m, args))))
     else e

  (** (let (x, y, ...) = e1 in e2)@e3 ↦ let (x, y, ...) = e1 in e2@e3  (if e1 or e3 is pure) *)
  | I.E_app (m, I.Args_cons (mc1, e3,
                I.Args_cons (mc2, I.E_let_tuple (ml, e1, I.Binds (mb, tys, e2)),
                I.Args_nil mnil))) ->
     if is_pure e1 || is_pure e3 then
      (changed := true;
       let e3' = Coq.Compiler.shift (Coq.Datatypes.length tys) Helpers.nat_one e3 in
       let args = Helpers.args_of_list' [e3'; e2] [mc1; mc2; mnil] in
       I.E_let_tuple (ml, e1, I.Binds (mb, tys, I.E_app (m, args))))
     else e


  (* let (x0, x1, ...) = (e0, e1, ...) in body ↦
     let ... in let x1 = e1 in let x0 = e0 in body
     (here, purity of the ei does not matter)
     *)
  | I.E_let_tuple (_, I.E_inline_michelson (_, _, [Prim (_, "PAIR", _, _)], es), I.Binds (_, tys, body))
  | I.E_let_tuple (_, I.E_tuple (_, es), I.Binds (_, tys, body)) ->
     changed := true;
     let es = Helpers.args_to_list es in
     (* NOTE: arguments in reversed order and we need shift expressions:
        let (V2, V1, V0) = e2, e1, e0 in ...
        ↦
        let (V2) = e2 in ...
        let (V1) = ↑e1 in
        let (V0) = ↑↑e0 in
      *)
     let _, es =
       List.fold_left es ~init:(Helpers.nat_zero, []) ~f:(fun (n, acc) e ->
         let e' = Coq.Compiler.shift n Helpers.nat_zero e in
         (S n, e' :: acc))
     in
     let es = List.rev es in
     List.fold_right (List.zip_exn es tys) ~init:body ~f:(
       fun (e, ty) body ->
         I.E_let_in (dummy, e, I.Binds (dummy, [ty], body)))
  | _ -> e

let betas : bool ref -> expr -> expr =
  fun changed ->
  map_expression (beta changed)

let eta : bool ref -> expr -> expr =
  fun changed e ->
  match e with
  (* PAIR (CAR e1, CDR e1) ↦ e1 *)
  | I.E_inline_michelson (_, _, [Prim (_, "PAIR", _, _)],
    I.Args_cons (_, I.E_inline_michelson (_, _, [Prim (_, "CDR", _, _)], argsr),
    I.Args_cons (_, I.E_inline_michelson (_, _, [Prim (_, "CAR", _, _)], argsl),
    I.Args_nil _))) ->
    begin match (argsl, argsr) with
    | I.Args_cons (_, I.E_var (m, li), I.Args_nil _),
      I.Args_cons (_, I.E_var (_, ri), I.Args_nil _)
         when Coq.PeanoNat.Nat.eq_dec li ri ->
       (changed := true; I.E_var (m, li))
    | _ -> e
    end

  (* (x.(0), x.(1), ...) ↦ x *)
  | I.E_tuple (m, es) ->
    let es = Helpers.args_to_list es in
    let count = List.length es in
    let projs =
      List.mapi
        ~f:(fun i e ->
           match e with
           | I.E_proj (_, e', j, n) ->
             let j = Helpers.nat_to_int j in
             let n = Helpers.nat_to_int n in
             if i = j && n = count
             then
               match e' with
               | I.E_var (_, x) -> Some x
               | _ -> None
             else None
           | _ -> None)
        es in
    (match Option.all projs with
     | None -> e
     | Some vars ->
       match vars with
       | var :: _ ->
         if List.for_all ~f:(Coq.PeanoNat.Nat.eqb var) vars
         then I.E_var (m, var)
         else e
       | _ -> e)
  | _ -> e

let etas : bool ref -> expr -> expr =
  fun changed ->
  map_expression (eta changed)

(* let compiler_push : bool ref -> expr -> expr = *)
(*   fun changed -> *)
(*   map_expression (fun e -> *)
(*     match e with *)
(*     | E_inline_michelson (m, thunk, [Prim (_, "PUSH", _, _)], *)
(*                           Args_cons (_, E_literal (_, Literal_string s), Args_nil m0)) *)
(*       -> let hash = Ligo_string.extract s in *)
(*          let prim_args = [Stacking.To_micheline.translate_type ; Micheline.String (dummy_meta, hash)] in *)
(*        E_inline_michelson (m, thunk, [Prim (_, "PUSH", prim_args, _)], Args_nil m0) *)
(*     | _ -> e *)
(*   ) *)

let contract_check ~raise (init: binds) : binds =
  let all = [Michelson_restrictions.self_in_lambdas ~raise] in
  let all_e = List.map ~f:(Helpers.map_sub_level_expression) all in
  List.fold ~f:(|>) all_e ~init
let rec all_expression ~raise : expr -> expr =
  fun e ->
  let changed = ref false in
  (* Format.printf "%a\n\n" Helpers.pp_expr e; *)
  let e = inline_lets changed e in
  (* Format.printf "INLINE:\n%a\n\n" Helpers.pp_expr e; *)
  let e = betas changed e in
  (* Format.printf "BETA:\n%a\n\n" Helpers.pp_expr e; *)
  let e = etas changed e in
  if !changed
  then all_expression ~raise e
  else e

let all_expression ~raise e =
  (* Format.printf "ORIGINAL:\n%a\n\n" Helpers.pp_expr e; *)
  let e = Uncurry.uncurry_expression e in
  (* Format.printf "UNCURRY:\n%a\n\n" Helpers.pp_expr e; *)
  let e = all_expression ~raise e in
  e
