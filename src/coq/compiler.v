Local Set Warnings "-implicit-core-hint-db".
Set Implicit Arguments.
From Coq Require Import String List Arith ZArith Program.Tactics micromega.Lia micromega.Zify.
From Coq Require Extraction.
Import ListNotations.
Open Scope string_scope.

(* For Mini_c *)
(* Require Import Coq.Classes.RelationClasses. *)
From Coq.Relations Require Import Relation_Operators Operators_Properties Relation_Definitions.
Require Import Coq.Program.Equality.

From ligo_coq Require Import ope.

(* http://poleiro.info/posts/2018-10-15-checking-for-constructors.html *)
Ltac head t :=
  (* slightly modified, we will evaluate to get a head *)
  let t := eval hnf in t in
  match t with
  | ?t' _ => head t'
  | _ => t
  end.

Ltac head_constructor t :=
  let t' := head t in is_constructor t'.

Ltac invert H := inversion H; subst; clear H.

Ltac invert_pred p :=
  match goal with
  | [H : p |- _] => invert H
  | [H : p _ |- _] => invert H
  | [H : p _ _ |- _] => invert H
  | [H : p _ _ _ |- _] => invert H
  | [H : p _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  end.

Definition done (T : Type) : Type := T.

Ltac mark_done H :=
  let A := type of H in
  change (done A) in H.

Ltac clear_done :=
  repeat
    match goal with
    | [H : done ?A |- _] =>
      change A in H
    end.

Lemma cut_and1 : forall {a b c : Prop}, (c -> (a /\ b)) <-> ((c -> a) /\ (c -> b)).
Proof. intuition. Qed.

Lemma cut_and2 : forall {A : Type} {P1 P2 : A -> Prop}, (forall x, (P1 x /\ P2 x)) <-> ((forall x, P1 x) /\ (forall x, P2 x)).
Proof. firstorder. Qed.

Ltac cut_IH H :=
  repeat setoid_rewrite cut_and1 in H;
  repeat setoid_rewrite cut_and2 in H.

Axiom bytes : Set.
Extract Inlined Constant bytes => "bytes".

(* I am sorry this is one big section. It seemed like the easiest,
   stupidest way to deal with the Context assumptions below. If you
   find a way to break this up into multiple files, please do it.

   Maybe most of the Context should instead be Axioms with custom
   Extraction? *)
Section compiler.

Local Open Scope list.

Context {meta : Set}. (* metadata for expressions (source location, etc) *)
Context {null : meta}. (* null metadata *)
Context {base_type : Set}. (* arbitrary base types (probably Micheline) *)

(* Types: essentially Michelson's types, shared by source and target
   stages here *)
Inductive ty : Set :=
| T_base : meta -> base_type -> ty

| T_unit : meta -> ty
| T_pair : meta -> option string -> option string -> ty -> ty -> ty

| T_or : meta -> option string -> option string -> ty -> ty -> ty

| T_func : meta -> ty -> ty -> ty
| T_lambda : meta -> ty -> ty -> ty

| T_option : meta -> ty -> ty
| T_list : meta -> ty -> ty
| T_set : meta -> ty -> ty
| T_map : meta -> ty -> ty -> ty
| T_big_map : meta -> ty -> ty -> ty
| T_ticket : meta -> ty -> ty
| T_contract : meta -> ty -> ty
(* TODO delete the types we don't need *)
| T_bool : meta -> ty
| T_int : meta -> ty
| T_nat : meta -> ty
| T_mutez : meta -> ty
| T_string : meta -> ty
| T_bytes : meta -> ty
(* these only exist for CREATE_CONTRACT *)
| T_address : meta -> ty
| T_key_hash : meta -> ty
| T_operation : meta -> ty
.

Context {with_var_names : list ty -> meta -> meta}.
Context {lit micheline : Set}.
Context {lit_code : meta -> lit -> list micheline}.
Context {global_constant : meta -> string -> list micheline}.


(* Source language: de Bruijn LIGO expressions. For comments about
   specific constructors, see the typing derivations [expr_typed]
   below.

   The expressions are currently given as three mutual inductive
   types: [expr] for expressions, [binds] for binding forms in
   expressions (lambda bodies, match cases, etc,) and [args] for
   tuples of expressions.

   [args] is isomorphic to [list expr], but is defined mutually in
   order to avoid nested induction. Compare this to the different
   choice adopted for Michelson below, where [prog] _is_ just [list
   instr], and so nested induction is required to define
   [strengthen_instr]. *)
Inductive expr : Set :=
| E_var : meta -> nat -> expr
| E_let_in : meta -> expr -> binds -> expr

| E_tuple : meta -> args -> expr
| E_let_tuple : meta -> expr -> binds -> expr
| E_proj : meta -> expr -> nat -> nat -> expr
| E_update : meta -> args -> nat -> nat -> expr

| E_app : meta -> args -> expr
| E_lam : meta -> binds -> ty -> expr

| E_literal : meta -> lit -> expr

| E_pair : meta -> args -> expr
| E_car : meta -> expr -> expr
| E_cdr : meta -> expr -> expr

| E_unit : meta -> expr

| E_left : meta -> ty -> expr -> expr
| E_right : meta -> ty -> expr -> expr

| E_if_left : meta -> expr -> binds -> binds -> expr
| E_if_bool : meta -> expr -> expr -> expr -> expr
| E_if_none : meta -> expr -> expr -> binds -> expr
| E_if_cons : meta -> expr -> binds -> expr -> expr

| E_iter : meta -> binds -> expr -> expr
| E_map : meta -> binds -> expr -> expr
| E_loop_left : meta -> binds -> ty -> expr -> expr
| E_fold : meta -> expr -> expr -> binds -> expr
| E_fold_right : meta -> ty -> expr -> expr -> binds -> expr

(* TODO add typing rules for these ...maybe ;) *)
| E_deref : meta -> nat -> expr
| E_let_mut_in : meta -> expr -> binds -> expr
| E_assign : meta -> nat -> expr -> expr
| E_for : meta -> args -> binds -> expr
| E_for_each : meta -> expr -> binds -> expr
| E_while : meta -> expr -> expr -> expr

| E_failwith : meta -> expr -> expr

| E_raw_michelson : meta -> ty -> ty -> list micheline -> expr
| E_inline_michelson : meta -> bool -> list micheline -> args -> expr
| E_global_constant : meta -> ty -> string -> args -> expr
| E_create_contract : meta -> ty -> ty -> binds -> args -> expr

with binds : Set :=
| Binds : meta -> list ty -> expr -> binds

with args : Set :=
| Args_nil : meta -> args
| Args_cons : meta -> expr -> args -> args
.

Scheme expr_ind' := Induction for expr Sort Prop
with binds_ind' := Induction for binds Sort Prop
with args_ind' := Induction for args Sort Prop.
Combined Scheme expr_mutind from expr_ind', binds_ind', args_ind'.

Fixpoint args_length (e : args) : nat :=
  match e with
  | Args_nil _ => O
  | Args_cons _ _ e => S (args_length e)
  end.

Definition binds_length (e : binds) : nat :=
  match e with
  | Binds _ ts _ => length ts
  end.

Local Generalizable Variable l n.

Inductive tuple : list ty -> ty -> Prop :=
| Tuple_nil :
  `{tuple [] (T_unit l)}
| Tuple_one {a} :
  tuple [a] a
| Tuple_cons {a1 a2 az a2z'} :
  `{tuple (a2 :: az) a2z' ->
    tuple (a1 :: a2 :: az) (T_pair l n1 n2 a1 a2z')}
.

Inductive iter_class : ty -> ty -> Prop :=
| Iter_list {a} :
  `{iter_class a (T_list l a)}
| Iter_set {a} :
  `{iter_class a (T_set l a)}
| Iter_map {k v} :
  `{iter_class (T_pair l1 n1 n2 k v) (T_map l2 k v)}
.

Inductive map_class : ty -> ty -> ty -> ty -> Prop :=
| Map_list {a b} :
  `{map_class a b (T_list l1 a) (T_list l2 b)}
| Map_map {k v r} :
  `{map_class (T_pair l1 n1 n2 k v) r (T_map l2 k v) (T_map l3 k r)}
.

Hint Constructors iter_class : michelson.
Hint Constructors map_class : michelson.

Generalizable Variable g. (* TODO *)

Inductive expr_typed : list ty -> expr -> ty -> Prop :=
| E_var_typed {a} :
    `{List.nth_error g n = Some a ->
      expr_typed g (E_var l n) a}
| E_let_in_typed {e1 e2 a b} :
    `{expr_typed g e1 a ->
      binds_typed g e2 [a] b ->
      expr_typed g (E_let_in l e1 e2) b}
| E_tuple_typed {args az t} :
    `{tuple az t ->
      args_typed g args az ->
      expr_typed g (E_tuple l1 args) t}
| E_let_tuple_typed {az azt c e1 e2} :
    `{tuple az azt ->
      expr_typed g e1 azt ->
      binds_typed g e2 az c ->
      expr_typed g (E_let_tuple l3 e1 e2) c}
| E_proj_typed {az azt a e i n} :
    `{tuple az azt ->
      List.nth_error az i = Some a ->
      List.length az = n ->
      expr_typed g e azt ->
      expr_typed g (E_proj l1 e i n) a}
| E_update_typed {a az azt args i n} :
    `{tuple az azt ->
      List.nth_error az i = Some a ->
      List.length az = n ->
      args_typed g args [azt; a] ->
      expr_typed g (E_update l1 args i n) azt}
| E_app_typed {args a b} :
    `{args_typed g args [T_func l1 a b; a] ->
      expr_typed g (E_app l2 args) b}
| E_lam_typed {a b e} :
    `{binds_typed g e [a] b ->
      expr_typed g (E_lam l1 e b)  (T_func l2 a b)}
| E_literal_typed {lit a} :
    `{(* TODO postulate some typing *)
      expr_typed g (E_literal l lit) a}
| E_pair_typed {args a b} :
    `{args_typed g args [a; b] ->
      expr_typed g (E_pair l1 args) (T_pair l2 n1 n2 a b)}
| E_car_typed {e a b} :
    `{expr_typed g e (T_pair l1 n1 n2 a b) ->
      expr_typed g (E_car l2 e) a}
| E_cdr_typed {e a b} :
    `{expr_typed g e (T_pair l1 n1 n2 a b) ->
      expr_typed g (E_cdr l2 e) b}
| E_unit_typed :
    `{expr_typed g (E_unit l1) (T_unit l2)}
| E_left_typed {e a b} :
    `{expr_typed g e a ->
      expr_typed g (E_left l1 b e) (T_or l2 n1 n2 a b)}
| E_right_typed {e a b} :
    `{expr_typed g e b ->
      expr_typed g (E_right l1 a e) (T_or l2 n1 n2 a b)}
| E_if_left_typed {e1 e2 e3 a b c} :
    `{expr_typed g e1 (T_or l1 n1 n2 a b) ->
      binds_typed g e2 [a] c ->
      binds_typed g e3 [b] c ->
      expr_typed g (E_if_left l2 e1 e2 e3) c}
| E_if_bool_typed {e1 e2 e3 c} :
    `{expr_typed g e1 (T_bool l1) ->
      expr_typed g e2 c ->
      expr_typed g e3 c ->
      expr_typed g (E_if_bool l2 e1 e2 e3) c}
| E_if_none_typed {e1 e2 e3 a c} :
    `{expr_typed g e1 (T_option l1 a) ->
      expr_typed g e2 c ->
      binds_typed g e3 [a] c ->
      expr_typed g (E_if_none l2 e1 e2 e3) c}
| E_if_cons_typed {e1 b2 e3 a c} :
    `{expr_typed g e1 (T_list l1 a) ->
      binds_typed g b2 [a; T_list l2 a] c ->
      expr_typed g e3 c ->
      expr_typed g (E_if_cons l3 e1 b2 e3) c}
| E_iter_typed {elem coll e1 e2} :
    `{iter_class elem coll ->
      binds_typed g e1 [elem] (T_unit l1) ->
      expr_typed g e2 coll ->
      expr_typed g (E_iter l2 e1 e2) (T_unit l3)}
| E_map_typed {elem elem' coll coll' e1 e2} :
    `{map_class elem elem' coll coll' ->
      binds_typed g e1 [elem] elem' ->
      expr_typed g e2 coll ->
      expr_typed g (E_map l e1 e2) coll'}
| E_loop_left_typed {a b e1 e2} :
    `{binds_typed g e1 [a] (T_or l1 n1 n2 a b) ->
      expr_typed g e2 a ->
      expr_typed g (E_loop_left l2 e1 b e2) b}
| E_fold_typed {elem coll ret e1 e2 e3} :
    `{iter_class elem coll ->
      expr_typed g e1 ret ->
      expr_typed g e2 coll ->
      binds_typed g e3 [T_pair l1 n1 n2 ret elem] ret ->
      expr_typed g (E_fold l2 e1 e2 e3) ret}
| E_fold_right_typed {elem coll ret e1 e2 e3} :
    `{iter_class elem coll ->
      expr_typed g e1 ret ->
      expr_typed g e2 coll ->
      binds_typed g e3 [T_pair l1 n1 n2 elem ret] ret ->
      expr_typed g (E_fold_right l2 elem e1 e2 e3) ret}
| E_failwith_typed {a b e} :
    `{expr_typed g e a ->
      expr_typed g (E_failwith l1 e) b}
(* E_raw_michelson emits a Michelson lambda directly. It is used for
   [%Michelson ({| ... |} : a -> b)] in LIGO. Probably we should
   delete it and migrate to E_inline_michelson. *)
| E_raw_michelson_typed {code a b} :
    `{(* TODO should postulate some typing *)
      expr_typed g (E_raw_michelson l1 a b code) (T_lambda l2 a b)}
(* E_inline_michelson inlines Michelson code directly, applied to some
   given arguments. It is currently only used for the "predefined
   constants" in predefined.ml, but could be exposed to users
   someday. TODO we should probably have a [ty] in the syntax for the
   return type [b], since it cannot be inferred? *)
| E_inline_michelson_typed {code args az b p} :
    `{args_typed g args az ->
      (* TODO should postulate some typing *)
      expr_typed g (E_inline_michelson l1 p code args) b}
(* E_global_constant is for Tezos "global constants". It is very
   similar to E_inline_michelson, but accepts the string hash of a
   Tezos "global constant" in place of the Michelson code. *)
| E_global_constant_typed {az b hash args} :
    `{args_typed g args az ->
      (* TODO should postulate some typing *)
      expr_typed g (E_global_constant l1 b hash args) b}
(* E_create_contract only exists here because it seemed easiest to put
   it here.

   It is not easy to handle in an earlier pass using
   E_inline_michelson, because that earlier pass would need access to
   the later passes of the compiler, in order to compile, optimize,
   and render the given contract into Micheline.

   A previous version of this Coq compiler had a more complicated
   interaction with the predefineds which made it possible to handle
   CREATE_CONTRACT using a predefined, but that was very confusing,
   and didn't work well with the current choices here. *)
| E_create_contract_typed {p s script args} :
    `{binds_typed [] script [T_pair l1 n1 n2 p s] (T_pair l2 n3 n4 (T_operation l3) s) ->
      args_typed g args [T_option l4 (T_key_hash l5); T_mutez l6; s] ->
      expr_typed g (E_create_contract l7 p s script args) (T_pair l8 n5 n6 (T_operation l9) (T_address l10))}
with binds_typed : list ty -> binds -> list ty -> ty -> Prop :=
| Binds_typed {a e ts} :
    `{expr_typed (ts ++ g) e a ->
      binds_typed g (Binds l ts e) ts a}
with args_typed : list ty -> args -> list ty -> Prop :=
| Args_nil_typed :
    `{args_typed g (Args_nil l) []}
| Args_cons_typed {e args d a} :
    `{expr_typed g e a ->
      args_typed g args d ->
      args_typed g (Args_cons l e args) (d ++ [a])}
.

Hint Constructors expr_typed : ligo.
Hint Constructors binds_typed : ligo.
Hint Constructors args_typed : ligo.

(****************************
 *   MINI_C OPTIMIZATIONS   *
 ****************************)

Section Mini_c.

(** * Definitions *)

Local Generalizable Variable m.

(** Shift all variables [i] that equal or above [k] by [n] *)
Fixpoint shift (n : nat) (k : nat) (e : expr) : expr :=
let shift' e := shift n k e in
let shift_binds' b := shift_binds n k b in
let shift_args' args := shift_args n k args in
match e with
| E_var m i
  => E_var m (if le_lt_dec k i then i+n else i)
| E_let_in m e body
  => let e'    := shift n k e in
    let body' := shift_binds n k body in
    E_let_in m e' body'

| E_deref m i
  => E_deref m (if le_lt_dec k i then i+n else i)
| E_let_mut_in m e body
  => let e'    := shift n k e in
    let body' := shift_binds n k body in
    E_let_mut_in m e' body'
| E_assign m i e
  => let e' := shift' e in
     E_assign m (if le_lt_dec k i then i+n else i) e'

| E_tuple m args => E_tuple m (shift_args n k args)
| E_let_tuple m e body
  => let e' := shift n k e in (* e ≡ E_tuple _ *)
    let body' := shift_binds n k body in
    E_let_tuple m e' body'
| E_proj m e i l
  => let e' := shift n k e in
    E_proj m e' i l
| E_update m args i j => E_update m (shift_args n k args) i j
| E_app m args        => E_app m (shift_args n k args)
| E_lam m b t         => E_lam m (shift_binds' b) t

| E_literal m l => E_literal m l
| E_pair m args => E_pair m (shift_args' args)
| E_car m e => E_car m (shift' e)
| E_cdr m e => E_cdr m (shift' e)
| E_unit m => E_unit m
| E_left m t e => E_left m t (shift' e)
| E_right m t e => E_right m t (shift' e)
| E_if_left m e b1 b2 => E_if_left m (shift' e) (shift_binds' b1) (shift_binds' b2)
| E_if_bool m e1 e2 e3 => E_if_bool m (shift' e1) (shift' e2) (shift' e3)
| E_if_none m e1 e2 b => E_if_none m (shift' e1) (shift' e2) (shift_binds' b)
| E_if_cons m e1 b e2 => E_if_cons m (shift' e1) (shift_binds' b) (shift' e2)
| E_iter m b e => E_iter m (shift_binds' b) (shift' e)
| E_map m b e  => E_map m (shift_binds' b) (shift' e)
| E_loop_left m b t e => E_loop_left m (shift_binds' b) t (shift' e)
| E_fold m e1 e2 b => E_fold m (shift' e1) (shift' e2) (shift_binds' b)
| E_fold_right m t e1 e2 b => E_fold_right m t (shift' e1) (shift' e2) (shift_binds' b)
| E_failwith m e => E_failwith m (shift' e)
| E_raw_michelson m t1 t2 l => E_raw_michelson m t1 t2 l
| E_inline_michelson m p l args => E_inline_michelson m p l (shift_args' args)
| E_global_constant m t s args => E_global_constant m t s (shift_args' args)
| E_create_contract m t1 t2 b args => E_create_contract m t1 t2 b (shift_args' args)

| E_for m e1 e2 =>
    let e1' := shift_args' e1 in
    let e2' := shift_binds' e2 in
    E_for m e1' e2'
| E_for_each m e1 e2 =>
    let e1' := shift' e1 in
    let e2' := shift_binds' e2 in
    E_for_each m e1' e2'
| E_while m e1 e2 =>
    let e1' := shift' e1 in
    let e2' := shift' e2 in
    E_while m e1' e2'

end
with shift_args (n : nat) (k : nat) (args : args) :=
match args with
| Args_nil m       => Args_nil m
| Args_cons m x xs => Args_cons m (shift n k x) (shift_args n k xs)
end
with shift_binds (n : nat) (k : nat) (binds : binds) :=
match binds with
| Binds m tys e => Binds m tys (shift n (length tys + k) e)
end
.

Definition shift_env (k : nat) (env : list ty)
           (l : list ty) {Hlen: length l = k} : list ty :=
  l ++ env.
(* Example: shift_env 1 2 [0][1][2][3] --> [0][3][4][5]
   [0] + [ ][ ] + [1][2][3]
   [0] + [1][2] + [3][4][5]  *)

(* TODO update for mutable let *)
Inductive free_in : nat -> expr -> Prop :=
| Free_in_var {k i} : i <> k -> `{free_in k (E_var m i)}
| Free_in_let_in {k e b} :
  free_in k e -> free_in_binds k b ->
  `{free_in k (E_let_in m e b)}

| Free_in_deref {k i} : i <> k -> `{free_in k (E_deref m i)}
| Free_in_let_mut_in {k e b} :
  free_in k e -> free_in_binds k b ->
  `{free_in k (E_let_mut_in m e b)}
| Free_in_assign {k i e} :
  i <> k -> free_in k e -> (* wrong (like many other cases here) *)
  `{free_in k (E_assign m i e)}
| Free_in_for {k e1 e2} :
  free_in_args k e1 -> free_in_binds k e2 ->
  `{free_in k (E_for m e1 e2)}
| Free_in_for_each {k e1 e2} :
  free_in k e1 -> free_in_binds k e2 ->
  `{free_in k (E_for_each m e1 e2)}
| Free_in_while {k e1 e2} :
  free_in k e1 -> free_in k e2 ->
  `{free_in k (E_while m e1 e2)}

| Free_in_tuple {k a} :
  free_in_args k a ->
  `{free_in k (E_tuple m a)}
| Free_in_let_tuple {k e b} :
  free_in k e -> free_in_binds k b ->
  `{free_in k (E_let_tuple m e b)}
| Free_in_proj {k e i l} :
  free_in k e ->
  `{free_in k (E_proj m e i l)}
| Free_in_update {k m args i j} :
  free_in_args k args ->
  `{free_in k (E_update m args i j)}
| Free_in_app {k a} :
  free_in_args k a ->
  `{free_in k (E_app m a)}
| Free_in_lam {k b t} :
  free_in_binds k b ->
  `{free_in k (E_lam m b t)}

| Free_in_literal {k m l} :
  `{free_in k (E_literal m l)}
| Free_in_pair {k m args} :
  free_in_args k args ->
  `{free_in k (E_pair m args)}
| Free_in_car {k m e} :
  free_in k e ->
  `{free_in k (E_car m e)}
| Free_in_cdr {k m e} :
  free_in k e ->
  `{free_in k (E_cdr m e)}
| Free_in_unit {k} :
  `{free_in k (E_unit m)}
| Free_in_left {k m t e} :
  free_in k e ->
  `{free_in k (E_left m t e)}
| Free_in_right {k m t e} :
  free_in k e ->
  `{free_in k (E_right m t e)}
| Free_in_if_left {k m e b1 b2} :
  free_in k e ->  free_in_binds k b1 -> free_in_binds k b2 ->
  `{free_in k (E_if_left m e (b1) (b2))}
| Free_in_if_bool {k m e1 e2 e3} :
  free_in k e1 -> free_in k e2 -> free_in k e3 ->
  `{free_in k (E_if_bool m (e1) (e2) (e3))}
| Free_in_if_none {k m e1 e2 b} :
  free_in k e1 -> free_in k e2 -> free_in_binds k b ->
  `{free_in k (E_if_none m (e1) (e2) (b))}
| Free_in_if_cons {k m e1 b e2} :
  free_in k e1 -> free_in k e2 -> free_in_binds k b ->
  `{free_in k (E_if_cons m (e1) (b) (e2))}
| Free_in_iter {k m b e} :
  free_in k e ->  free_in_binds k b ->
  `{free_in k (E_iter m (b) (e))}
| Free_in_map {k m b e}  :
  free_in k e -> free_in_binds k b ->
  `{free_in k (E_map m (b) (e))}
| Free_in_loop_left {k m b t e} :
  free_in k e -> free_in_binds k b ->
  `{free_in k (E_loop_left m (b) t (e))}
| Free_in_fold {k m e1 e2 b} :
  free_in k e1 -> free_in k e2 -> free_in_binds k b ->
  `{free_in k (E_fold m (e1) (e2) (b))}
| Free_in_fold_right {k m t e1 e2 b} :
  free_in k e1 -> free_in k e2 -> free_in_binds k b ->
  `{free_in k (E_fold_right m t (e1) (e2) (b))}
| Free_in_failwith {k m e} :
  free_in k e ->
  `{free_in k (E_failwith m (e))}
| Free_in_raw_michelson {k m t1 t2 l} :
  `{free_in k (E_raw_michelson m t1 t2 l)}
| Free_in_inline_michelson {k m l args p} :
  free_in_args k args ->
  `{free_in k (E_inline_michelson m p l (args))}
| Free_in_global_constant {k m t s args} :
  free_in_args k args ->
  `{free_in k (E_global_constant m t s (args))}
| Free_in_create_contract {k m t1 t2 b args} :
  free_in_args k args ->
  `{free_in k (E_create_contract m t1 t2 (b) (args))}

with free_in_binds : nat -> binds -> Prop :=
| Free_in_binds {k e tys} :
  free_in (length tys + k) e ->
  `{free_in_binds k (Binds m tys e)}

with free_in_args : nat -> args -> Prop :=
| Free_in_nil {k} : `{free_in_args k (Args_nil m)}
| Free_in_cons {k e args} :
  free_in k e -> free_in_args k args ->
  `{free_in_args k (Args_cons m e args)}
.

Scheme free_in_expr_ind' := Induction for free_in Sort Prop
with free_in_binds_ind' := Induction for free_in_binds Sort Prop
with free_in_args_ind' := Induction for free_in_args Sort Prop.
Combined Scheme free_in_mutind from free_in_expr_ind', free_in_binds_ind', free_in_args_ind'.

(** [body[k:=E]] without shifting down *)
Fixpoint subst (E : expr) (k : nat) (body : expr) : expr :=
let subst' e := subst E k e in
let subst_binds' b := subst_to_binds E k b in
let subst_args' args := subst_to_args E k args in
match body with
| E_var m i
  => if le_lt_dec i k
    then if Nat.eq_dec i k then shift k 0 E else E_var m i
    else E_var m i
| E_let_in m e body => E_let_in m (subst E k e) (subst_to_binds E k body)

| E_deref m i
  (* TODO hmm, this shouldn't happen *)
  => if le_lt_dec i k
    then if Nat.eq_dec i k then shift k 0 E else E_deref m i
    else E_deref m i
| E_let_mut_in m e body => E_let_mut_in m (subst E k e) (subst_to_binds E k body)

| E_assign m i e =>
    (* TODO hmm, we assume that i <> k, otherwise the substitution was invalid! *)
    let e := subst' e in
    E_assign m i e
| E_for m e1 e2 =>
    let e1' := subst_args' e1 in
    let e2' := subst_binds' e2 in
    E_for m e1' e2'
| E_for_each m e1 e2 =>
    let e1' := subst' e1 in
    let e2' := subst_binds' e2 in
    E_for_each m e1' e2'
| E_while m e1 e2 =>
    let e1' := subst' e1 in
    let e2' := subst' e2 in
    E_while m e1' e2'

| E_tuple m args    => E_tuple m (subst_to_args E k args)
| E_let_tuple m e body => E_let_tuple m (subst E k e) (subst_to_binds E k body)
| E_proj m e i l => E_proj m (subst E k e) i l
| E_update m args i j => E_update m (subst_args' args) i j
| E_app m args        => E_app m (subst_args' args)
| E_lam m b t         => E_lam m (subst_binds' b) t

| E_literal m l => E_literal m l
| E_pair m args => E_pair m (subst_args' args)
| E_car m e => E_car m (subst' e)
| E_cdr m e => E_cdr m (subst' e)
| E_unit m => E_unit m
| E_left m t e => E_left m t (subst' e)
| E_right m t e => E_right m t (subst' e)
| E_if_left m e b1 b2 => E_if_left m (subst' e) (subst_binds' b1) (subst_binds' b2)
| E_if_bool m e1 e2 e3 => E_if_bool m (subst' e1) (subst' e2) (subst' e3)
| E_if_none m e1 e2 b => E_if_none m (subst' e1) (subst' e2) (subst_binds' b)
| E_if_cons m e1 b e2 => E_if_cons m (subst' e1) (subst_binds' b) (subst' e2)
| E_iter m b e => E_iter m (subst_binds' b) (subst' e)
| E_map m b e  => E_map m (subst_binds' b) (subst' e)
| E_loop_left m b t e => E_loop_left m (subst_binds' b) t (subst' e)
| E_fold m e1 e2 b => E_fold m (subst' e1) (subst' e2) (subst_binds' b)
| E_fold_right m t e1 e2 b => E_fold_right m t (subst' e1) (subst' e2) (subst_binds' b)
| E_failwith m e => E_failwith m (subst' e)
| E_raw_michelson m t1 t2 l => E_raw_michelson m t1 t2 l
| E_inline_michelson m p l args => E_inline_michelson m p l (subst_args' args)
| E_global_constant m t s args => E_global_constant m t s (subst_args' args)
| E_create_contract m t1 t2 b args => E_create_contract m t1 t2 b (subst_args' args)
end
with subst_to_args (E : expr) (k : nat) (args : args) :=
match args with
| Args_nil m => Args_nil m
| Args_cons m x xs => Args_cons m (subst E k x) (subst_to_args E k xs)
end
with subst_to_binds (E : expr) (k : nat) (binds : binds) :=
match binds with
| Binds m tys e => Binds m tys (subst E (length tys + k) e)
end
.

Notation "x .1" := (proj1 x) (at level 1, left associativity, format "x .1").
Notation "x .2" := (proj2 x) (at level 1, left associativity, format "x .2").

Fixpoint shift_down (k : nat) (e : expr) :=
let shift_down' e := shift_down k e in
let shift_down_binds' b := shift_down_binds k b in
let shift_down_args' args := shift_down_args k args in
match e with
| E_var m i => E_var m (if le_lt_dec k i then pred i else i)
| E_let_in m e b => E_let_in m (shift_down k e) (shift_down_binds k b)

| E_deref m i => E_deref m (if le_lt_dec k i then pred i else i)
| E_let_mut_in m e b => E_let_mut_in m (shift_down k e) (shift_down_binds k b)

| E_assign m i e => E_assign m i (shift_down' e)
| E_for m e1 e2 => E_for m (shift_down_args' e1) (shift_down_binds' e2)
| E_for_each m e1 e2 => E_for_each m (shift_down' e1) (shift_down_binds' e2)
| E_while m e1 e2 => E_while m (shift_down' e1) (shift_down' e2)

| E_tuple m args => E_tuple m (shift_down_args k args)
| E_let_tuple m e b => E_let_tuple m (shift_down k e) (shift_down_binds k b)
| E_proj m e i l => E_proj m (shift_down k e) i l
| E_update m args i j => E_update m (shift_down_args' args) i j
| E_app m args => E_app m (shift_down_args k args)
| E_lam m b t  => E_lam m (shift_down_binds k b) t
| E_literal m l => E_literal m l
| E_pair m args => E_pair m (shift_down_args' args)
| E_car m e => E_car m (shift_down' e)
| E_cdr m e => E_cdr m (shift_down' e)
| E_unit m => E_unit m
| E_left m t e => E_left m t (shift_down' e)
| E_right m t e => E_right m t (shift_down' e)
| E_if_left m e b1 b2 => E_if_left m (shift_down' e) (shift_down_binds' b1) (shift_down_binds' b2)
| E_if_bool m e1 e2 e3 => E_if_bool m (shift_down' e1) (shift_down' e2) (shift_down' e3)
| E_if_none m e1 e2 b => E_if_none m (shift_down' e1) (shift_down' e2) (shift_down_binds' b)
| E_if_cons m e1 b e2 => E_if_cons m (shift_down' e1) (shift_down_binds' b) (shift_down' e2)
| E_iter m b e => E_iter m (shift_down_binds' b) (shift_down' e)
| E_map m b e  => E_map m (shift_down_binds' b) (shift_down' e)
| E_loop_left m b t e => E_loop_left m (shift_down_binds' b) t (shift_down' e)
| E_fold m e1 e2 b => E_fold m (shift_down' e1) (shift_down' e2) (shift_down_binds' b)
| E_fold_right m t e1 e2 b => E_fold_right m t (shift_down' e1) (shift_down' e2) (shift_down_binds' b)
| E_failwith m e => E_failwith m (shift_down' e)
| E_raw_michelson m t1 t2 l => E_raw_michelson m t1 t2 l
| E_inline_michelson m p l args => E_inline_michelson m p l (shift_down_args' args)
| E_global_constant m t s args => E_global_constant m t s (shift_down_args' args)
| E_create_contract m t1 t2 b args => E_create_contract m t1 t2 b (shift_down_args' args)
end
with shift_down_binds (k : nat) (b : binds) :=
match b with
| Binds m tys e => Binds m tys (shift_down (length tys + k) e)
end
with shift_down_args (k : nat) (args : args) :=
match args with
| Args_nil m => Args_nil m
| Args_cons m x xs => Args_cons m (shift_down k x) (shift_down_args k xs)
end
.

(* TODO: Actually we can shift from the [S k], because k is free after subst. *)
Definition subst' e k body := shift_down k (subst (shift 1 0 e) k body).
Definition subst_to_binds' e k body := shift_down_binds k (subst_to_binds (shift 1 0 e) k body).
Definition subst_to_args' e k body := shift_down_args k (subst_to_args (shift 1 0 e) k body).
Transparent subst' subst_to_binds' subst_to_args'.

(** ** Optimizations *)

Context {should_inline : meta -> expr -> expr -> bool}.

(* TODO: try to implement iteration process (maybe we braga method) *)
Fixpoint inline (e : expr) : expr :=
match e with
| E_let_in m1 E (Binds m2 [t] e) =>
  (* TODO: optimize *)
  if should_inline m1 E e
  then shift_down 0 (subst (shift 1 0 E) 0 e)
  else
    let E' := inline E in
    let e' := inline e in
    E_let_in m1 E' (Binds m2 [t] e')
| E_let_in m1 E (Binds m2 ts e) =>
  E_let_in m1 (inline E) (Binds m2 ts (inline e))
| E_let_tuple m1 E (Binds m2 tys e) =>
  E_let_tuple m1 (inline E) (Binds m2 tys (inline e))
| E_var m i => E_var m i

| E_deref m i => E_deref m i
(* in some cases we could inline a letmut, but we won't for now *)
| E_let_mut_in m1 E (Binds m2 ts e) =>
  E_let_mut_in m1 (inline E) (Binds m2 ts (inline e))
| E_assign m i e => E_assign m i (inline e)
| E_for m e1 e2 => E_for m (inline_in_args e1) (inline_in_binds e2)
| E_for_each m e1 e2 => E_for_each m (inline e1) (inline_in_binds e2)
| E_while m e1 e2 => E_while m (inline e1) (inline e2)

| E_tuple m args => E_tuple m (inline_in_args args)
| E_proj m e i l => E_proj m (inline e) i l
| E_update m args i j => E_update m (inline_in_args args) i j
| E_app m args => E_app m (inline_in_args args)
| E_lam m b t  => E_lam m (inline_in_binds b) t
| E_literal m l => E_literal m l
| E_pair m args => E_pair m (inline_in_args args)
| E_car m e => E_car m (inline e)
| E_cdr m e => E_cdr m (inline e)
| E_unit m => E_unit m
| E_left m t e => E_left m t (inline e)
| E_right m t e => E_right m t (inline e)
| E_if_left m e b1 b2 => E_if_left m (inline e) (inline_in_binds b1) (inline_in_binds b2)
| E_if_bool m e1 e2 e3 => E_if_bool m (inline e1) (inline e2) (inline e3)
| E_if_none m e1 e2 b => E_if_none m (inline e1) (inline e2) (inline_in_binds b)
| E_if_cons m e1 b e2 => E_if_cons m (inline e1) (inline_in_binds b) (inline e2)
| E_iter m b e => E_iter m (inline_in_binds b) (inline e)
| E_map m b e  => E_map m (inline_in_binds b) (inline e)
| E_loop_left m b t e => E_loop_left m (inline_in_binds b) t (inline e)
| E_fold m e1 e2 b => E_fold m (inline e1) (inline e2) (inline_in_binds b)
| E_fold_right m t e1 e2 b => E_fold_right m t (inline e1) (inline e2) (inline_in_binds b)
| E_failwith m e => E_failwith m (inline e)
| E_raw_michelson m t1 t2 l => E_raw_michelson m t1 t2 l
| E_inline_michelson m p l args => E_inline_michelson m p l (inline_in_args args)
| E_global_constant m t s args => E_global_constant m t s (inline_in_args args)
| E_create_contract m t1 t2 b args => E_create_contract m t1 t2 (inline_in_binds b) (inline_in_args args)
end
with inline_in_binds (b : binds) :=
match b with
| Binds m tys e => Binds m tys (inline e)
end
with inline_in_args (args : args) :=
match args with
| Args_nil m => Args_nil m
| Args_cons m x xs => Args_cons m (inline x) (inline_in_args xs)
end
.

(** * General Tactics

     TODO: We should move it into separate file *)

Ltac destruct_exists :=
    repeat
      match goal with
      | H : exists _, _ |- _ => destruct H as [?t ?H]
      end.

Ltac clear_ctx :=
  repeat match goal with
  | H : ?T |- _ => lazymatch type of T with
                 | Prop => fail
                 | ?TT => clear H
                 end
  end.
Tactic Notation "dep_ind'" ident(X1) := dependent induction X1.
Tactic Notation "dep_ind" ident(X1)
  := clear_ctx; dep_ind' X1.
Tactic Notation "dep_ind" ident(X1) "," ident(X2)
  := clear_ctx; dep_ind' X1; dep_ind' X2.
Tactic Notation "dep_ind" ident(X1) "," ident(X2) "," ident(X3)
  := clear_ctx; dep_ind' X1; dep_ind' X2; dep_ind' X3.

(* split on depth [N] (not optimal) *)
Ltac splits N :=
  match N with
  | O => idtac
  | S ?N' => tryif split then splits N' else idtac
  end.

(* Inspired by https://softwarefoundations.cis.upenn.edu/plf-current/LibTactics.html *)

Tactic Notation "gen" ident(X1) := generalize dependent X1.
Tactic Notation "gen" ident(X1) ident(X2) := gen X1; gen X2.
Tactic Notation "gen" ident(X1) ident(X2) ident(X3) := gen X1; gen X2; gen X3.
Tactic Notation "gen" ident(X1) ident(X2) ident(X3) ident(X4) := gen X1; gen X2; gen X3; gen X4.

Tactic Notation "admit_rewrite" constr(T) :=
  let M := fresh "TEMP" in
  assert (M: T) by admit; repeat rewrite M in *; clear M.

Tactic Notation "assert_rewrite" constr(E) :=
  let EQ := fresh "TEMP" in
  assert (EQ : E); [> idtac | rewrite EQ; clear EQ].
Tactic Notation "assert_rewrite" constr(E) "by" tactic(tac):=
  let EQ := fresh "TEMP" in
  assert (EQ : E); [> tac | (rewrite EQ; clear EQ)].
Tactic Notation "assert_rewrite" constr(E) "at" integer(n) "by" tactic(tac):=
  let EQ := fresh "TEMP" in
  assert (EQ : E); [> tac | (rewrite EQ at n; clear EQ)].

(** check that [e] is head of type [t] *)
Ltac head_constructor' e t :=
  let et := type of e in
  match et with
  | t => head_constructor e
  | t _ => head_constructor e
  | t _ _ => head_constructor e
  | t _ _ _ => head_constructor e
  | t _ _ _ _ => head_constructor e
  end.

Local Set Warnings "-cast-in-pattern". (* TODO ? *)
Ltac invert_inductive t :=
  match goal with
  | [H : ?p (?e : t) |- _ ]
    => first [ head_constructor' e t ]; invert H
  | [H : ?p ?e1 ?e2 |- _ ]
    => first [ head_constructor' e1 t | head_constructor' e2 t ]; invert H
  | [H : ?p ?e1 ?e2 ?e3 |- _]
    => first [ head_constructor' e1 t | head_constructor' e2 t
            | head_constructor' e3 t ]; invert H
  | [H : ?p ?e1 ?e2 ?e3 ?e4 |- _]
    => first [ head_constructor' e1 t | head_constructor' e2 t
            | head_constructor' e3 t | head_constructor' e4 t ]; invert H
  | [H : ?p ?e1 ?e2 ?e3 ?e4 ?e5 |- _]
    => first [ head_constructor' e1 t | head_constructor' e2 t
            | head_constructor' e3 t | head_constructor' e4 t
            | head_constructor' e5 t ]; invert H
  end.

Hint Extern 1 => match goal with | H : _ /\ _ |- _ => destruct H end : core.

(** * General properties of de Bruijn operations and predicates *)

(** ** Auxiliary tactics *)

Ltac invert_expr := repeat (invert_inductive expr || invert_inductive binds || invert_inductive args).

Lemma fold_subst :
  (forall e1 e2 k, shift_down k (subst (shift 1 0 e1) k e2) = subst' e1 k e2)
  /\ (forall e1 e2 k, shift_down_binds k (subst_to_binds (shift 1 0 e1) k e2) = subst_to_binds' e1 k e2)
  /\ (forall e1 e2 k, shift_down_args k (subst_to_args (shift 1 0 e1) k e2) = subst_to_args' e1 k e2).
Proof.
  splits 2; reflexivity.
Qed.

(* For convenient reading goal after simplification through constructors *)
Ltac fold_subst := repeat (rewrite fold_subst.1 || rewrite fold_subst.2.1 || rewrite fold_subst.2.2).

(** ** Extra stdlib proofs *)

Lemma add_0_r {n} : n + 0 = n.
Proof. induction n; [> reflexivity | simpl; f_equal; assumption]. Qed.

(* Lemma nth_error_app2 l l' n : length l <= n -> *)
(*     nth_error (l++l') n = nth_error l' (n-length l). *)

Lemma nth_error_hd : forall {A : Type} {l : list A} {a n},
      n <> 0 ->
      nth_error l (pred n) = nth_error (a :: l) n.
Proof.
  induction n.
  - intro H; exfalso; exact (H eq_refl).
  - intro H; simpl; reflexivity.
Qed.

(** ** Simplification lemmas about de Bruijn operations *)

Lemma shift0_simpl :
  (forall {e k}, shift 0 k e = e)
  /\ (forall {b k}, shift_binds 0 k b = b)
  /\ (forall {args k}, shift_args 0 k args = args).
Proof.
  apply expr_mutind;
  simpl; intros; try destruct (le_lt_dec k n); try f_equal; try lia; auto.
Qed.

Lemma shift_extend :
  (forall {e l k n}, (forall i, k <= i < k + l -> free_in i e) ->
                shift n (k + l) e = shift n k e)
  /\ (forall {b l k n}, (forall i, k <= i < k + l -> free_in_binds i b) ->
                  shift_binds n (k + l) b = shift_binds n k b)
  /\ (forall {a l k n}, (forall i, k <= i < k + l -> free_in_args i a) ->
                  shift_args n (k + l) a = shift_args n k a).
Proof.
  apply expr_mutind.
  all: intros; simpl; f_equal.
  all: try (apply H || apply H1 || apply H2 || apply H0).
  all: try intros i Hi; try (
      (specialize (H1 i Hi); inversion_clear H1; assumption)
    || (specialize (H0 i Hi); inversion_clear H0; assumption)
    || (specialize (H2 i Hi); inversion_clear H2; assumption)).
  - destruct (le_lt_dec k n); destruct (le_lt_dec (k + l) n); try lia.
    + specialize (H n (conj l0 l1)).
      inversion_clear H.
      exfalso; apply H0; reflexivity.
  - destruct (le_lt_dec k n); destruct (le_lt_dec (k + l) n); try lia.
    + specialize (H n (conj l0 l1)).
      inversion_clear H.
      exfalso; apply H0; reflexivity.
  - admit. (* E_assign *)
  - rewrite Nat.add_assoc. apply H. intros i Hi.
    assert (k <= i - length l < k + l0) as H' by lia.
    specialize (H0 (i - length l) H'); inversion_clear H0.
    assert (length l + (i - length l) = i) as Hsimp by lia.
    rewrite <- Hsimp.
    assumption.
Admitted.

(* More handy version of previous one *)
Lemma shift_specialize :
  forall {k1 k2 e n}, k1 <= k2 -> (forall i, k1 <= i < k2 -> free_in i e) ->
                 shift n k1 e = shift n k2 e.
Proof.
  intros.
  destruct (Nat.le_exists_sub _ _ H) as [k' [? _]].
  rewrite H1 in *.
  rewrite Nat.add_comm. rewrite shift_extend.1.
  reflexivity. rewrite Nat.add_comm. assumption.
Qed.

Lemma shift_down_extend :
  (forall {e l k}, (forall i, k <= i < k + l -> free_in i e) ->
                shift_down (k + l) e = shift_down k e)
  /\ (forall {b l k}, (forall i, k <= i < k + l -> free_in_binds i b) ->
                  shift_down_binds (k + l) b = shift_down_binds k b)
  /\ (forall {a l k}, (forall i, k <= i < k + l -> free_in_args i a) ->
                  shift_down_args (k + l) a = shift_down_args k a).
Proof.
  apply expr_mutind.
  all: intros; simpl; f_equal.
  all: try (apply H || apply H1 || apply H2 || apply H0).
  all: try intros i Hi; try (
      (specialize (H1 i Hi); inversion_clear H1; assumption)
    || (specialize (H0 i Hi); inversion_clear H0; assumption)
    || (specialize (H2 i Hi); inversion_clear H2; assumption)).
  - destruct (le_lt_dec k n); destruct (le_lt_dec (k + l) n); try lia.
    + specialize (H n (conj l0 l1)).
      inversion_clear H.
      exfalso; apply H0; reflexivity.
  - admit. (* E_deref *)
  - rewrite Nat.add_assoc. apply H. intros i Hi.
    assert (k <= i - length l < k + l0) as H' by lia.
    specialize (H0 (i - length l) H'); inversion_clear H0.
    assert (length l + (i - length l) = i) as Hsimp by lia.
    rewrite <- Hsimp.
    assumption.
Admitted.

(* More handy version of previous one *)
Lemma shift_down_specialize :
  forall {k1 k2 e}, k1 <= k2 -> (forall i, k1 <= i < k2 -> free_in i e) ->
                 shift_down k1 e = shift_down k2 e.
Proof.
  intros.
  destruct (Nat.le_exists_sub _ _ H) as [k' [? _]].
  rewrite H1 in *.
  rewrite Nat.add_comm. rewrite shift_down_extend.1.
  reflexivity. rewrite Nat.add_comm. assumption.
Qed.

Lemma shift_shift_simpl :
  (forall {e k n1 n2}, shift n1 (k + n2) (shift n2 k e) = shift (n1 + n2) k e)
  /\ (forall {b k n1 n2}, shift_binds n1 (k + n2) (shift_binds n2 k b) = shift_binds (n1 + n2) k b)
  /\ (forall {a k n1 n2}, shift_args n1 (k + n2) (shift_args n2 k a) = shift_args (n1 + n2) k a).
Proof.
 apply expr_mutind.
 all: intros; simpl.
 all: try congruence.
 - destruct (le_lt_dec k n).
   + destruct (le_lt_dec (k + n2) (n + n2)); [> f_equal| exfalso]; lia.
   + destruct (le_lt_dec (k + n2) n); [> exfalso; lia | reflexivity].
 - admit. (* E_deref *)
 - admit. (* E_assign *)
 - apply f_equal.
   rewrite <- H, Nat.add_assoc.
   reflexivity.
Admitted.

Lemma shift_down_shift_simpl :
  (forall {e k}, shift_down (S k) (shift 1 k e) = e)
  /\ (forall {b k}, shift_down_binds (S k) (shift_binds 1 k b) = b)
  /\ (forall {args k}, shift_down_args (S k) (shift_args 1 k args) = args).
Proof.
  apply expr_mutind; intros.
  1: unfold shift, shift_down; destruct (le_lt_dec k n); f_equal;
       destruct le_lt_dec; lia.
  24: unfold shift, shift_down; destruct (le_lt_dec k n); f_equal;
       destruct le_lt_dec; lia.
  all: simpl; f_equal; trivial.
  admit. (* TODO hmm, what happened here *)
  specialize (H (length l + k)).
  rewrite <- H at 2.
  rewrite Nat.add_succ_r; reflexivity.
Admitted.

Create HintDb expr_simpl_hints.
Ltac simpl_expr := autorewrite with expr_simpl_hints.

Hint Rewrite ->
     shift0_simpl.1 shift0_simpl.2.1 shift0_simpl.2.2
     shift_down_shift_simpl.1 shift_down_shift_simpl.2.1 shift_down_shift_simpl.2.2
  : expr_simpl_hints.

(** ** Lemmas about [free_in] predicate *)

Lemma free_in_shifted0 :
  (forall {e k n}, 0 < n -> free_in k (shift n k e))
  /\ (forall {b k n}, 0 < n -> free_in_binds k (shift_binds n k b))
  /\ (forall {args k n}, 0 < n -> free_in_args k (shift_args n k args)).
Proof.
  apply expr_mutind.
  all: intros; simpl.
  all: try (econstructor; eauto).
  - destruct le_lt_dec; lia.
  - destruct le_lt_dec; lia.
  - destruct le_lt_dec; lia.
Qed.

Lemma free_in_shifted :
  (forall {e k n i}, k <= i < k + n -> free_in i (shift n k e))
  /\ (forall {b k n i}, k <= i < k + n -> free_in_binds i (shift_binds n k b))
  /\ (forall {args k n i}, k <= i < k + n -> free_in_args i (shift_args n k args)).
Proof.
  apply expr_mutind.
  all: intros; simpl.
  all: try econstructor.
  all: try ((apply H; eauto) || (apply H0; eauto) || (apply H1; eauto)).
  - destruct le_lt_dec; lia.
  - destruct le_lt_dec; lia.
  - destruct le_lt_dec; lia.
  - lia.
Qed.

Lemma free_after_shift :
  (forall {e k i n}, k <= i -> free_in i e <-> free_in (i + n) (shift n k e))
  /\ (forall {b k i n}, k <= i -> free_in_binds i b <-> free_in_binds (i + n) (shift_binds n k b))
  /\ (forall {args k i n}, k <= i -> free_in_args i args <-> free_in_args (i + n) (shift_args n k args)).
Proof.
  apply expr_mutind.
  all: split; simpl.
  all: intro H'; inversion_clear H'; simpl; constructor.
  all: try (rewrite <-H0; eauto); try (rewrite ->H0; eauto).
  all: try (rewrite <-H ; eauto); try (rewrite ->H; eauto).
  all: try (rewrite <-H1; eauto); try (rewrite ->H1; eauto).
  all: try (destruct (le_lt_dec k n); lia).
  - rewrite Nat.add_assoc. rewrite <- H. apply H1. lia.
  - rewrite <-(H _ _ n) by lia.
    rewrite Nat.add_assoc in H1.
    rewrite <-H in H1 by lia.
    assumption.
Qed.

Lemma helper1 {e k n} : free_in k (shift k 0 e) -> free_in (n + k) (shift (n + k) 0 e).
Proof.
  intro H.
  rewrite <- (@free_after_shift.1 _ 0 0 k) in H by reflexivity.
  rewrite -> (@free_after_shift.1 _ 0 0 (n + k)) in H by reflexivity.
  simpl in H.
  exact H.
Qed.

Lemma free_after_subst :
  (forall {e : expr}  {E : expr} {k : nat},
      free_in k (shift k 0 E) ->
      free_in k (subst E k e)) /\
  (forall {b : binds} {E : expr} {k : nat},
      free_in (binds_length b + k) (shift (binds_length b + k) 0 E) ->
      free_in_binds k (subst_to_binds E k b)) /\
  (forall {a : args}  {E : expr} {k : nat},
      free_in k (shift k 0 E) ->
      free_in_args k (subst_to_args E k a)).
Proof.
  apply expr_mutind; intros.
  all: try (simpl; constructor; auto).
  all: try ((apply H0 || apply H || apply H1); apply helper1; assumption).
  -  simpl; destruct (le_lt_dec n k); [> destruct (Nat.eq_dec n k) |].
    + assumption. (* TODO *)
    + constructor; assumption.
    + constructor; lia.
  - admit. (* E_deref *)
  - admit. (* ??? *)
Admitted.

Lemma shift_free_simpl :
  (forall {e k n} (HF: forall i, k <= i -> free_in i e), shift n k e = e) /\
  (forall {e k n} (HF: forall i, k <= i -> free_in_binds i e), shift_binds n k e = e) /\
  (forall {e k n} (HF: forall i, k <= i -> free_in_args i e), shift_args n k e = e).
Proof.
  apply expr_mutind; simpl; intros.
  all: try solve [ f_equal; (apply H || apply H0 || apply H1); intros i H';
                   specialize (HF i H'); invert_expr; auto].
  - destruct le_lt_dec; try reflexivity.
    specialize (HF _ l). invert HF. lia.
  - admit. (* E_deref *)
  - admit. (* E_assign *)
  - f_equal.
    apply (H). intros i H'.
    assert (k <= i - length l) by lia.
    specialize (HF (i - length l) H0). invert_expr.
    rewrite le_plus_minus_r in H3 by lia.
    assumption.
Admitted.

Lemma subst_free_simpl :
  (forall {k e2} {HF: free_in k e2} {e1}, subst e1 k e2 = e2)
  /\ (forall {k e2} {HF: free_in_binds k e2} {e1}, subst_to_binds e1 k e2 = e2)
  /\ (forall {k e2} {HF: free_in_args k e2} {e1}, subst_to_args e1 k e2 = e2).
Proof.
  apply free_in_mutind; simpl; intros.
  all: try f_equal; auto.
  - destruct le_lt_dec; try destruct Nat.eq_dec; lia || reflexivity.
  - admit. (* E_deref *)
Admitted.

(** ** Auxiliary forms of simplification lemmas *)

(* TODO: generalize for binds and args *)
Lemma shift_shift_simpl' : forall {e n1 n2},
    shift n1 0 (shift n2 0 e) = shift (n1 + n2) 0 e.
Proof.
  intros.
  rewrite <-(shift_extend.1 _ n2) by (apply free_in_shifted; lia).
  rewrite shift_shift_simpl.1. reflexivity.
Qed.

Lemma shift_down_shift_simpl' :
  (forall {e k}, shift_down k (shift 1 k e) = e)
  /\ (forall {b k}, shift_down_binds k (shift_binds 1 k b) = b)
  /\ (forall {args k}, shift_down_args k (shift_args 1 k args) = args).
Proof.
  splits 2; intros.
  all: erewrite <- (@shift_down_extend.1 _ 1)
     || erewrite <- (@shift_down_extend.2.1 _ 1)
     || erewrite <- (@shift_down_extend.2.2 _ 1).
  all: try (rewrite Nat.add_1_r; apply shift_down_shift_simpl).
  all: intros; eapply free_in_shifted; assumption.
Qed.

Hint Rewrite ->
     @shift_shift_simpl'
     shift_down_shift_simpl'.1 shift_down_shift_simpl'.2.1 shift_down_shift_simpl'.2.2
  : expr_simpl_hints.

Lemma shift_down_shift_simpl''' :
  forall {e k1 k2}, shift_down k2 (shift (k2 + S k1) 0 e) = (shift (k2 + k1) 0 e).
Proof.
  intros.
  assert_rewrite (k2 + S k1 = 1 + (k2 + k1)) by lia.
  rewrite <-(shift_shift_simpl.1).
  assert_rewrite (0 + (k2 + k1) = k2 + k1) by lia.
  rewrite shift_extend.1 by (intros; apply free_in_shifted; lia).
  rewrite shift_down_shift_simpl'.1.
  reflexivity.
Qed.

Lemma shift_down_shift_simpl'' :
  forall {e k}, shift_down k (shift k 0 (shift 1 0 e)) = shift k 0 e.
Proof.
  intros e k.
  destruct k; [> simpl_expr; reflexivity |].
  rewrite (@shift_specialize 0 1); try lia || (intros; apply free_in_shifted; lia).
  assert_rewrite (1 = 0 + 1) at 1 by lia.
  rewrite shift_shift_simpl.1.
  rewrite shift_down_shift_simpl'''.
  assert_rewrite (S k + 0 = S k) by lia.
  reflexivity.
Qed.

Hint Rewrite ->
     @shift_down_shift_simpl'' @shift_down_shift_simpl''' : expr_simpl_hints.

(** * Type preservation theorems *)

Lemma shift_type_preservation :
  (forall {e l gl gr t}, expr_typed (gl ++ gr) e t ->
                    expr_typed (gl ++ l ++ gr) (shift (length l) (length gl) e) t)
  /\ (forall {b l gl gr t ts}, binds_typed (gl ++ gr) b ts t ->
                         binds_typed (gl ++ l ++ gr) (shift_binds (length l) (length gl) b) ts t)
  /\ (forall {args l gl gr ts} , args_typed (gl ++ gr) args ts ->
                           args_typed (gl ++ l ++ gr) (shift_args (length l) (length gl) args) ts).
Proof.
  apply expr_mutind; intros; simpl.
  all: invert_expr.
  all: try (econstructor; try eassumption; try reflexivity); eauto.
  - destruct (le_lt_dec _ _).
    + rewrite nth_error_app2 by lia.
      rewrite nth_error_app2 by lia.
      rewrite nth_error_app2 in H4 by lia.
      rewrite <-H4; f_equal; lia.
    + rewrite nth_error_app1 by lia.
      rewrite nth_error_app1 in H4 by lia.
      exact H4.
  - specialize (H l0 (ts ++ gl)%list gr t).
    do 2 rewrite <-app_assoc in H.
    rewrite app_length in H.
    exact (H H7).
Qed.

Lemma shift_type_preservation0 :
  (forall {e l g t}, expr_typed g e t ->
                expr_typed (l ++ g) (shift (length l) 0 e) t)
  /\ (forall {b l g t ts}, binds_typed g b ts t ->
                       binds_typed (l ++ g) (shift_binds (length l) 0 b) ts t)
  /\ (forall {args l g ts} , args_typed g args ts ->
                         args_typed (l ++ g) (shift_args (length l) 0 args) ts).
Proof.
  split; [> | split ]; intros x l.
  - epose (shift_type_preservation.1 x l []) as H; simpl in H; exact H.
  - epose (shift_type_preservation.2.1 x l []) as H; simpl in H; exact H.
  - epose (shift_type_preservation.2.2 x l []) as H; simpl in H; exact H.
Qed.

(* without shifting down *)
Theorem subst_type_preservation :
  (forall {expr g E t tE k}, expr_typed g (shift k 0 E) tE ->
                        expr_typed g expr t ->
                        nth_error g k = Some tE ->
    expr_typed g (subst E k expr) t) /\
  (forall {b g E t ts tE k}, expr_typed g (shift k 0 E) tE ->
                        binds_typed g b ts t ->
                        nth_error g k = Some tE ->
    binds_typed g (subst_to_binds E k b) ts t) /\
  (forall {args g E ts tE k}, expr_typed g (shift k 0 E) tE ->
                         args_typed g args ts ->
                         nth_error g k = Some tE ->
    args_typed g (subst_to_args E k args) ts).
Proof.
  apply expr_mutind; intros; simpl.
  all: try invert_expr.
  all: try econstructor; try eassumption; try reflexivity.
  all: try ((eapply H || eapply H0 || eapply H1); try econstructor; eassumption).
  - destruct (le_lt_dec n k); destruct (Nat.eq_dec n k);
      try (constructor; assumption).
    + subst k. rewrite H1 in H6; invert H6. assumption.
  - eapply H; try eassumption.
    + rewrite <- shift_shift_simpl.1.
      rewrite shift_extend.1.
      apply shift_type_preservation0.1; eassumption.
      intro i; exact (@free_in_shifted.1 E 0 k i).
    + rewrite nth_error_app2 by lia.
      rewrite <-H2; f_equal; lia.
Qed.

Lemma shift_down_type_preservasion :
  (forall {e a gl gr t}, free_in (length gl) e ->
                  expr_typed (gl ++ a :: gr) e t ->
                  expr_typed (gl ++ gr) (shift_down (length gl) e) t)
  /\ (forall {b a gl gr t ts}, free_in_binds (length gl) b ->
                    binds_typed (gl ++ a :: gr) b ts t ->
                    binds_typed (gl ++ gr) (shift_down_binds (length gl) b) ts t)
  /\ (forall {args a gl gr ts}, free_in_args (length gl) args ->
                     args_typed (gl ++ a :: gr) args ts ->
                     args_typed (gl ++ gr) (shift_down_args (length gl) args) ts).
Proof.
  apply expr_mutind; intros; simpl.
  all: invert_expr.
  all: try econstructor; try eassumption; try reflexivity.
  all: try ((try eapply H; try eapply H0); eauto).
  all: try (assumption).
  - destruct (le_lt_dec _ _).
    + rewrite nth_error_app2 by lia.
      rewrite nth_error_app2 in H5 by lia.
      rewrite <-H5.
      assert (pred n - length gl = pred (n - length gl)) as h1 by lia.
      rewrite h1; clear h1.
      erewrite nth_error_hd by lia.
      reflexivity.
   + rewrite nth_error_app1 in * by lia.
     assumption.
  - epose (H a (ts ++ gl)%list gr t) as H'.
    do 2 rewrite <-app_assoc in H'.
    rewrite app_length in H'.
    exact (H' H4 H8).
Qed.

Lemma shift_down_type_preservasion0 :
  (forall {e a g t}, free_in 0 e ->
                  expr_typed (a :: g) e t ->
                  expr_typed g (shift_down 0 e) t)
  /\ (forall {b a g t ts}, free_in_binds 0 b ->
                    binds_typed (a :: g) b ts t ->
                    binds_typed g (shift_down_binds 0 b) ts t)
  /\ (forall {args a g ts}, free_in_args 0 args ->
                     args_typed (a :: g) args ts ->
                     args_typed g (shift_down_args 0 args) ts).
Proof.
  split; [> | split]; intros x a.
  - pose (shift_down_type_preservasion.1 x a []) as H; simpl in H; exact H.
  - pose (shift_down_type_preservasion.2.1 x a []) as H; simpl in H; exact H.
  - pose (shift_down_type_preservasion.2.2 x a []) as H; simpl in H; exact H.
Qed.

Lemma subst'0_type_preservation :
  forall {e1 e2 g t1 t2}, expr_typed g e1 t1 ->
                     expr_typed ([t1] ++ g) e2 t2 ->
                     expr_typed g (subst' e1 0 e2) t2.
Proof.
  intros; unfold subst'.
  eapply (shift_down_type_preservasion0.1).
  - eapply free_after_subst; rewrite shift0_simpl.1; eapply free_in_shifted0.1; lia.
  - eapply (subst_type_preservation.1).
    + rewrite shift0_simpl.1.
      epose (@shift_type_preservation.1 e1 [t1] [] g t1) as H1.
      simpl in H1. apply H1, H.
    + simpl in H0; apply H0.
    + reflexivity.
Qed.

Theorem inline_type_preservasion :
  (forall {e t g}, expr_typed g e t -> expr_typed g (inline e) t)
  /\ (forall {b t ts g}, binds_typed g b ts t -> binds_typed g (inline_in_binds b) ts t)
  /\ (forall {args ts g}, args_typed g args ts -> args_typed g (inline_in_args args) ts).
Proof.
  apply expr_mutind;
    simpl; intros; try assumption.
  all: try (try destruct b; inversion_clear H0; econstructor; eauto).
  all: try (try destruct b; inversion_clear H1; econstructor; eauto).
  all: try (try destruct b; inversion_clear H2; econstructor; eauto).
  - (* E_let_in *)
    destruct b.
    destruct (should_inline m e e0).
    2: { destruct l; [>| destruct l];
         inversion_clear H1; econstructor; eauto. }
    inversion_clear H1.
    inversion H3; subst.
    rename e into E'; rename e0 into e'.
    eassert (expr_typed (a :: g) (subst (shift 1 0 E') 0 e') t) as H'.
    { eapply subst_type_preservation.1.
      - rewrite shift0_simpl.1.
        assert (@shift_env 1 g [a] eq_refl = a :: g) by reflexivity.
        rewrite <- H1.
        eapply shift_type_preservation0.1.
        apply H2.
      - simpl in H0.
        enough (binds_typed g (inline_in_binds (Binds m0 [a] e')) [a] t).
        * inversion_clear H1; subst; simpl in *; assumption.
        * apply H0.
          constructor; assumption.
      - simpl. reflexivity.
    }
    eapply shift_down_type_preservasion0.
    apply (free_after_subst.1).
    rewrite shift0_simpl.1.
    apply (free_in_shifted0); lia.
    apply H'.
Qed.

Local Generalizable No Variables.

End Mini_c.

(*************
 * Michelson *
 *************)

Reserved Notation "'prog'".

Inductive instr : Set :=
| I_RAW : meta -> nat -> list micheline -> instr

| I_SEQ : meta -> prog -> instr
| I_DIP : meta -> prog -> instr
| I_DIG : meta -> nat -> instr
| I_DUG : meta -> nat -> instr
| I_DUP : meta -> nat -> instr
| I_DROP : meta -> nat -> instr
| I_SWAP : meta -> instr

| I_UNIT : meta -> instr
| I_TRUE : meta -> instr (* fictional version of PUSH bool True, for impl of E_while... *)

| I_LEFT : meta -> ty -> instr
| I_RIGHT : meta -> ty -> instr
| I_IF_LEFT : meta -> prog -> prog -> instr

| I_PAIR : meta -> nat -> instr
| I_UNPAIR : meta -> nat -> instr
| I_GET : meta -> nat -> instr
| I_UPDATE : meta -> nat -> instr
| I_CAR : meta -> instr
| I_CDR : meta -> instr

| I_IF : meta -> prog -> prog -> instr

| I_IF_NONE : meta -> prog -> prog -> instr

| I_NIL : meta -> ty -> instr
| I_CONS : meta -> instr
| I_IF_CONS : meta -> prog -> prog -> instr

| I_FUNC : meta -> list ty -> ty -> ty -> list bool -> list bool -> prog -> instr (* VERY FICTION *)
| I_LAMBDA : meta -> ty -> ty -> prog -> instr
| I_EXEC : meta -> instr (* func or lambda *)
| I_APPLY_LAMBDA : meta -> ty -> instr (* FICTION (APPLY but with a type arg) *)

| I_LOOP : meta -> prog -> instr
| I_LOOP_LEFT : meta -> prog -> instr

| I_FAILWITH : meta -> instr

| I_ITER : meta -> prog -> instr
| I_MAP : meta -> prog -> instr

(* convenient fictional instruction for implementing E_for (arithmetic
   progression loops) without exposing arithmetic here right now... *)
| I_FOR : meta -> prog -> instr

| I_CREATE_CONTRACT : meta -> ty -> ty -> prog -> instr
where
"'prog'" := (list instr).

Inductive packable : ty -> Prop :=
| Packable_unit : `{packable (T_unit l)}
| Packable_pair {a b} :
  `{packable a -> packable b ->
    packable (T_pair l n1 n2 a b)}
| Packable_or {a b} :
    `{packable a -> packable b ->
      packable (T_or l n1 n2 a b)}
(* T_func is not packable, T_lambda is: *)
| Packable_lambda {a b} :
  `{packable (T_lambda l a b)}
| Packable_int :
  `{packable (T_int l)}
| Packable_nat :
  `{packable (T_nat l)}
| Packable_mutez :
  `{packable (T_mutez l)}
| Packable_bool :
  `{packable (T_bool l)}
| Packable_bytes :
  `{packable (T_bytes l)}
(* TODO *)
.

Hint Constructors packable : michelson.

Inductive comparable : ty -> Prop :=
| Comparable_int : `{comparable (T_int l)}
| Comparable_nat : `{comparable (T_nat l)}
| Comparable_mutez : `{comparable (T_mutez l)}
(* TODO *)
.

Hint Constructors comparable : michelson.

(* Characterization of Michelson "comb types", which must have at
   least 2 "fields" *)
Inductive comb_ty : ty -> list ty -> Prop :=
| Comb_two {a b} :
    `{comb_ty (T_pair l n1 n2 a b) [a; b]}
| Comb_cons {c ts t} :
    `{comb_ty c ts ->
      comb_ty (T_pair l n1 n2 t c) (t :: ts)}
.

Hint Constructors comb_ty : michelson.

(* Characterization of comb [GET k] typing *)
Inductive comb_get_ty : ty -> nat -> ty -> Prop :=
| Comb_get_zero' {a} :
  comb_get_ty a O a
| Comb_get_one' {x y} :
  `{comb_get_ty (T_pair l1 n1 n2 x y) 1 x}
| Comb_get_plustwo {n x y z} :
  `{comb_get_ty y n z ->
    comb_get_ty (T_pair l1 n1 n2 x y) (S (S n)) z}
.

(* Characterization of comb [UPDATE k] typing *)
Inductive comb_update_ty : ty -> ty -> nat -> ty -> Prop :=
| Comb_update_zero {t x}:
  comb_update_ty t x O x
| Comb_update_one {a1 a2 b} :
  `{comb_update_ty (T_pair l1 n1 n2 a1 b) a2 1 (T_pair l2 n3 n4 a2 b)}
| Comb_update_plustwo {n a b1 b2 c} :
  `{comb_update_ty b1 c n b2 ->
    comb_update_ty (T_pair l1 n1 n2 a b1) c (S (S n)) (T_pair l2 n3 n4 a b2)}
.

Definition mutez_bound : Z.t := Z.pow 2 63.

Inductive instr_typed : instr -> list ty -> list ty -> Prop :=
(* I_RAW allows embedding raw Michelson. It's like an instruction [RAW n { code }] which means to apply the code *)
| Raw_typed {p s1 s2 b} :
  `{(* raw_typed p s1 b -> *)
      length s1 = n ->
      instr_typed (I_RAW l n p) (s1 ++ s2) (b :: s2)}
(* Structural stuff *)
| Seq_typed {p s1 s2} :
    `{prog_typed p s1 s2 ->
      instr_typed (I_SEQ l p) s1 s2}
| Dip_typed {p a s1 s2} :
    `{prog_typed p s1 s2 ->
      instr_typed (I_DIP l p) (a :: s1) (a :: s2)}
| Dig_typed {n a s1 s2} :
    `{length s1 = n ->
      instr_typed (I_DIG l n) (s1 ++ a :: s2) (a :: s1 ++ s2)}
| Dug_typed {n a s1 s2} :
    `{length s1 = n ->
      instr_typed (I_DUG l n) (a :: s1 ++ s2) (s1 ++ a :: s2)}
| Dup_typed {n s t} :
    `{List.nth_error s n = Some t ->
      instr_typed (I_DUP l (S n)) s (t :: s)}
| Drop_typed {s1 s2} :
    `{length s1 = n ->
      instr_typed (I_DROP l n) (s1 ++ s2) s2}
| Swap_typed {a b s} :
    `{instr_typed (I_SWAP l) (a :: b :: s) (b :: a :: s)}

(* Unit *)
| Unit_typed {s} :
    `{instr_typed (I_UNIT l1) s (T_unit l2 :: s)}
| True_typed {s} :
    `{instr_typed (I_TRUE l1) s (T_bool l2 :: s)}

(* Or *)
| Left_typed {a b s} :
    `{instr_typed (I_LEFT l1 b) (a :: s) (T_or l2 n1 n2 a b :: s)}
| Right_typed {a b s} :
    `{instr_typed (I_RIGHT l1 a) (b :: s) (T_or l2 n1 n2 a b :: s)}
| If_left_typed {bt bf a b s1 s2} :
    `{prog_typed bt (a :: s1) s2 ->
      prog_typed bf (b :: s1) s2 ->
      instr_typed (I_IF_LEFT l1 bt bf) (T_or l2 n1 n2 a b :: s1) s2}

(* Pairs *)
| Pair_typed {n t ts s} :
    `{comb_ty t ts ->
      length ts = n ->
      instr_typed (I_PAIR l1 n) (ts ++ s) (t :: s)}
| Unpair_typed {n t ts s} :
    `{comb_ty t ts ->
      length ts = n ->
      instr_typed (I_UNPAIR l1 n) (t :: s) (ts ++ s)}

| Get_typed {t n x s} :
  `{comb_get_ty t n x ->
    instr_typed (I_GET l1 n) (t :: s) (x :: s)}
| Update_typed {t x n t' s} :
  `{comb_update_ty t x n t' ->
    instr_typed (I_UPDATE l1 n) (x :: t :: s) (t' :: s)}

| Car_typed {a b s} :
    `{instr_typed (I_CAR l1) (T_pair l2 n1 n2 a b :: s) (a :: s)}
| Cdr_typed {a b s} :
    `{instr_typed (I_CDR l1) (T_pair l2 n1 n2 a b :: s) (b :: s)}

(* Bools *)
| If_typed {bt bf s1 s2} :
    `{prog_typed bt s1 s2 ->
      prog_typed bf s1 s2 ->
      instr_typed (I_IF l1 bt bf) (T_bool l2 :: s1) s2}

(* Option *)
| If_none_typed {bt bf a s1 s2} :
    `{prog_typed bt s1 s2 ->
      prog_typed bf (a :: s1) s2 ->
      instr_typed (I_IF_NONE l1 bt bf) (T_option l2 a :: s1) s2}

(* List *)
| Nil_instr_typed {a s1} :
  `{instr_typed (I_NIL l1 a) s1 (T_list l2 a :: s1)}
| Cons_instr_typed {a s} :
  `{instr_typed (I_CONS l1) (a :: T_list l1 a :: s) (T_list l2 a :: s)}
| If_cons_typed {bt bf a s1 s2} :
    `{prog_typed bt (a :: T_list l1 a :: s1) s2 ->
      prog_typed bf s1 s2 ->
      instr_typed (I_IF_CONS l2 bt bf) (T_list l3 a :: s1) s2}

(* Functions *)
| Func_typed {a b r1 r2 code s d1 d2} :
    `{prog_typed code (a :: d1) (b :: d2) ->
      ope_valid r1 (length s) ->
      select r1 s = d1 ->
      ope_valid r2 (length d1) ->
      select r2 d1 = d2 ->
      instr_typed (I_FUNC l1 d1 a b r1 r2 code) s (T_func l2 a b :: s)}
| Exec_func_typed {a b s} :
    `{instr_typed (I_EXEC l1) (a :: T_func l2 a b :: s) (b :: s)}

(* Lambdas *)
| Lambda_typed {a b code s} :
    `{prog_typed code [a] [b] ->
      instr_typed (I_LAMBDA l1 a b code) s (T_lambda l2 a b :: s)}
| Exec_lambda_typed {a b s} :
    `{instr_typed (I_EXEC l1) (a :: T_lambda l2 a b :: s) (b :: s)}
| Apply_lambda_typed {a b c s} :
    `{packable a ->
      instr_typed (I_APPLY_LAMBDA l1 a) (a :: T_lambda l2 (T_pair l3 n1 n2 a b) c :: s) (T_lambda l4 b c :: s)}

(* Loops *)
| Loop_typed {body s} :
    `{prog_typed body s (T_bool l1 :: s) ->
      instr_typed (I_LOOP l2 body) (T_bool l3 :: s) s}
| Loop_left_typed {a b body s} :
    `{prog_typed body (a :: s) (T_or l1 n1 n2 a b :: s) ->
      instr_typed (I_LOOP_LEFT l2 body) (T_or l3 n1 n2 a b :: s) (b :: s)}
| Iter_typed_list {body a s} :
  `{prog_typed body (a :: s) s ->
    instr_typed (I_ITER l1 body) (T_list l2 a :: s) s}
| Map_typed_list {body a b s} :
    `{prog_typed body (a :: s) (b :: s) ->
      instr_typed (I_MAP l1 body) (T_list l2 a :: s) (T_list l3 b :: s)}
(* FICTION *)
| For_typed {body s} :
  `{prog_typed body (T_int l1 :: s) s ->
    instr_typed (I_FOR l2 body) (T_int l3 :: T_int l4 :: T_int l5 :: s) s}

(* Failure *)
| Failwith_typed {a s1 s2} :
    `{instr_typed (I_FAILWITH l) (a :: s1) s2}

(* CREATE_CONTRACT *)
| Create_contract_typed_list {p' s' code s1} :
    `{prog_typed code [T_pair l1 n1 n2 p' s'] [T_pair l2 n3 n4 (T_list l3 (T_operation l4)) s'] ->
      instr_typed (I_CREATE_CONTRACT l5 p' s' code) (T_option l6 (T_key_hash l7) :: T_mutez l8 :: s' :: s1) (T_operation l9 :: T_address l10 :: s1)}

with prog_typed : prog -> list ty -> list ty -> Prop :=
| Nil_typed {s} :
    prog_typed [] (* P_nil *) s s
| Cons_typed {i p s1 s2 s3} :
    instr_typed i s1 s2 ->
    prog_typed p s2 s3 ->
    prog_typed (i :: p) (* (P_cons i p) *) s1 s3
.

Hint Constructors instr_typed : michelson.
Hint Constructors prog_typed : michelson.

Scheme instr_typed_ind' := Induction for instr_typed Sort Prop
with prog_typed_ind' := Induction for prog_typed Sort Prop.

Combined Scheme instr_typed_mutind from
  instr_typed_ind', prog_typed_ind'.

Lemma dig_0_typed {l x s1} : instr_typed (I_DIG l 0) (x :: s1) (x :: s1).
Proof.
  change (instr_typed (I_DIG l 0) ([] ++ x :: s1) (x :: [] ++ s1));
    eauto with michelson.
Defined.

Lemma drop_1_typed {l x s1} : instr_typed (I_DROP l 1) (x :: s1) s1.
Proof.
  change (instr_typed (I_DROP l 1) ([x] ++ s1) s1);
    eauto with michelson.
Defined.

Lemma pair_2_typed {x y s1} : `{instr_typed (I_PAIR l1 2) (x :: y :: s1) (T_pair l2 n1 n2 x y :: s1)}.
Proof.
  intros;
    change (instr_typed (I_PAIR l1 2) ([x; y] ++ s1) (T_pair l2 n1 n2 x y :: s1));
    eauto with michelson.
Defined.

Hint Resolve dig_0_typed : michelson.
Hint Resolve drop_1_typed : michelson.
Hint Resolve pair_2_typed : michelson.

Lemma prog_typed_app {p1 p2 s1 s2 s3} :
  prog_typed p1 s1 s2 ->
  prog_typed p2 s2 s3 ->
  prog_typed (p1 ++ p2) s1 s3.
Proof.
  intros H;
    revert p2 s3;
    induction H;
    intros;
    simpl; eauto with michelson.
Qed.

Hint Resolve prog_typed_app : michelson.

Notation stack_ty := (list ty).

(************
 * Compiler *
 ************)

Fixpoint compile_ope_aux (n : nat) (us : ope) : prog :=
  match us with
  | [] => []
  | false :: us => [I_DIG null n;
                    I_SEQ null (compile_ope_aux (S n) us);
                    I_DROP null 1]
  | true :: us => compile_ope_aux (S n) us
  end.

Definition compile_ope (us : ope) : prog :=
  compile_ope_aux O us.

Lemma compile_ope_aux_typed :
  forall us n s1 s2,
    length s1 = n ->
    length us = length s2 ->
    prog_typed (compile_ope_aux n us) (s1 ++ s2) (s1 ++ select us s2).
Proof.
  intros us; induction us as [|u us]; intros n s1 s2 H1 H2; eauto;
    destruct s2; try (simpl in H2; exfalso; lia);
    simpl; eauto with michelson;
    destruct u; simpl; eauto.
  - enough (H : prog_typed (compile_ope_aux (S n) us) ((s1 ++ [t]) ++ s2) ((s1 ++ [t]) ++ select us s2))
      by (repeat rewrite <- app_assoc in H; exact H);
      eapply IHus.
    + rewrite app_length; simpl; lia.
    + simpl in H2; lia.
  - repeat econstructor; eauto.
    + enough (H : prog_typed (compile_ope_aux (S n) us) (t :: s1 ++ s2) ([t] ++ s1 ++ select us s2)) by exact H;
        change (prog_typed (compile_ope_aux (S n) us) ((t :: s1) ++ s2) ((t :: s1) ++ select us s2));
        eapply IHus; simpl; eauto.
    + eauto.
Qed.

Lemma compile_ope_typed :
  forall {us g g'},
    length us = length g ->
    select us g = g' ->
    prog_typed (compile_ope us) g g'.
Proof.
  intros; subst; apply compile_ope_aux_typed with (s1 := []); auto.
Qed.

Lemma compile_ope_aux_weak_lemma1  :
  forall us m n, compile_ope_aux n (us ++ repeat true m) = compile_ope_aux n us.
Proof.
  intros us; induction us as [|u us]; intros m n; simpl.
  - revert n; induction m; intros n.
    + reflexivity.
    + eapply IHm.
  - destruct u; rewrite IHus; reflexivity.
Qed.

Lemma compile_ope_weak_lemma1  :
  forall us m, compile_ope (us ++ repeat true m) = compile_ope us.
Proof.
  unfold compile_ope; intros; eapply compile_ope_aux_weak_lemma1.
Qed.

Lemma compile_ope_weak_typed :
  forall {us g g' d},
    length us = length g ->
    select us g = g' ->
    prog_typed (compile_ope us) (g ++ d) (g' ++ d).
Proof.
  intros us g g' d H1 H2;
    assert (H3 : prog_typed (compile_ope (us ++ repeat true (length d))) (g ++ d) (g' ++ d))
      by (eapply compile_ope_typed;
          [ repeat rewrite app_length; rewrite repeat_length; eauto
          | rewrite select_app; eauto; rewrite select_repeat_true; rewrite H2; reflexivity ]);
    rewrite compile_ope_weak_lemma1 in H3;
    exact H3.
Qed.

Fixpoint comb (az : list ty) : ty :=
  match az with
  | [] => T_unit null
  | [a] => a
  | a :: az => T_pair null None None a (comb az)
  end.

Definition PAIR (n : nat) : prog :=
  match n with
  | 0 => [I_UNIT null]
  | 1 => []
  | _ => [I_PAIR null n]
  end.

Definition UNPAIR (n : nat) : prog :=
  match n with
  | 0 => [I_DROP null 1]
  | 1 => []
  | _ => [I_UNPAIR null n]
  end.

(* hmm, reusing the names GET and UPDATE is confusing because these are different *)
Definition GET (i n : nat) : prog :=
  let i := if beq_nat (S i) n
           then 2 * i
           else 2 * i + 1 in
  [I_GET null i].

Definition UPDATE (i n : nat) : prog :=
  let i := if beq_nat (S i) n
           then 2 * i
           else 2 * i + 1 in
  [I_UPDATE null i].

Fixpoint compile_expr (r : ope) (env : list ty) (e : expr) {struct e} : prog :=
  match e with
  | E_var _ n => [I_DUP null (S (embed r n))]
  | E_let_in _ e1 e2 =>
      let e1' := compile_expr r env e1 in
      let e2' := compile_binds r env e2 in
      [I_SEQ null e1';
       I_SEQ null e2']
  | E_deref _ n => [I_DUP null (S (embed r n))]
  | E_let_mut_in _ e1 e2 =>
      let e1' := compile_expr r env e1 in
      let e2' := compile_binds r env e2 in
      [I_SEQ null e1';
       I_SEQ null e2']
  | E_assign _ n e1 =>
      let e1' := compile_expr r env e1 in
      [I_SEQ null e1';
       I_DUG null (embed r n);
       I_DIG null (S (embed r n));
       I_DROP null 1;
       I_UNIT null]
  | E_for _ args body =>
      let args' := compile_args r env args in
      let body' := compile_binds r env body in
      [I_SEQ null args';
       I_FOR null [I_SEQ null body'; I_DROP null 1];
       I_UNIT null]
  | E_for_each l coll body =>
      let coll' := compile_expr r env coll in
      let body' := compile_binds r env body in
      (* TODO? this is a hack to make map iteration work -- Michelson
         gives you the key and value in a pair, but LIGO things here
         are set up to expect two stack elements, so insert an UNPAIR
         in the map case: *)
      if beq_nat 2 (binds_length body)
      then
        [I_SEQ null coll';
         I_ITER l [I_UNPAIR null 2; I_SEQ null body'; I_DROP null 1];
         I_UNIT null]
      else
        [I_SEQ null coll';
         I_ITER l [I_SEQ null body'; I_DROP null 1];
         I_UNIT null]
  | E_while l cond body =>
      let cond' := compile_expr r env cond in
      let body' := compile_expr (false :: r) env body in
      [I_TRUE null;
       I_LOOP l [I_SEQ null cond';
                 I_DUP null 1;
                 I_IF null [I_SEQ null body'; I_DROP null 1] []];
       I_UNIT null]
  | E_tuple _ args =>
      [I_SEQ null (compile_args r env args);
       I_SEQ null (PAIR (args_length args))]
  | E_let_tuple _ e1 e2 =>
      [I_SEQ null (compile_expr r env e1);
       I_SEQ null (UNPAIR (binds_length e2));
       I_SEQ null (compile_binds r env e2)]
  | E_proj _ e i n =>
    [I_SEQ null (compile_expr r env e);
     I_SEQ null (GET i n)]
  | E_update _ args i n =>
    [I_SEQ null (compile_args r env args);
     I_SEQ null (UPDATE i n)]
  | E_app _ e =>
      [I_SEQ null (compile_args r env e);
       I_SWAP null;
       I_EXEC null]
  | E_lam l e b =>
      let a := match e with
               | Binds _ [a] _ => a
               | _ => T_unit null
               end in
      [I_FUNC l env a b (trim r (length env)) (repeat true (length env))
              (compile_binds (repeat true (length env)) env e)]
  | E_literal l lit =>
      [I_RAW null O (lit_code l lit)]
  | E_pair l e =>
      [I_SEQ null (compile_args r env e);
       I_PAIR l 2]
  | E_car l e =>
      [I_SEQ null (compile_expr r env e);
       I_CAR l]
  | E_cdr l e =>
      [I_SEQ null (compile_expr r env e);
       I_CDR l]
  | E_unit l =>
      [I_UNIT l]
  | E_left l b e =>
      [I_SEQ null (compile_expr r env e);
       I_LEFT l b]
  | E_right l a e =>
      [I_SEQ null (compile_expr r env e);
       I_RIGHT l a]
  | E_if_bool l e1 e2 e3 =>
      [I_SEQ null (compile_expr r env e1);
       I_IF l (compile_expr r env e2) (compile_expr r env e3)]
  | E_if_none l e1 e2 b3 =>
      [I_SEQ null (compile_expr r env e1);
       I_IF_NONE l (compile_expr r env e2) (compile_binds r env b3)]
  | E_if_cons l e1 b2 e3 =>
      [I_SEQ null (compile_expr r env e1);
       I_IF_CONS l (compile_binds r env b2) (compile_expr r env e3)]
  | E_if_left l e1 b2 b3 =>
      [I_SEQ null (compile_expr r env e1);
       I_IF_LEFT l (compile_binds r env b2) (compile_binds r env b3)]
  | E_iter l e1 e2 =>
      [I_SEQ null (compile_expr r env e2);
       I_ITER l (compile_binds r env e1 ++ [I_DROP l 1]);
       I_UNIT l]
  | E_map l e1 e2 =>
      [I_SEQ null (compile_expr r env e2);
       I_MAP l (compile_binds r env e1)]
  | E_loop_left l e1 b e2 =>
      [I_SEQ null (compile_expr r env e2);
       I_LEFT null b;
       I_LOOP_LEFT l (compile_binds r env e1)]
  | E_fold l e1 e2 e3 =>
      [I_SEQ null (compile_expr r env e1);
       I_SEQ null (compile_expr (false :: r) env e2);
       I_ITER l [I_SWAP null; I_PAIR null 2;
                 I_SEQ null (compile_binds r env e3)]]
  | E_fold_right l elem e1 e2 e3 =>
      [I_SEQ null (compile_expr r env e1);
       I_SEQ null (compile_expr (false :: r) env e2);
       I_NIL null elem; I_SWAP null; I_ITER null [I_CONS null];
       I_ITER l [I_PAIR null 2;
                 I_SEQ null (compile_binds r env e3)]]
  | E_failwith x e =>
      [I_SEQ null (compile_expr r env e);
       I_FAILWITH null]
  | E_raw_michelson l a b code =>
      [I_LAMBDA null a b [I_RAW l 1 code]]
  | E_inline_michelson l _ code args =>
      compile_args r env args ++ [I_RAW l (args_length args) code]
  | E_global_constant l b hash args =>
      [I_SEQ null (compile_args r env args);
       I_RAW null (args_length args) (global_constant l hash)]
  | E_create_contract l p s script args =>
      [I_SEQ null (compile_args r env args);
       I_CREATE_CONTRACT l p s (compile_binds [true] [T_pair null None None p s] script);
       I_PAIR null 2]
  end
with
compile_args
  (r : ope) (env : list ty) (e : args) {struct e} : prog :=
  match e with
  | Args_nil l => []
  | Args_cons l e args =>
    [I_SEQ null (compile_expr r env e);
     I_SEQ null (compile_args (false :: r) env args)]
  end
with
compile_binds
  (r : ope) (env : list ty) (e : binds) {struct e} : prog :=
  match e with
  | Binds l az e =>
      let env' := az ++ env in
      let r' := repeat true (length az) ++ r in
      app [I_SEQ (with_var_names env' null) []]
          (app (compile_expr r' env' e)
               [I_DIP null [I_DROP null (length az)]])
  end.

Definition compile_expr_typed (e : expr) : Prop :=
  forall g a,
    expr_typed g e a ->
    forall r d,
      select r d = g ->
      prog_typed (compile_expr r g e) d (a :: d).

Definition compile_args_typed (e : args) : Prop :=
  forall g az,
    args_typed g e az ->
    forall r d,
      select r d = g ->
      prog_typed (compile_args r g e) d (az ++ d).

Definition compile_binds_typed (e : binds) : Prop :=
  forall g az b,
    binds_typed g e az b ->
    forall r d,
      select r d = g ->
      prog_typed (compile_binds r g e) (az ++ d) (b :: d).

Ltac Zify.zify_pre_hook ::=
  repeat match goal with
         | [H : @eq (list _) _ _ |- _] =>
             eapply (f_equal (@length _)) in H;
             repeat rewrite app_length, plus_comm in H;
             simpl in H
         end.

Lemma type_preservation :
  (forall e, compile_expr_typed e) /\
    (forall e, compile_binds_typed e) /\
    (forall e, compile_args_typed e).
Proof.
  eapply expr_mutind;
    unfold compile_expr_typed, compile_binds_typed, compile_args_typed;
    intros;
    first[ invert_pred expr_typed
         | invert_pred args_typed
         | invert_pred binds_typed
         ];
    repeat match goal with
           | [H1 : forall g b, expr_typed g ?e b -> forall r d, select r d = g -> prog_typed (compile_expr r g ?e) d (b :: d),
                H2: expr_typed ?g ?e ?b |- _] =>
               first [epose proof (H1 _ _ H2 [] _ eq_refl)
                     |epose proof (H1 _ _ H2 _ _ eq_refl)];
               mark_done H2
           | [H1 : forall g az b, binds_typed g ?e az b -> forall r d, select r d = g -> prog_typed (compile_binds r g ?e) (az ++ d) (b :: d),
                H2: binds_typed ?g ?e ?az ?b |- _] =>
               first [epose proof (H1 _ _ _ H2 [] _ eq_refl)
                     |epose proof (H1 _ _ _ H2 _ _ eq_refl)];
               mark_done H2
           | [H1 : forall g az, args_typed g ?e az -> forall r d, select r d = g -> prog_typed (compile_args r g ?e) d (az ++ d),
                H2: args_typed ?g ?e ?az |- _] =>
               first [epose proof (H1 _ _ H2 [] _ eq_refl)
                     |epose proof (H1 _ _ H2 _ _ eq_refl)];
               mark_done H2
           end;
    clear_done;
    simpl.
  (* E_var *)
  - repeat econstructor;
      rewrite nth_error_embed;
      auto; congruence.
  (* E_let_in *)
  - eauto with michelson.
  (* E_tuple *)
  - admit.
  (* E_let_tuple *)
  - admit.
  (* E_proj *)
  - admit.
  (* E_update *)
  - admit.
  (* E_app *)
  - admit.
  (* E_lam *)
  - destruct b as [l3 az e];
      destruct az as [|a1 [|a2 az]];
      inversion H7; subst;
      (* hmm *)
      assert (lemma : forall {A : Set} r (xs : list A), select (repeat true (length (select r xs))) (select r xs) = select r xs)
      by (clear; intros A r; induction r as [|[] r]; intros [|x xs]; try reflexivity; simpl;
          try apply f_equal; try rewrite IHr; try rewrite select_nothing; reflexivity);
      specialize (H _ _ _ H7 (repeat true (length (select r d))) (select r d) (lemma _ r d));
      econstructor; [|econstructor];
      econstructor;
      try reflexivity;
      try rewrite select_repeat_true';
      try rewrite repeat_length;
      try lia;
      try assumption;
      try apply select_length_le_weight.
    + eapply trim_valid.
    + rewrite trim_ok; auto.
    + eapply repeat_valid.
  (* E_literal *)
  - admit.
  (* E_pair *)
  - admit.
  (* E_car *)
  - admit.
  (* E_cdr *)
  - admit.
  (* E_unit *)
  - admit.
  (* E_left *)
  - admit.
  (* E_right *)
  - admit.
  (* E_if_left *)
  - admit.
  (* E_if_bool *)
  - admit.
  (* E_if_none *)
  - admit.
  (* E_if_cons *)
  - admit.
  (* E_iter *)
  - admit.
  (* E_map *)
  - admit.
  (* E_loop_left *)
  - admit.
  (* E_fold *)
  - invert_pred iter_class.
    + eauto 20 with michelson.
    + admit.
    + admit.
  (* E_fold_right *)
  - invert_pred iter_class.
    + eauto 20 with michelson.
    + admit.
    + admit.
  (* E_failwith *)
  - eauto with michelson.
  (* E_raw_michelson *)
  - admit.
  (* E_inline_michelson *)
  - admit.
  (* E_global_constant *)
  - admit.
  (* E_create_contract *)
  - admit.
  (* Binds *)
  - specialize (H _ _ H8 (repeat true (length az) ++ r) (az ++ d));
      rewrite select_app in H by (rewrite repeat_length; auto);
      rewrite select_repeat_true in H;
      specialize (H eq_refl);
      econstructor; [repeat econstructor|];
      eapply prog_typed_app;
      eauto with michelson.
  (* Args_nil *)
  - eauto with michelson.
  (* Args_cons *)
  - eauto with michelson.
Admitted.

(* This is used to postulate termination for strengthening loops. Should
   probably replace it with specific Axioms? *)
Definition TRUST_ME_IT_TERMINATES {A : Set} : A.
Proof. admit. Admitted.

Reserved Notation "'strengthen_progg'".

Context {strengthen_meta : ope -> meta -> meta}.

Fixpoint
  strengthen_instr (i : instr) (r : list bool) {struct i} : list bool * prog :=
  match i with
  | I_RAW l n code =>
      if ope_hd r
      then (repeat true n ++ tl r,
             [I_RAW l n code])
      (* if we knew that the code is pure, we could do this: *)
      (* else (repeat false n ++ tl r, []) *)
      (* but instead we have to settle for: *)
      else (repeat true n ++ tl r,
             [I_RAW l n code; I_DROP null 1])
  | I_SEQ l p =>
      let rp' := strengthen_progg p r in
      let r' := fst rp' in
      let p' := snd rp' in
      let l' := strengthen_meta r'  l in
      (r',
        [I_SEQ l' p'])
  | I_DIP l p =>
      let rp' := strengthen_progg p (tl r) in
      let r' := fst rp' in
      let p' := snd rp' in
      (ope_hd r :: r',
        if ope_hd r
        then [I_DIP l p']
        else p')
  | I_DIG l n =>
      if ope_hd r
      then
        let r := tl r in
        (split_left n r ++ true :: split_right n r,
          [I_DIG l (weight (split_left n r))])
      else
        let r := tl r in
        (split_left n r ++ false :: split_right n r,
          [])
  | I_DUG l n =>
      let r1 := split_left n r in
      let r2 := split_right n r in
      if ope_hd r2
      then
        let r2 := tl r2 in
        (true :: r1 ++ r2,
          [I_DUG l (weight r1)])
      else
        let r2 := tl r2 in
        (false :: r1 ++ r2,
          [])
  | I_DUP l n =>
      let r1 := split_left n r in
      let r2 := split_right n r in
      if ope_hd r1
      then
        (tl r1 ++ true :: tl r2,
          if ope_hd r2
          then [I_DUP l (S (weight (tl r1)))]
          else [I_DIG l (weight (tl r1))])
      else
        (tl r1 ++ r2, [])
  | I_DROP l n =>
      (repeat false n ++ r, []) (* :D *)
  | I_SWAP l =>
      (ope_hd (tl r) :: ope_hd r :: tl (tl r),
        if andb (ope_hd r) (ope_hd (tl r))
        then [I_SWAP l]
        else [])
  | I_UNIT l =>
      (tl r,
        if ope_hd r
        then [I_UNIT l]
        else [])
  | I_TRUE l =>
      (tl r,
        if ope_hd r
        then [I_TRUE l]
        else [])
  | I_PAIR l n =>
      if ope_hd r
      then (repeat true n ++ tl r, [I_PAIR l n])
      else (repeat false n ++ tl r, [])
  | I_UNPAIR l n =>
      let r1 := split_left n r in
      let r2 := split_right n r in
      if leb 1 (weight r1)
      then
        if leb (weight r1) 1
        then
          (* only one thing selected, use GET *)
          let idx := (fix todo (n : nat) (r1 : ope) {struct r1} : nat :=
                        match r1 with
                        | [] => 0
                        | b :: r1 => if b then n else todo (S n) r1
                        end) 0 r1 in
          (* comb elements are indexed with GET 2i+1, except the last
             element which is GET 2i. *)
          let zero_or_one := if (beq_nat idx (n-1)) then 0 else 1 in
          (true :: r2, [I_GET l (zero_or_one + 2 * idx)])
        else
          (* more than one thing selected, use UNPAIR *)
          (true :: r2, I_UNPAIR l n :: compile_ope r1)
      else
        (* nothing selected *)
        (false :: r2, [])
  | I_GET l n =>
      (r,
        if ope_hd r
        then [I_GET l n]
        else [])
  | I_UPDATE l n =>
      (ope_hd r :: r,
        if ope_hd r
        then [I_UPDATE l n]
        else [])
  | I_CAR l =>
      (r,
        if ope_hd r
        then [I_CAR l]
        else [])
  | I_CDR l =>
      (r,
        if ope_hd r
        then [I_CDR l]
        else [])
  | I_LEFT l b =>
      (r,
        if ope_hd r
        then [I_LEFT l b]
        else [])
  | I_RIGHT l a =>
      (r,
        if ope_hd r
        then [I_RIGHT l a]
        else [])
  | I_IF_LEFT l bt bf =>
      let (rt, bt') := strengthen_progg bt r in
      let (rf, bf') := strengthen_progg bf r in
      let r' := true :: union (tl rt) (tl rf) in
      (r',
        [I_IF_LEFT l
                   (compile_ope (ope_hd rt :: inj1 (tl rt) (tl rf)) ++ bt')
                   (compile_ope (ope_hd rf :: inj2 (tl rt) (tl rf)) ++ bf')])

  | I_IF l bt bf =>
      let (rt, bt') := strengthen_progg bt r in
      let (rf, bf') := strengthen_progg bf r in
      let r' := true :: union rt rf in
      (r',
        [I_IF l
              (compile_ope (inj1 rt rf) ++ bt')
              (compile_ope (inj2 rt rf) ++ bf')])

  | I_IF_NONE l bt bf =>
      let (rt, bt') := strengthen_progg bt r in
      let (rf, bf') := strengthen_progg bf r in
      let r' := true :: union rt (tl rf) in
      (r',
        [I_IF_NONE l
                   (compile_ope (inj1 rt (tl rf)) ++ bt')
                   (compile_ope (ope_hd rf :: inj2 rt (tl rf)) ++ bf')])

  | I_NIL l a =>
      (tl r,
        if ope_hd r
        then [I_NIL l a]
        else [])
  | I_CONS l =>
      (ope_hd r :: ope_hd r :: tl r,
        if ope_hd r
        then [I_CONS l]
        else [])
  | I_IF_CONS l bt bf =>
      let (rt, bt') := strengthen_progg bt r in
      let (rf, bf') := strengthen_progg bf r in
      let r' := true :: union (tl (tl rt)) rf in
      (r',
        [I_IF_CONS l
                   (compile_ope (ope_hd rt :: ope_hd (tl rt) :: inj1 (tl (tl rt)) rf) ++ bt')
                   (compile_ope (inj2 (tl (tl rt)) rf) ++ bf')])

  | I_FUNC l cs a b r1 r2 body =>
      if ope_hd r
      then
        let (rb, body') := strengthen_progg body (true :: repeat false (weight r2)) in
        (union (comp (tl rb) r1) (tl r),
          I_FUNC l
                 (select (tl rb) cs)
                 a b
                 (inj1 (comp (tl rb) r1) (tl r))
                 (repeat false (weight (tl rb)))
                 (compile_ope [ope_hd rb] ++ body')
            :: compile_ope (true :: inj2 (comp (tl rb) r1) (tl r)))
      else (tl r, [])
  | I_LAMBDA l a b body =>
      (tl r,
        if ope_hd r
        then
          let (rb, body') := strengthen_progg body [true] in
          [I_LAMBDA l a b (compile_ope [ope_hd rb] ++ body')]
        else [])
  | I_EXEC l =>
      (true :: true :: tl r,
        I_EXEC l :: compile_ope [ope_hd r])
  | I_APPLY_LAMBDA l c =>
      (ope_hd r :: ope_hd r :: tl r,
        if ope_hd r
        then [I_APPLY_LAMBDA l c]
        else [])

  | I_FAILWITH l =>
      ([true],
        [I_FAILWITH l])

  | I_LOOP l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (fst (strengthen_progg body (true :: x))) r) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body (true :: rfix) in
      (true :: rfix,
        I_LOOP l (compile_ope (inj1 rb r) ++ body') :: compile_ope (inj2 rb r))
  | I_LOOP_LEFT l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (tl (fst (strengthen_progg body (true :: x)))) (tl r)) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body (true :: rfix) in
      (true :: rfix,
        I_LOOP_LEFT l (compile_ope (ope_hd rb :: inj1 (tl rb) (tl r)) ++ body') :: compile_ope (ope_hd r :: inj2 (tl rb) (tl r)))
  | I_ITER l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (tl (fst (strengthen_progg body x))) r) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body rfix in
      (true :: rfix,
        I_ITER l (compile_ope (ope_hd rb :: inj1 (tl rb) r) ++ body') :: compile_ope (inj2 (tl rb) r))
  | I_MAP l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (tl (fst (strengthen_progg body (true :: x)))) (tl r)) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body (true :: rfix) in
      (true :: rfix,
        I_MAP l (compile_ope (true :: inj1 (tl rb) (tl r)) ++ body') :: compile_ope (ope_hd r :: inj2 (tl rb) (tl r)))
  | I_FOR l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (tl (fst (strengthen_progg body x))) r) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body rfix in
      (true :: true :: true :: rfix,
        I_FOR l (compile_ope (ope_hd rb :: inj1 (tl rb) r) ++ body') :: compile_ope (inj2 (tl rb) r))
  | I_CREATE_CONTRACT l p s script =>
      let (rs, script') := strengthen_progg script [true] in
      (true :: true :: true :: tl (tl r),
        [I_CREATE_CONTRACT l p s (compile_ope [ope_hd rs] ++ script')])
        
  end
(* this notation deals with the nested induction problem here, with
   instrs containing lists of instrs *)
where "'strengthen_progg'" :=
  (fix strengthen_prog (p : prog) (r : ope) {struct p} : list bool * prog :=
     match p with
     | [] => (r, [])
     | i :: p =>
         let r1p' := strengthen_prog p r in
         let r1 := fst r1p' in
         let p' := snd r1p' in
         let r2i' := strengthen_instr i r1 in
         let r2 := fst r2i' in
         let i' := snd r2i' in
         (r2, i' ++ p')
     end).

(* Exposing the 'strengthen_progg' notation for extraction: *)
Definition strengthen_prog (p : prog) (r : ope) : list bool * prog :=
  strengthen_progg p r.

Definition strengthen_instr_preservation :
  forall i s1 s2, instr_typed i s1 s2 -> Prop :=
  fun i s1 s2 _ => 
    forall r,
      (* TODO use ope_valid here? *)
      length r <= length s2 ->
      length (fst (strengthen_instr i r)) <= length s1 /\
      prog_typed (snd (strengthen_instr i r))
                 (select (fst (strengthen_instr i r)) s1)
                 (select r s2).

Definition strengthen_prog_preservation :
  forall p s1 s2, prog_typed p s1 s2 -> Prop :=
  fun p s1 s2 _ => 
    forall r,
      length r <= length s2 ->
      length (fst (strengthen_progg p r)) <= length s1 /\
      prog_typed (snd (strengthen_progg p r))
                 (select (fst (strengthen_progg p r)) s1)
                 (select r s2).

Lemma tl_length {A : Set} (xs : list A) :
  length (tl xs) = length xs - 1.
Proof. destruct xs; simpl; try rewrite Nat.sub_0_r; reflexivity. Qed.

Ltac strengthen_rewrite :=
  subst; simpl; simpl in *;
  repeat first [rewrite app_length in *
               |rewrite repeat_length in *
               |rewrite tl_length in *
               |rewrite inj1_length in *
               |rewrite inj2_length in *
               |rewrite union_length in *
               |rewrite select_inj1 in *
               |rewrite select_inj2 in *
               |rewrite select_app in *
               |rewrite select_filter_id in *
               |rewrite select_repeat_false in *
               |rewrite select_repeat_true' in *
               |rewrite split_left_length in *
               |rewrite split_right_length in *
               |rewrite split_left_nil in *
               |rewrite split_right_nil in *
               |rewrite select_split in *
               |rewrite select_weight in *
               |simpl in *].

Opaque compile_ope.

Lemma strengthen_instr_typed :
  (forall i s1 s2 (H : instr_typed i s1 s2), strengthen_instr_preservation H) /\
    (forall p s1 s2 (H : prog_typed p s1 s2), strengthen_prog_preservation H).
Proof with try split; try lia; eauto 15 with michelson.
  eapply (@instr_typed_mutind
            strengthen_instr_preservation
            strengthen_prog_preservation);
    unfold strengthen_prog_preservation;
    unfold strengthen_instr_preservation;
    simpl; intros;
    match goal with
    | [|- True] => trivial
    | _ => idtac
    end.
  (* I_RAW *)
  - destruct r as [|[|] r];
      strengthen_rewrite...
  (* I_SEQ *)
  - specialize (H r H0); destruct_conjs; eauto with michelson.
  (* I_DIP *)
  - specialize (H (tl r));
      rewrite tl_length in H;
      specialize (H ltac:(lia));
      destruct_conjs;
      remember (strengthen_progg p (tl r)) as p';
      destruct r as [|[|] r];
      split;
      try lia;
      try eassumption;
      simpl;
      eauto with michelson.
  (* I_DIG *)
  - destruct r as [|[|] r];
      strengthen_rewrite;
      try split; try lia; eauto with michelson;
      econstructor; econstructor;
      strengthen_rewrite;
      auto.
  (* I_DUG *)
  - remember (split_right (length s1) r) as rr;
      destruct rr as [|[|] rr];
      try destruct u;
      subst; rewrite <- Heqrr in *;
      strengthen_rewrite;
      try rewrite <- Heqrr in *;
      try split; try lia; eauto with michelson;
      pose proof (f_equal (@length _) Heqrr) as Hlen;
      simpl in Hlen;
      rewrite split_right_length in Hlen;
      try lia;
      econstructor; econstructor;
      strengthen_rewrite; auto.
  (* I_DUP *)
  - pose proof (nth_error_split _ _ e);
      destruct_conjs;
      destruct r as [|[|] r];
      strengthen_rewrite;
      try split; try lia; eauto with michelson;
      remember (split_right (length H0) r) as rr;
      destruct rr as [|[|] rr];
      strengthen_rewrite;
      econstructor; econstructor;
      strengthen_rewrite;
      auto;
      rewrite nth_error_app2 by (rewrite select_weight; auto; rewrite split_left_length; auto);
      rewrite select_weight by (rewrite split_left_length; auto);
      rewrite Nat.sub_diag;
      reflexivity.
  (* I_DROP *)
  - strengthen_rewrite...
  (* I_SWAP *)
  - destruct r as [|[|] [|[|] r]]; strengthen_rewrite...
  (* I_UNIT *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_TRUE *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_LEFT *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_RIGHT *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_IF_LEFT *)
  - specialize (H r);
      specialize (H0 r);
      remember (strengthen_progg bt r) as bt';
      destruct bt' as [[|[|] rt] bt'];
      remember (strengthen_progg bf r) as bf';
      destruct bf' as [[|[|] rf] bf'];
      simpl;
      specialize (H H1);
      specialize (H0 H1);
      destruct_conjs;
      strengthen_rewrite;
      try split;
      repeat rewrite union_nil_r;
      try lia;
      try solve [rewrite union_length; lia];
      try solve [econstructor; econstructor;
                 eapply prog_typed_app;
                 try eassumption;
                 eapply compile_ope_typed;
                 simpl;
                 first[ rewrite inj1_length;
                        rewrite select_weight by (rewrite union_length; lia);
                        reflexivity
                      | rewrite select_inj1; reflexivity
                      | rewrite inj2_length;
                        rewrite select_weight by (rewrite union_length; lia);
                        reflexivity
                      | rewrite select_inj2; reflexivity ]].
    + econstructor; econstructor;
      eapply prog_typed_app;
      try eassumption;
      eapply compile_ope_typed;
      auto.
    + assert (hmm : match rf with
                    | [] => []
                    | _ :: _ => repeat false (Datatypes.length (filter id rf))
                    end = repeat false (Datatypes.length (filter id rf)))
        by (destruct rf; reflexivity);
        rewrite hmm; clear hmm;
        econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
        simpl;
        try rewrite repeat_length;
        try rewrite select_weight;
        try rewrite select_repeat_false;
        try rewrite select_filter_id;
        auto; lia.
    + assert (hmm : match rf with
                    | [] => []
                    | _ :: _ => repeat false (Datatypes.length (filter id rf))
                    end = repeat false (Datatypes.length (filter id rf)))
        by (destruct rf; reflexivity);
        rewrite hmm; clear hmm;
        econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
        simpl;
        try rewrite repeat_length;
        try rewrite select_weight;
        try rewrite select_repeat_false;
        try rewrite select_filter_id;
        auto; lia.
    + econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
        simpl;
        try rewrite repeat_length;
        try rewrite select_weight;
        try rewrite select_repeat_false;
        try rewrite select_filter_id;
        auto; try lia;
        repeat rewrite inj1_length;
        repeat rewrite inj2_length;
        repeat rewrite union_nil_r;
        repeat rewrite union_nil_l;
        auto;
        match goal with
        | [|- context G [select (inj1 ?r []) (select ?r ?s)]] =>
            let H := fresh "H" in
            let g := context G [select (inj1 r []) (select (union r []) s)] in
            enough (H : g) by (rewrite union_nil_r in H; exact H);
            rewrite select_inj1
        | [|- context G [select (inj2 ?r []) (select ?r ?s)]] =>
            let H := fresh "H" in
            let g := context G [select (inj2 r []) (select (union r []) s)] in
            enough (H : g) by (rewrite union_nil_r in H; exact H);
            rewrite select_inj2
        end;
        reflexivity.
    + econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
        simpl.
      * rewrite inj1_length; rewrite union_nil_r;
          rewrite select_weight by lia; auto.
      * rewrite <- (union_nil_r rt) at 2;
          rewrite select_inj1; auto.
      * rewrite inj2_length, union_nil_r;
          rewrite select_weight by lia; auto.
      * rewrite <- (union_nil_r rt) at 2;
          rewrite select_inj2; auto.
  (* I_PAIR *)
  - destruct r as [|[|] r];
      strengthen_rewrite...
  (* I_UNPAIR *)
  - admit.
  (* I_GET *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_UPDATE *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_CAR *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_CDR *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_IF *)
  - specialize (H r);
      specialize (H0 r);
      remember (strengthen_progg bt r) as bt';
      destruct bt' as [[|[|] rt] bt'];
      remember (strengthen_progg bf r) as bf';
      destruct bf' as [[|[|] rf] bf'];
      simpl;
      specialize (H H1);
      specialize (H0 H1);
      destruct_conjs;
      strengthen_rewrite;
      try split;
      repeat rewrite union_nil_r;
      try lia;
      try solve [rewrite union_length; lia];
      econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
      try reflexivity;
      destruct s1; strengthen_rewrite...
  (* I_IF_NONE *)
  - specialize (H r);
      specialize (H0 r);
      remember (strengthen_progg bt r) as bt';
      destruct bt' as [rt bt'];
      remember (strengthen_progg bf r) as bf';
      destruct bf' as [[|[|] rf] bf'];
      simpl;
      specialize (H H1);
      specialize (H0 H1);
      destruct_conjs;
      strengthen_rewrite;
      try split;
      try lia;
      try eassumption;
      econstructor; try econstructor;
      try eapply prog_typed_app;
      try eapply compile_ope_typed;
      strengthen_rewrite...
  (* I_NIL *)
  - split; destruct r as [|[|] r]; strengthen_rewrite...
  (* I_CONS *)
  - admit.
  (* I_IF_CONS *)
  - specialize (H r);
      specialize (H0 r);
      remember (strengthen_progg bt r) as bt';
      destruct bt' as [[|[|] [|[|] rt]] bt'];
      remember (strengthen_progg bf r) as bf';
      destruct bf' as [rf bf'];
      specialize (H H1);
      specialize (H0 H1);
      destruct_conjs;
      strengthen_rewrite;
      try split;
      try lia;
      try eassumption;
      econstructor; econstructor;
      eapply prog_typed_app;
      try eassumption;
      try eapply compile_ope_typed;
      try destruct rf;
      try destruct b;
      try destruct s1;
      strengthen_rewrite...
  (* I_FUNC *)
  - admit.
  (* I_EXEC *)
  - destruct r;
      split;
      simpl in *;
      try lia;
      repeat econstructor.
    + eapply compile_ope_typed; auto.
    + destruct b0.
      * apply (@compile_ope_weak_typed [true] [b] [b]); auto.
      * apply (@compile_ope_weak_typed [false] [b] []); auto.
  (* I_LAMBDA *)
  - specialize (H [true] ltac:(simpl; lia));
      remember (strengthen_progg code [true]) as code';
      destruct code' as [rcode code'];
      destruct r as [|[|] r];
      strengthen_rewrite...
      destruct_conjs;
    econstructor; econstructor;
        eapply prog_typed_app;
        try eapply compile_ope_typed;
        try eassumption;
        try reflexivity;
        destruct rcode as [|[|] rcode];
        try reflexivity;
        simpl; rewrite select_nothing;
        reflexivity.
  (* I_EXEC *)
  - destruct r as [|[|] r]; split; simpl in *; try lia;
      try (repeat econstructor; eapply compile_ope_typed; reflexivity);
      repeat econstructor; eapply (@compile_ope_weak_typed [false] [b] []); reflexivity.
  (* I_APPLY_LAMBDA *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_LOOP *)
  - match goal with [|- context[fix_ope ?f ?x ?d]] => remember (fix_ope f x d) as rfix end;
    destruct rfix as [rfix rfixed];
    simpl;
    pose proof (union_length (fst (strengthen_progg body (true :: rfix))) r) as Hlen1;
    assert (Hlen2 : length rfix <= length s).
    { eapply (f_equal (@proj1_sig _ _)) in Heqrfix;
        simpl in Heqrfix;
        rewrite Heqrfix;
        eapply fix_ope_preserves.
      - intros x Hx;
        rewrite union_length;
        pose proof (proj1 (H (true :: x) (le_n_S _ _ Hx)));
        lia.
      - simpl; lia. }
    pose proof (H (true :: rfix) (le_n_S _ _ Hlen2)) as [H1 H2];
      repeat match goal with
             | [|- context[let (x, y) := ?z in _]] =>
                 rewrite (surjective_pairing z)
             end;
      remember (strengthen_progg body (true :: rfix)) as rbody';
      simpl;
      split; [lia|];
      repeat econstructor;
      try eapply prog_typed_app;
      try eapply compile_ope_typed;
      try rewrite inj1_length;
      try rewrite inj2_length.
    + admit.
    + assert (HH : select (inj1 (fst rbody') r) (select rfix s) =
                     select (inj1 (fst rbody') r) (select (union (fst rbody') r) s)) by admit.
      rewrite HH, select_inj1; reflexivity.
    + exact H2.
    + admit.
    + assert (HH : select (inj2 (fst rbody') r) (select rfix s) =
                     select (inj2 (fst rbody') r) (select (union (fst rbody') r) s)) by admit.
      rewrite HH, select_inj2; reflexivity.
  (* I_LOOP_LEFT *)
  - admit.
  (* I_ITER *)
  - admit.
  (* I_MAP *)
  - admit.
  (* I_FOR *)
  - admit.
  (* I_FAILWITH *)
  - idtac...
  (* I_CREATE_CONTRACT *)
  - admit.
  (* nil prog *)
  - split...
  (* cons prog *)
  - specialize (H0 r H1);
      destruct_conjs;
      specialize (H (fst (strengthen_progg p r)) H0);
      destruct_conjs;
      split; try lia;
      remember (strengthen_progg p r) as p';
      destruct p' as [r' p'];
      try lia;
      eapply prog_typed_app...
Admitted.

End compiler.
