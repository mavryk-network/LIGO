Set Implicit Arguments.
From Coq Require Import String.

From ligo_coq_type_checker Require Import rows.
From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import types.

Module Terms.

    Inductive term_classifier : Set :=
    | C_infer : term_classifier
    | C_check : term_classifier
    .

    Inductive pattern : Set :=
    | Pa_unit : pattern
    | Pa_variable : string -> pattern
    | Pa_constructor : string -> pattern -> pattern
    | Pa_record : Rows.t pattern -> pattern
    .

    Inductive matchers (T:Set): Set := 
    | Ma_single : pattern -> T -> matchers T
    | Ma_matchers : pattern -> T -> matchers T -> matchers T
    .

    Inductive t : forall (A:term_classifier), Set := 
    (* Basic terms ---------------------- *)
    | Te_variable {A} : string -> t A
    | Te_unit {A} : t A
    | Te_constructor : string -> t C_check
    (* Function and application --------- *)
    | Te_lambda : string -> t C_infer -> t C_infer
    | Te_typed_lambda {A} : string -> Types.t Types.C_poly -> t A -> t A
    | Te_apply {A} : t A -> t A -> t A
    (* Explicit type -------------------- *)
    | Te_ascription {A} : t C_infer -> Types.t Types.C_poly -> t A
    (* let binding corner --------------- *)
    | Te_ty_lambda : string -> Kinds.t -> t C_infer -> t C_infer
    | Te_let : string -> t C_infer -> t C_infer -> t C_infer
    | Te_let_type : string -> Kinds.t -> Types.t Types.C_poly -> t C_infer -> t C_infer
    | Te_let_rec {A} : string -> Types.t Types.C_poly -> string -> t A -> t A
    (* Pattern matching ----------------- *)
    | Te_match {A} : t C_check -> matchers ( t A) -> t A
    (* record corner -------------------- *)
    | Te_record {A} : Rows.t ( t C_check) -> t A
    | Te_access {A} : t C_check -> string -> t A
    | Te_record_with {A} : t C_check -> string -> t C_check -> t A
    .

End Terms.

(**
 * TODO:
 * - Add module 
 * - Define smart constructors
 * - Define catamorphisms and defaulted catamorphisms
 *)