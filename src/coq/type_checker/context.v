Set Implicit Arguments.
From Coq Require Import String List.

From ligo_coq_type_checker Require Import kinds.
From ligo_coq_type_checker Require Import types.

Import ListNotations.

Module Context.

    Inductive assertion : Set := 
    | Co_type_variable : string -> Types.t Types.C_poly -> assertion
    | Co_kind_variable : string -> Kinds.t -> assertion
    | Co_kind_bound_variable : string -> Kinds.t -> Types.t Types.C_poly -> assertion
    | Co_exist_marker : string -> assertion
    | Co_kind_inferable : string -> Kinds.t -> assertion
    | Co_kind_bound_inferable : string -> Kinds.t -> Types.t Types.C_mono -> assertion
    .

    (* Naive representation for the moment *)
    Inductive t : Set := 
    | Co_empty : t
    | Co_assertion : t -> assertion -> t
    .

    Definition domain (a:assertion) : list string :=
        match a with
        | Co_type_variable n _ => [n]
        | Co_kind_variable n _ => [n]
        | Co_kind_bound_variable n _ _ => [n]
        | Co_exist_marker _ => []
        | Co_kind_inferable n _ => [n]
        | Co_kind_bound_inferable n _ _ => [n]
        end.

    Fixpoint domains (c:t) : list string :=
        match c with
        | Co_empty => []
        | Co_assertion c a => domains c ++ domain a
        end.

End Context.