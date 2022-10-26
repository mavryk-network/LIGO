Set Implicit Arguments.
From Coq Require Import String List.

From ligo_coq_type_checker Require Import assertions.

Import ListNotations.

Module Context.

    Inductive t : Set := 
    | Co_empty : t
    | Co_assertion : t -> Assertions.t -> t
    .

    Fixpoint domains (c:t) : list string :=
        match c with
        | Co_empty => []
        | Co_assertion c a => domains c ++ Assertions.domains a
        end.

End Context.

(**
 * TODO:
 * - Define defaulted catamorphisms
 *)