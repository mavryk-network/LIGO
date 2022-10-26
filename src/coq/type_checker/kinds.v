
Module Kinds.

    Inductive t: Set := 
    | K_Type : t
    | K_Arrow : t -> t -> t
    .

    Definition kind := K_Type.
    Definition apply d c := K_Arrow d c.

    Definition fold {B} (v:t) {type} {arrow} : B :=
        match v with
        | K_Type => type tt
        | K_Arrow l r => arrow l r
        end.

End Kinds.

(**
 * TODO:
 * - Define defaulted catamorphisms
 *)