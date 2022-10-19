
Module Kinds.

    Inductive t: Set := 
    | K_Type : t
    | K_Arrow : t -> t -> t
    .

    Definition kind := K_Type.
    Definition apply d c := K_Arrow d c.

End Kinds.

(**
 * TODO:
 * - Define catamorphisms and defaulted catamorphisms
 *)