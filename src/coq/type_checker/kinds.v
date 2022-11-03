
Module Kinds.

    Inductive t: Set := 
    | K_Kind : t
    | K_Arrow : t -> t -> t
    .

    Definition kind := K_Kind.
    Definition apply d c := K_Arrow d c.

    Definition fold {B} (v:t) {kind} {arrow} : B :=
        match v with
        | K_Kind => kind tt
        | K_Arrow l r => arrow l r
        end.

    Definition isKind (v:t) : Prop :=
        fold v
            (kind:=fun _ => True) 
            (arrow:=fun _ _ => False).

End Kinds.

(**
 * TODO:
 * - Define defaulted catamorphisms
 *)