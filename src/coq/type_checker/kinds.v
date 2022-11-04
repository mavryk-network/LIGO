
Module Kinds.

    Inductive t: Set := 
    | K_Kind : t
    | K_Arrow : t -> t -> t
    .

    Definition kind := K_Kind.
    Definition arrow d c := K_Arrow d c.

    Definition fold {B} (v:t) {kind} {arrow} : B :=
        match v with
        | K_Kind => kind tt
        | K_Arrow l r => arrow l r
        end.

    Definition Is_Kind (v:t) : Prop :=
        fold v
            (kind:=fun _ => True) 
            (arrow:=fun _ _ => False).

End Kinds.
