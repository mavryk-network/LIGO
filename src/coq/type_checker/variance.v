Module Variance.

    Inductive t : Set := 
    | Va_variant : t
    | Va_covariant : t
    | Va_invariant : t
    .

    Definition variant : t := Va_variant.
    Definition covariant : t := Va_covariant.
    Definition invariant : t := Va_invariant.

    Definition fold {B} (v:t) variant covariant invariant : B :=
        match v with
        | Va_variant => variant tt
        | Va_covariant => covariant tt
        | Va_invariant => invariant tt
        end.

End Variance.
