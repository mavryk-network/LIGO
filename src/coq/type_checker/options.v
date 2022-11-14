Module Options.

    Definition map {A B : Type} (f: A -> B) (a : option A) : option B := 
        @option_map A B f a.

    Definition bind {A B : Type} (a: option A) (f : A -> option B) : option B :=
        match a with
            | Some x => f x
            | None => None
        end.

    Definition fold {A B : Type} (a: option A) {some: A ->  B} {none: unit -> B} : B :=
        match a with
            | Some x => some x
            | None => none tt
        end.

    Definition Is_Some {A : Type} (a: option A) : Prop :=
        fold a (some:=fun _ => True) (none:=fun _ => False).

End Options.

(* Notation extensions corner *)

Notation "'let*' pat := ma 'in' fa" := (Options.bind ma (fun e => let pat := e in fa)) 
    (at level 61, pat pattern, fa at next level, right associativity).

Notation "'let+' pat := ma 'in' fa" := (Options.map (fun e => let pat := e in fa) ma) 
    (at level 61, pat pattern, fa at next level, right associativity).
