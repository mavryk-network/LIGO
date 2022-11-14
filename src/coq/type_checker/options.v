Module Options.

    Definition fold {A B : Type} (a: option A) {some: A ->  B} {none: unit -> B} : B :=
        match a with
            | Some x => some x
            | None => none tt
        end.

    Definition map {A B : Type} (f: A -> B) (a : option A) : option B := 
        fold a (some:=fun e => Some (f e)) (none:=fun _ => None).

    Definition join {A : Type} (a: option (option A)) : option A :=
        fold a (some:=fun t => t) (none:=fun _ => None).

    Definition bind {A B : Type} (a: option A) (f : A -> option B) : option B :=
        join (map f a).

    Definition Is_Some {A : Type} (a: option A) : Prop :=
        fold a (some:=fun _ => True) (none:=fun _ => False).

End Options.

(* Notation extensions corner *)

Notation "'let*' pat := ma 'in' mf" := (Options.bind ma (fun e => let pat := e in mf)) 
    (at level 61, pat pattern, mf at next level, right associativity).

Notation "'let+' pat := ma 'in' fa" := (Options.map (fun e => let pat := e in fa) ma) 
    (at level 61, pat pattern, fa at next level, right associativity).
