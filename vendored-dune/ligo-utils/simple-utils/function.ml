let constant x _ = x
let compose f g x = f (g x)
let ( <@ ) = compose
let compose_2 f g x y = f (g x y)
let compose_3 f g x y z = f (g x y z)
let compose_4 f g a b c d = f (g a b c d)
