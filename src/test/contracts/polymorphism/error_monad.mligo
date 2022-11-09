
type 'a result = Ok of 'a | Error of string
type 'a t = 'a result

let ret (type a) (x : a) : a result =
  Ok x

let fail (type a) (x : string) : a result =
  Error x

type 'a t = 'a result

let bind (type a b) (x : a result) (f : a -> b result) : b result =
  match x with
  | Error s -> Error s
  | Ok a -> f a

let run (type a) (m : a result) : a =
  match m with
  | Ok a -> a
  | Error s -> failwith s
