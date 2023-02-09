let x = 2
let x = 1

let f (y : int) =
  y * x

let test =
  let v = Test.run_exn f 4 in
  Test.log v
