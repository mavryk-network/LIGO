
let @test =
  let () = Test.print "Hello " in
  let () = Test.println "world" in
  let () = Test.print (Option.unopt (Test.chr 64n)) in
  let () = Test.print (Test.to_string 42) in
  let () = Test.print Test.nl in
  Test.to_string (true, 42n)
