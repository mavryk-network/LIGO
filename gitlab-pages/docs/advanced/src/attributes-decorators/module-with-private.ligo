(* This is gitlab-pages/docs/advanced/src/attributes-decorators/module-with-private.ligo *)
[@private] const stuff : int = 42
[@private] function g (const x : int) : int is x * stuff
function f (const x : int) : int is (g (x)) + 1