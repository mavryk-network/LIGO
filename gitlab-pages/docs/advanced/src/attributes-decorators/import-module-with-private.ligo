(* This is gitlab-pages/docs/advanced/src/attributes-decorators/import-module-with-private.ligo *)
#import "gitlab-pages/docs/advanced/src/attributes-decorators/module-with-private.ligo" "ModuleWithPrivate"

(* foo = 5167 = (123 * 42) + 1 *)
const foo : int = ModuleWithPrivate.f (123)

(*
  The following lines cause errors because g and stuff are private:

  const bad_1 = ModuleWithPrivate.g (123)
  const bad_2 = ModuleWithPrivate.stuff
*)