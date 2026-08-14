#import "gitlab-pages/docs/mavryk/decorators/src/private/module-with-private.ligo" "ModuleWithPrivate"

const foo : int = ModuleWithPrivate.f (123)  // = 5167

(*
  The following lines cause errors because g and stuff are private:

  const bad_1 = ModuleWithPrivate.g (123)
  const bad_2 = ModuleWithPrivate.stuff
*)